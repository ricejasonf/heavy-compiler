//===--- ParserHeavyScheme.h - HeavyScheme Language Parser ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Parser interface for HeavyScheme.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_PARSE_PARSER_HEAVY_SCHEME_H
#define LLVM_CLANG_PARSE_PARSER_HEAVY_SCHEME_H

#include "clang/AST/HeavyScheme.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/Parser.h"
#include <string>

namespace clang {

class Parser;

class ParserHeavyScheme {
  using ValueResult = heavy_scheme::ValueResult;
  Preprocessor& PP;
  Parser& CxxParser;
  Token Tok = {};
  SourceLocation PrevTokLocation;
  std::string LiteralResult = {};

  SourceLocation ConsumeToken() {
    PrevTokLocation = Tok.getLocation();
    PP.LexHeavyScheme(Tok);
    return PrevTokLocation;
  }

  bool TryConsumeToken(tok::TokenKind Expected) {
    if (Tok.isNot(Expected))
      return false;
    ConsumeToken();
    return true;
  }

  ValueResult ParseTopLevelExpr();
  ValueResult ParseExpr();

  ValueResult ParseCharConstant();
  ValueResult ParseCppDecl();
  ValueResult ParseList();
  ValueResult ParseListStart();
  ValueResult ParseNumber();
  ValueResult ParseString();
  ValueResult ParseSymbol();
  ValueResult ParseTypename();
  ValueResult ParseVector();

  ValueResult ParseDottedCdr();
  ValueResult ParseSpecialEscapeSequence();

  heavy_scheme::Context getContext() {
    return heavy_scheme::Context(CxxParser);
  }

public:
  ParserHeavyScheme(Preprocessor& PP, Parser& P)
    : PP(PP)
    , CxxParser(P)
  { }

  // Parses until the terminator token (ie heavy_end)
  // and evaluates top level expressions.
  // Expects that the first token is heavy_begin
  // Returns true if there was an error
  bool Parse();
};

}  // end namespace clang

#endif
