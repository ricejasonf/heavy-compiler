//===--- ParserHeavyScheme.cpp - HeavyScheme Language Parser --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Parser for HeavyScheme.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/Scope.h"
#include "llvm/Support/Path.h"
using namespace clang;

using heavy_scheme::ValueResult;
using heavy_scheme::ValueError;
using heavy_scheme::ValueEmpty;
using heavy_scheme::Context;

namespace {
  // Returns true on an invalid number prefix notation
  bool parseNumberPrefix(const char*& CurPtr,
                         Optional<bool>& IsExact,
                         Optional<int>& Radix) {
    if (*CurPtr != '#')
      return false;

    ++CurPtr;
    char c = *CurPtr;
    if (Radix.hasValue() ||
        (IsExact.hasValue() &&
         (c == 'e' || c == 'i'))) {
      return true;
    }

    switch (c) {
    case 'e':
      IsExact = true;
      break;
    case 'i':
      IsExact = false;
      break;
    case 'b':
      Radix = 2;
      break;
    case 'o':
      Radix = 8;
      break;
    case 'd':
      Radix = 10;
      break;
    case 'h':
      Radix = 16;
      break;
    default:
      return true;
    }
    ++CurPtr;
    return parseNumberPrefix(CurPtr, IsExact, Radix, Result);
  }
}

bool ParserHeavyScheme::Parse() {
  assert(Tok == tok::kw_heavy_begin);
  PP.InitHeavySchemeLexer();

  while (true) {
    PP.LexHeavyScheme(Tok);
    switch (Tok.getKind()) {
    case tok::unknown:
    case tok::eof:
    case tok::kw_heavy_end:
      return Finish(Tok.getKind());
    default:
      // uhhh create ast node I guess?
    }
  }

  // Return control to Parser
  return Finish();
}

bool Parser::Finish(TokenKind Kind) {
  // Update Preprocessor with
  // the current file position
  PP.FinishHeavySchemeLexer();

  if (Kind != tok::kw_heavy_end) {
    // TODO emit diagnostic for unexpected token
    return true;
  }

  // TODO check that parens were closed properly
  bool Result = false;
  return Result;
}

ValueResult ParserHeavyScheme::ParseExpr() {
  switch (Tok.getKind()) {
  case tok::l_paren:
    return ParseListStart();
  case tok::numeric_constant:
    return ParseNumber();
  case tok::kw_typename:
  case tok::kw_constexpr:
  case tok::raw_identifier:
    return ParseSymbol();
  case tok::char_constant:
    return ParseCharConstant();
  case tok::heavy_true:
    return Context::CreateBoolean(true);
  case tok::heavy_false:
    return Context::CreateBoolean(false);
  case tok::string_literal:
    return ParseString();
}

ValueResult ParserHeavyScheme::ParseListStart() {
  // Consume the l_paren
  ConsumeToken();

  if (!Tok.isOneOf(tok::kw_typename,
                   tok::kw_constexr)) {
    return ParseList();
  }

  llvm_unreachable("TODO Implement C++ parser escape sequences")

#if 0
  // Parse the keyword as a symbol
  ValueResult Car = ParseSymbol();

  PrepareToLexCXX();
  //
  // handle special escape sequences
  if (Tok.is(tok::kw_typename)) {
    TypeResult TR = Parser.ParseTypeName();
  } else if (tok::kw_constexpr) {
    ExprResult = Parser.ParseExpr();
    // uhh we expect the expr to be the name of a declaration
  }
    if (!TryConsumeToken(tok::r_paren)) {
      // TODO emit error of illegal notation
    }
  }
#endif
}

ValueResult ParserHeavyScheme::ParseList() {
  if (Tok.is(tok::r_paren)) {
    return Context::CreateEmpty();
  }

  ValueResult Car = ParseExpr();
  ValueResult Cdr;
  if (Tok.is(tok::period)) {
    Cdr = ParseDottedCdr();
  } else {
    Cdr = ParseList();
  }

  if (Car->isInvalid() ||
      Cdr->IsInvalid()) {
    return ValueError();
  }

  return Context::CreatePair(Car->getResult(),
                             Cdr->getResult());
}

// We have a dot while parsing a list,
// so we expect a single expression
// then the closing r_paren
ValueResult ParserHeavyScheme::ParseDottedCdr() {
  assert(Tok.is(tok::period));
  ConsumeToken();
  ValueResult Cdr = ParseExpr();
  if (!TryConsumeToken(tok::r_paren)) {
    llvm_unreachable(
        "TODO emit a diagnostic about illegal dot notation");
    return ValueError();
  }
}

ValueResult ParserHeavyScheme::ParseBoolean() {
  llvm_unreachable("TODO");
}

ValueResult ParserHeavyScheme::ParseCharConstant(){
  llvm_unreachable("TODO");
}

ValueResult ParserHeavyScheme::ParseCppDecl(){
  llvm_unreachable("TODO");
}

ValueResult ParserHeavyScheme::ParseNumber() {
  char const* Current = Tok.getLiteralData();
  char const* End = Current + (Tok.getLength() - 2);
  int BitWidth = ????Ctx.getTargetInfo().getIntSize();
  llvm::Optional<bool> IsExactOpt;
  llvm::Optional<int> RadixOpt;
  llvm::Optional<llvm::APInt> IntOpt;

  if (parseNumberPrefix(Current, IsExactOpt, RadixOpt)) {
    llvm_unreachable("TODO diagnose invalid number prefix");
  }

  bool IsExact = IsExactOpt.getValueOr(true);
  int Radix = Radix.getValueOr(10);

  StringRef TokenSpan(Current, End);
  if (IsValidInteger(TokenSpan, Radix)) {
    IntOpt = llvm::APInt(BitWidth, TokenSpan, Radix);
  }
  if (IsExact && IntOpt.hasValue()) {
    return Context.CreateInteger(IntOpt.getValue());
  }

  llvm::APFloat Float();
  if (IntOpt.hasValue()) {
    llvm::APInt Int = IntOpt.getValue()
    Float.convertFromAPInt(Int, Int.isSigned(),
                           llvm::APFloat::rmNearestTiesToEven);
  } else {
    auto Result = Float.convertFromString(
        TokenSpan, llvm::APFloat::rmNearestTiesToEven);
    if (!Result) {
      llvm_unreachable("TODO invalid numerical syntax");
    }
  }
  return Context.CreateFloat(Float
}

ValueResult ParserHeavyScheme::ParseString(){
  // the literal must include the ""
  assert(Tok.getLength() > 2);
  char const* Current = Tok.getLiteralData() + 1;
  char const* End = Current + (Tok.getLength() - 2);
  StringRef TokenSpan(Tok.getLiteralData() + 1, Tok.getLength() - 2);
  LiteralResult.clear();
  while (Current < End) {
    StringRef TokenSpan(Current, End);
    char c = TokenSpan[0];
    // R5RS specifically forbids a backslash in a string constant
    // without being escaped by another backslash, but it then
    // explicitly leaves what backslashes do in other cases
    // unspecified.
    // We allow standalone backslashes so they can be proxied
    // through to the host language. Otherwise users would get
    // stuck escaping everything twice making things confusing.
    if (c == '\\') {
      if (TokenSpan.startswith("\\\"")) {
        // escaped double quote
        c = '"';
      }
      else if (TokenSpan.startswith("\\\\")) {
        // escaped backslash
        c = '\\';
      }
    }
    else if (TokenSpan.startswith("\r\n")) {
      // normalize source-file newlines
      c = '\n'
    }
    LiteralResult.append(c);
    ++Current;
  }
  return Context.CreateString(StringRef(LiteralResult));
}

ValueResult ParserHeavyScheme::ParseTypename(){
  llvm_unreachable("TODO");
}

ValueResult ParserHeavyScheme::ParseVector(){
  llvm_unreachable("TODO");
}

