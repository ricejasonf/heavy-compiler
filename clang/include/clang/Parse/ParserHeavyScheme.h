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

#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"

namespace clang {
  class ParserHeavyScheme {
    Preprocessor& PP;
    Sema& Actions;
    DiagnosticsEngine& Diags;
    Token Tok = {};

    // TODO make a HeavySchemeEnvironment stack

  public:
    ParserHeavyScheme(Preprocessor& P, Sema& S, DiagnosticsEngine& D)
      : PP(P)
      , Actions(S)
      , Diags(D)
    { }

    // Parses until the terminator token (ie heavy_end).
    // Expects the first token is the initiating token (ie heavy_begin)
    // Returns true if there was an error
    // Evaluation and everything will likely occur inside this function
    bool Parse();
  };
}  // end namespace clang

#endif