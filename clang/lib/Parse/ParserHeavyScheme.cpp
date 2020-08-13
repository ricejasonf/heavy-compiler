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

bool Finish(TokenKind Kind) {
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
