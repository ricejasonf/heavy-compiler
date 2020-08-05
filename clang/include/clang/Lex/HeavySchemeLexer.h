//===- HeavySchemeLexer.h - Heavy Scheme Lexer ------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Lexer interface for the Heavy Scheme DSL.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LEX_HEAVY_SCHEME_LEXER_H
#define LLVM_CLANG_LEX_HEAVY_SCHEME_LEXER_H

#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/TokenKinds.h"
#include "clang/Lex/PreprocessorLexer.h"
#include "clang/Lex/Token.h"
#include "clang/Lex/Lexer.h"
#include <cassert>
#include <cstdint>

namespace clang {

class DiagnosticBuilder;
class Preprocessor;
class SourceManager;

class HeavySchemeLexer {
  void Lex(Token& Tok);
};

} // namespace clang

#endif
