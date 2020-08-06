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

#include "clang/Lex/Token.h"
#include "clang/Lex/Lexer.h"
#include <cassert>
#include <cstdint>

namespace clang {

class DiagnosticBuilder;
class Preprocessor;
class SourceManager;

class HeavySchemeLexer {
  friend class Preprocessor;
  const char* BufferStart;
  const char* BufferEnd;
  const char* BufferPtr;
  // CouldBePPDirective means that we are at the top level and
  // the start of a line (not including whitespace)
  bool IsAtStartOfLine;

public:
  void Init(const char* BS,
            const char* BE,
            const char* BP) {
    BufferStart = BS;
    BufferEnd = BE;
    BufferPtr = BP;
  }

  unsigned GetByteOffset() {
    return BufferPtr - BufferStart;
  }
  
  void Lex(Token& Tok);
private:
  void LexIdentifer(Token& Tok);
  void LexLiteral(Token& Tok);
  void LexStringLiteral(Token& Tok);
  void ProcessWhitespace(Token& Tok);

  char getAndAdvanceChar(const char *&Ptr) {
    return *Ptr++;
  }

  // Copy/Pasted from Lexer
  void FormTokenWithChars(Token &Result, const char *TokEnd,
                          tok::TokenKind Kind) {
    unsigned TokLen = TokEnd-BufferPtr;
    Result.setLength(TokLen);
    Result.setLocation(getSourceLocation(BufferPtr, TokLen));
    Result.setKind(Kind);
    BufferPtr = TokEnd;
  }
  SourceLocation getSourceLocation(const char *Loc, unsigned TokLen) const;
};

} // namespace clang

#endif
