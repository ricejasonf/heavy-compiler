//===- HeavySchemeLexer.h - Heavy Scheme Lexer ------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Lexer interface for the HeavyScheme DSL.
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
  Preprocessor& PP;
  IdentifierInfo* Ident_heavy_begin = nullptr;
  IdentifierInfo* Ident_heavy_end = nullptr;
  SourceLocation FileLoc;
  const char* BufferStart = nullptr;
  const char* BufferEnd = nullptr;
  const char* BufferPtr = nullptr;

  HeavySchemeLexer(Preprocessor& PP);

public:
  void Init(SourceLocation Loc,
            const char* BS,
            const char* BE,
            const char* BP) {
    FileLoc = Loc;
    BufferStart = BS;
    BufferEnd = BE;
    BufferPtr = BP;
  }

  unsigned GetByteOffset() {
    if (BufferPtr > BufferEnd)
      return BufferEnd - BufferStart;
    else
      return BufferPtr - BufferStart;
  }
  
  void Lex(Token& Tok);
private:
  void LexIdentifier(Token& Tok, const char *CurPtr);
  void LexNumberOrIdentifier(Token& Tok, const char *CurPtr);
  void LexNumberOrEllipsis(Token& Tok, const char *CurPtr);
  void LexNumber(Token& Tok, const char *CurPtr);
  void LexSharpLiteral(Token& Tok, const char *CurPtr);
  void LexStringLiteral(Token& Tok, const char *CurPtr);
  void LexUnknown(Token& Tok, const char *CurPtr);
  void SkipUntilDelimiter(const char *&CurPtr);
  void ProcessWhitespace(Token& Tok, const char *&CurPtr);

  // Advances the Ptr and returns the char
  char ConsumeChar(const char *&Ptr) {
    return *(++Ptr);
  }

  void FormRawIdentifier(Token &Result, const char *TokEnd) {
    const char* StartPtr = BufferPtr;
    FormTokenWithChars(Result, TokEnd, tok::raw_identifier);
    Result.setRawIdentifierData(StartPtr);
  }

  void FormLiteral(Token &Result, const char *TokEnd,
                   tok::TokenKind Kind) {
    const char* StartPtr = BufferPtr;
    FormTokenWithChars(Result, TokEnd, Kind);
    Result.setLiteralData(StartPtr);
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
