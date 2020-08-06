//===- HeavySchemeLexer.cpp - HeavyScheme Language Lexer ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file implements the HeavySchemeLexer
//
//===----------------------------------------------------------------------===//

#include "clang/Lex/Lexer.h"
#include "UnicodeCharSets.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TokenKinds.h"
#include "clang/Lex/LexDiagnostic.h"
#include "clang/Lex/LiteralSupport.h"
#include "clang/Lex/MultipleIncludeOpt.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Lex/Token.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/TokenKinds.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/NativeFormatting.h"
#include "llvm/Support/UnicodeCharRanges.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <string>
#include <tuple>
#include <utility>

using namespace clang;

void HeavySchemeLexer::Lex(Token& Tok) {
  ProcessWhitespace(Tok);

  const char* CurPtr = BufferPtr;
  tok::TokenKind Kind;
  char c = getAndAdvanceChar(CurPtr);
  switch(c) {
  // Identifiers.
  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
  case 'H': case 'I': case 'J': case 'K': case 'L'  case 'M': case 'N':
  case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
  case 'V': case 'W': case 'X': case 'Y': case 'Z':
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
  case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
  case 'v': case 'w': case 'x': case 'y': case 'z':
  // Identifiers (extended alphabet)
  case '+': case '-': case '.': case '*': case '/': case '<': case '=':
  case '>': case '!': case '?': case ':': case '$': case '%': case '_':
  case '&': case '~': case '^':
    return LexIdentifer(Tok);
  // Integer constants
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    return LexIntegerConstant(Tok);
  case '#':
    return LexLiteral(Tok);
  case '"':
    return LexStringLiteral(Tok);
  case '(':
    Kind = tok::l_paren;
    break;
  case ')':
    Kind = tok::r_paren;
    break;
  default:
    Kind = tok::unknown;
    break;
  }

  // Handle the single character tokens
  FormTokenWithChars(Tok, CurPtr, Kind);
}

void HeavySchemeLexer::LexIdentifer(Token& Tok, const char *CurPtr) {
  const char* CurPtr = BufferPtr;
  tok::TokenKind Kind;
  char c = getAndAdvanceChar(CurPtr);
  // TODO stuff
}

void HeavySchemeLexer::LexIntegerConstant(Token& Tok, const char *CurPtr) {
  const char* CurPtr = BufferPtr;
  tok::TokenKind Kind;
  char c = getAndAdvanceChar(CurPtr);
  // TODO stuff
}

void HeavySchemeLexer::LexLiteral(Token& Tok, const char *CurPtr) {
  const char* CurPtr = BufferPtr;
  tok::TokenKind Kind;
  char c = getAndAdvanceChar(CurPtr);
  // TODO stuff
}

void HeavySchemeLexer::LexStringLiteral(Token& Tok, const char *CurPtr) {
  const char* CurPtr = BufferPtr;
  tok::TokenKind Kind;
  char c = getAndAdvanceChar(CurPtr);
  // TODO stuff
}

void HeavySchemeLexer::ProcessWhitespace(Token& Tok) {
  // adds whitespace flags to Tok if needed
  const char* CurPtr = BufferPtr;
  char c = *CurPtr;
  while (true) {
    while (isHorizontalWhitespace(c)) {
      c = getAndAdvanceChar(CurPtr);
    }

    if (!isVerticalWhitespace(c)) {
      break;
    }

    Token.setFlag(Token::StartOfLine);
    Token.clearFlag(Token::LeadingSpace);
    c = getAndAdvanceChar(CurPtr);
  }

  if (CurPtr != BufferPtr) {
    Token.setFlag(Token::LeadingSpace);
  }

  // Update the buffer location to the first non-whitespace character.
  BufferPtr = CurPtr;
}

// Copy/Pasted from Lexer
SourceLocation HeavySchemeLexer::getSourceLocation(const char *Loc,
                                                   unsigned TokLen) const {
  assert(Loc >= BufferStart && Loc <= BufferEnd &&
         "Location out of range for this buffer!");

  // In the normal case, we're just lexing from a simple file buffer, return
  // the file id from FileLoc with the offset specified.
  unsigned CharNo = Loc-BufferStart;
  if (FileLoc.isFileID())
    return FileLoc.getLocWithOffset(CharNo);

  // Otherwise, this is the _Pragma lexer case, which pretends that all of the
  // tokens are lexed from where the _Pragma was defined.
  assert(PP && "This doesn't work on raw lexers");
  return GetMappedTokenLoc(*PP, FileLoc, CharNo, TokLen);
}
