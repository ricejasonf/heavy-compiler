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

namespace {
  bool isExtendedAlphabet(char c) {
    // TODO
    // We could make a table similar to clang::char_info::InfoTable
    // for more efficient processing here and possibly elsewhere.
    switch(c) {
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
    case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
    case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
    case 'V': case 'W': case 'X': case 'Y': case 'Z':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z':
    case '+': case '-': case '.': case '*': case '/': case '<': case '=':
    case '>': case '!': case '?': case ':': case '$': case '%': case '_':
    case '&': case '~': case '^':
      return true;
    }
    return false;
  }

  bool isDelimiter(char c) {
    // TODO
    // We could make a table similar to clang::char_info::InfoTable
    // for more efficient processing here and possibly elsewhere.
    switch(c) {
    case 0:
    case ',': case '\'': case '"': case '`': case ';':
    case '(': case ')':
    case '[': case ']':
    case '{': case '}':
      return true;
    }

    if (isWhitespace(c)) {
      return true;
    }

    return false;
  }
}

void HeavySchemeLexer::Lex(Token& Tok) {
  const char* CurPtr = BufferPtr;
  tok::TokenKind Kind;
  ProcessWhitespace(Tok, CurPtr);

  // Act on the current character
  char c = *CurPtr;
  // There are all considered "initial characters".
  switch(c) {
  // Identifiers.
  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
  case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
  case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
  case 'V': case 'W': case 'X': case 'Y': case 'Z':
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
  case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
  case 'v': case 'w': case 'x': case 'y': case 'z':
  // Identifiers (extended alphabet)
  case '*': case '/': case '<': case '=': case '>': case '!':
  case '?': case ':': case '$': case '%': case '_':
  case '&': case '~': case '^':
    return LexIdentifier(Tok, CurPtr);
  // Integer constants
  case '-': case '+':
    return LexNumberOrIdentifier(Tok, CurPtr);
  case '.':
    return LexNumberOrEllipsis(Tok, CurPtr);
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    return LexNumber(Tok, CurPtr);
  case '#':
    return LexSharp(Tok, CurPtr);
  case '"':
    return LexStringLiteral(Tok, CurPtr);
  case '(':
    Kind = tok::l_paren;
    break;
  case ')':
    Kind = tok::r_paren;
    break;
  case ',':
    // TODO handle quasiquotation token '@,'
    Kind = tok::comma;
    break;
  case '`':
    Kind = tok::heavy_grave;
    break;
  default:
    Kind = tok::unknown;
    break;
  }

  // Handle the single character tokens
  FormTokenWithChars(Tok, CurPtr, Kind);
}

void HeavySchemeLexer::LexIdentifier(Token& Tok, const char *CurPtr) {
  char c;
  do {
    c = getAndAdvanceChar(CurPtr);
  } while (isExtendedAlphabet(c));

  if (!isDelimiter(c)) {
    return LexUnknown(Tok, CurPtr);
  }

  // Check for heavy_begin and heavy_end which are the only
  // non-bindable syntactic keywords as they are really an
  // extension to the host C++ syntax
  StringRef IdStr(BufferPtr, CurPtr - BufferPtr);
  IdentifierInfo* II = PP.getIdentifierInfo(IdStr);
  if (II == Ident_heavy_begin) {
    return FormTokenWithChars(Tok, CurPtr, tok::kw_heavy_begin)
  } else if (II == Ident_heavy_end) {
    return FormTokenWithChars(Tok, CurPtr, tok::kw_heavy_end)
  } else {
    return FormRawIdentifier(Tok, CurPtr);
  }
}

void HeavySchemeLexer::LexNumberOrIdentifier(Token& Tok, const char *CurPtr) {
  const char *OrigPtr = CurPtr;
  char c = getAndAdvanceChar(CurPtr);
  if (isDelimiter(c)) {
    // '+' | '-' are valid identifiers
    return FormRawIdentifier(Tok, CurPtr);
  }
  // Lex as a number
  LexNumber(Tok, OrigPtr);
}

void HeavySchemeLexer::LexNumberOrEllipsis(Token& Tok, const char *CurPtr) {
  const char *OrigPtr = CurPtr;
  char c1 = getAndAdvanceChar(CurPtr);
  char c2 = getAndAdvanceChar(CurPtr);
  char c3 = getAndAdvanceChar(CurPtr);
  if (c1 == '.' && c2 ==  '.' && isDelimiter(c3)) {
    // '...' is a valid identifier
    return FormRawIdentifier(Tok, CurPtr);
  }
  // Lex as a number
  LexNumber(Tok, OrigPtr);
}

void HeavySchemeLexer::LexNumber(Token& Tok, const char *CurPtr) {
  while (true) {
    char c = getAndAdvanceChar(CurPtr);
    if (char_info::isDigit(c) || c == '.')
      continue;
    if (!isDelimiter(c))
      LexUnknown(Tok, CurPtr);
    break;
  }
  FormTokenWithChars(Tok, CurPtr, tok::numberic_constant);
}

// These could be numbers, character constants, or other literals
// such as #t #f for true and false
void HeavySchemeLexer::LexSharpLiteral(Token& Tok, const char *CurPtr) {
  char c = getAndAdvanceChar(CurPtr);
  // If we expect the token to end after
  // `c` then we set RequiresDelimiter
  bool RequiresDelimiter = false;
  switch (c) {
  case '\\':
    SkipUntilDelimiter(CurPtr);
    return FormTokenWithChars(Tok, CurPtr, tok::char_constant);
  case 't':
    Kind = tok::heavy_true;
    RequiresDelimiter = true;
    break;
  case 'f':
    Kind = tok::heavy_false;
    RequiresDelimiter = true;
    break;
  case '(':
    Kind = tok::heavy_vector_lparen;
    break;
  // unsupported radix R specifiers
  case 'd':
  case 'b': case 'o': case 'x':
  // unsupported exactness specifiers
  case 'i': case 'e':
  default:
    Kind = tok::unknown;
    SkipUntilDelimiter(CurPtr);
  }

  // We should be at a delimiter at this point or
  // we are dealing with something invalid
  if (RequiresDelimiter && !isDelimiter(*CurPtr)) {
    SkipUntilDelimiter(CurPtr);
    FormTokenWithChars(Tok, CurPtr, tok_unknown);
  } else {
    FormTokenWithChars(Tok, CurPtr, Kind);
  }
}

void HeavySchemeLexer::LexStringLiteral(Token& Tok, const char *CurPtr) {
  while (true) {
    char c = getAndAdvanceChar(CurPtr);
    if (c == '"')
      break;
    if (c == '\\') {
      // As an extension to R5RS we just consume any
      // character following a backslash so we can pass
      // through a reasonable subset of valid C++ string literals.
      getAndAdvanceChar(CurPtr);
    }
  }
  FormTokenWithChars(Tok, CurPtr, tok::string_literal);
}

void HeavySchemeLexer::LexCharacterLiteral(Token& Tok, const char *CurPtr) {
  SkipUntilDelimiter(CurPtr);
  FormTokenWithChars(Tok, CurPtr, tok::char_constant);
}

void HeavySchemeLexer::LexUnknown(Token& Tok, const char *CurPtr) {
  SkipUntilDelimiter(CurPtr);
  FormTokenWithChars(Tok, CurPtr, tok::unknown);
}

vois HeavySchemeLexer::SkipUntilDelimiter(const char *CurPtr) {
  while (!isDelimiter(getAndAdvanceChar(CurPtr))) { }
}

void HeavySchemeLexer::ProcessWhitespace(Token& Tok, const char *&CurPtr) {
  // adds whitespace flags to Tok if needed
  char c = *CurPtr;
  char PrevChar;

  if (!isWhitespace(c)) {
    return;
  }

  while (true) {
    PrevChar = c;
    while (isHorizontalWhitespace(c)) {
      c = getAndAdvanceChar(CurPtr);
    }

    if (!isVerticalWhitespace(c)) {
      break;
    }

    c = getAndAdvanceChar(CurPtr);
  }

  if (isHorizontalWhitespace(PrevChar)) {
    Token.setFlag(Token::LeadingSpace);
  } else if (isVerticalWhitespace(PrevChar)) {
    Token.setFlag(Token::StartOfLine);
  }
}

// Copy/Pasted from Lexer (mostly)
SourceLocation HeavySchemeLexer::getSourceLocation(const char *Loc,
                                                   unsigned TokLen) const {
  assert(Loc >= BufferStart && Loc <= BufferEnd &&
         "Location out of range for this buffer!");

  // In the normal case, we're just lexing from a simple file buffer, return
  // the file id from FileLoc with the offset specified.
  unsigned CharNo = Loc-BufferStart;
  return FileLoc.getLocWithOffset(CharNo);
r
