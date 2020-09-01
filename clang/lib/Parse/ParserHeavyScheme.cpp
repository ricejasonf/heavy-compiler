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

#include "clang/AST/HeavyScheme.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
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

  bool fitsInBits(int NumDigits, BitWidth, Radix) {
    switch (Radix) {
    case 2:
      return NumDigits <= BitWidth;
    case 8:
      return NumDigits <= BitWidth / 3; // Digits are groups of 3 bits.
    case 10:
      return NumDigits <= floor(log10(pow(2, bit_width))); // floor(log10(2^64))
    case 16:
      return NumDigits <= BitWidth / 4; // Digits are groups of 4 bits.
    default:
      llvm_unreachable("impossible Radix");
    }
  }

  llvm::Optional<llvm::APInt>
  tryParseInteger(StringRef TokenSpan, int BitWidth, int Radix) {
    // largely inspired by NumericLiteralParser::GetIntegerValue
    llvm::APInt RadixVal(BitWidth, Radix);
    llvm::APInt DigitVal(BitWidth, 0);
    llvm::APInt Val(BitWidth, 0);
    llvm::APInt OldVal = Val;
    bool Negate = false;
    bool OverflowOccurred = false;

    // may start with sign
    if (TokenSpan[0] == '+' || TokenSpan[0] == '-') {
      Negate = true;
      TokenSpan = TokenSpan.drop_front(1);
    }
    for (char c : TokenSpan) {
      unsigned Digit = llvm::hexDigitValue(c);
      if (Digit >= Radix)
        return {};
      DigitVal = Digit;
      OldVal = Val;

      Val *= RadixVal;
      // The inverse operation should be the same or
      // it overflowed
      OverflowOccurred |= Val.udiv(RadixVal) != OldVal;
      Val += DigitVal;
      OverflowOccurred |= Val.ult(DigitVal);
    }
    if (OverflowOccurred)
      return {};
    if (Negate) 
      Val.negate();
    return Val;
  }
}

bool ParserHeavyScheme::Parse() {
  PP.InitHeavySchemeLexer();
  ConsumeToken();
  assert(Tok == tok::kw_heavy_begin && "Expected heavy_begin");

  heavy_scheme::Context Ctx(Actions.getASTContext());
  ValueResult Result;
  while (true) {
    Result = ParseExpr(/*IsTopLevel=*/true);
    if (Result.isUsable()) {
      write(llvm::errs(), eval(Ctx, Result.get()));
    }
    else {
      break;
    }
  };

  // Return control to C++ Parser
  PP.FinishHeavySchemeLexer();
  return Result.isInvalid()
}

ValueResult ParserHeavyScheme::ParseExpr(bool IsTopLevel = false) {
  ConsumeToken();
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
  case tok::kw_heavy_end:
    if (IsTopLevel) {
      return ValueEmpty();
    }
    LLVM_FALLTHROUGH;
  default:
    // TODO emit error unexpected token
    return ValueError();
  }
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

ValueResult ParserHeavyScheme::ParseCharConstant(){
  llvm_unreachable("TODO");
}

ValueResult ParserHeavyScheme::ParseNumber() {
  char const* Current = Tok.getLiteralData();
  char const* End = Current + (Tok.getLength() - 2);
  int BitWidth = Context.getASTContext()
                        .getTargetInfo()
                        .getIntSize();
  llvm::Optional<bool> IsExactOpt;
  llvm::Optional<int> RadixOpt;
  llvm::Optional<llvm::APInt> IntOpt;

  if (parseNumberPrefix(Current, IsExactOpt, RadixOpt)) {
    llvm_unreachable("TODO diagnose invalid number prefix");
    return ValueError();
  }

  bool IsExact = IsExactOpt.getValueOr(true);
  int Radix = Radix.getValueOr(10);

  StringRef TokenSpan(Current, End);
  IntOpt = tryParseInteger(TokenSpan, BitWidth, Radix);

  if (IsExact && IntOpt.hasValue()) {
    return Context.CreateInteger(IntOpt.getValue());
  }

  llvm::APFloat FloatVal();
  if (IntOpt.hasValue()) {
    llvm::APInt Int = IntOpt.getValue()
    FloatVal.convertFromAPInt(Int, Int.isSigned(),
                           llvm::APFloat::rmNearestTiesToEven);
  } else {
    auto Result = FloatVal.convertFromString(
        TokenSpan, llvm::APFloat::rmNearestTiesToEven);
    if (!Result) {
      llvm_unreachable("TODO invalid numerical syntax");
      return ValueError();
    }
  }
  return Context.CreateFloat(FloatVal);
}

ValueResult ParserHeavyScheme::ParseString() {
  // the literal must include the ""
  assert(Tok.getLength() > 2);
  char const* Current = Tok.getLiteralData() + 1;
  char const* End = Current + (Tok.getLength() - 2);
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

ValueResult ParserHeavyScheme::ParseVector(){
  llvm_unreachable("TODO");
  return ValueError();
}

ValueResult ParserHeavyScheme::ParseTypename(){
  llvm_unreachable("TODO");
  return ValueError();
}

ValueResult ParserHeavyScheme::ParseCppDecl(){
  llvm_unreachable("TODO");
  return ValueError();
}

