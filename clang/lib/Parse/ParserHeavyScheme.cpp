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
#include "clang/Parse/ParserHeavyScheme.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/Scope.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
using namespace clang;

using heavy::ValueResult;
using heavy::ValueError;
using heavy::ValueEmpty;
using heavy::Context;

namespace {
  // Returns true on an invalid number prefix notation
  bool parseNumberPrefix(const char*& CurPtr,
                         Optional<bool>& IsExact,
                         Optional<unsigned>& Radix) {
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
    return parseNumberPrefix(CurPtr, IsExact, Radix);
  }

  llvm::Optional<llvm::APInt>
  tryParseInteger(StringRef TokenSpan, unsigned BitWidth, unsigned Radix) {
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
  if (!TryConsumeToken(tok::l_brace)) {
    CxxParser.Diag(Tok, diag::err_expected) << tok::l_brace;
    return true;
  }

  heavy::Context& Context = getContext();
  ValueResult Result;
  while (true) {
    Result = ParseTopLevelExpr();
    ValueResult EvalResult(false);
    if (Result.isUsable()) {
      EvalResult = eval(Context, Result.get());
    }
    if (EvalResult.isUsable()) {
      write(llvm::errs(), EvalResult.get());
      llvm::errs() << '\n';
    }
    else {
      break;
    }
  };

  // Return control to C++ Lexer
  PP.FinishHeavySchemeLexer();
  return Result.isInvalid();
}

ValueResult ParserHeavyScheme::ParseTopLevelExpr() {
  if (Tok.is(tok::r_brace)) {
    // The end of 
    //    heavy_scheme { ... }
    return ValueEmpty();
  }
  return ParseExpr();
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
    return getContext().CreateBoolean(true);
  case tok::heavy_false:
    return getContext().CreateBoolean(false);
  case tok::string_literal:
    return ParseString();
  default:
    // TODO emit error unexpected token
    return ValueError();
  }
}

ValueResult ParserHeavyScheme::ParseListStart() {
  // Consume the l_paren
  assert(Tok.is(tok::l_paren));
  ConsumeToken();
  return ParseList();
}

ValueResult ParserHeavyScheme::ParseList() {
  if (Tok.is(tok::r_paren)) {
    ConsumeToken();
    return getContext().CreateEmpty();
  }

  ValueResult Car = ParseExpr();
  if (!Car.isUsable()) {
    return ValueError();
  }

  ValueResult Cdr;
  if (Tok.is(tok::period)) {
    Cdr = ParseDottedCdr();
  } else {
    Cdr = ParseList();
  }

  if (!Cdr.isUsable()) {
    return ValueError();
  }

  return getContext().CreatePair(Car.get(),
                                 Cdr.get());
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
  return Cdr;
}

ValueResult ParserHeavyScheme::ParseCharConstant(){
  llvm_unreachable("TODO");
}

ValueResult ParserHeavyScheme::ParseNumber() {
  char const* Current = Tok.getLiteralData();
  char const* End = Current + Tok.getLength();
  int BitWidth = getContext().GetIntWidth();
  llvm::Optional<bool> IsExactOpt;
  llvm::Optional<unsigned> RadixOpt;
  llvm::Optional<llvm::APInt> IntOpt;

  if (parseNumberPrefix(Current, IsExactOpt, RadixOpt)) {
    llvm_unreachable("TODO diagnose invalid number prefix");
    return ValueError();
  }

  StringRef TokenSpan(Current, End - Current);
  bool IsExact = IsExactOpt.getValueOr(true);
  unsigned Radix = RadixOpt.getValueOr(10);

  IntOpt = tryParseInteger(TokenSpan, BitWidth, Radix);

  ConsumeToken();

  if (IsExact && IntOpt.hasValue()) {
    return getContext().CreateInteger(IntOpt.getValue());
  }

  llvm::APFloat FloatVal(0.0f);
  if (IntOpt.hasValue()) {
    llvm::APInt Int = IntOpt.getValue();
    FloatVal.convertFromAPInt(Int, /*isSigned=*/true,
                           llvm::APFloat::rmNearestTiesToEven);
  } else {
    auto Result = FloatVal.convertFromString(
        TokenSpan, llvm::APFloat::rmNearestTiesToEven);
    if (!Result) {
      llvm_unreachable("TODO invalid numerical syntax");
      return ValueError();
    }
  }
  return getContext().CreateFloat(FloatVal);
}

ValueResult ParserHeavyScheme::ParseString() {
  // the literal must include the ""
  assert(Tok.getLength() > 2);
  char const* Current = Tok.getLiteralData() + 1;
  char const* End = Current + (Tok.getLength() - 2);
  LiteralResult.clear();
  while (Current < End) {
    StringRef TokenSpan(Current, End - Current);
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
      c = '\n';
    }
    LiteralResult.push_back(c);
    ++Current;
  }
  ConsumeToken();
  return getContext().CreateString(StringRef(LiteralResult));
}

ValueResult ParserHeavyScheme::ParseSymbol(){
  StringRef Str = Tok.getRawIdentifier();
  ConsumeToken();
  return getContext().CreateSymbol(Str);
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

