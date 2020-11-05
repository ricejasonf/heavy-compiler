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

#include "clang/AST/DeclBase.h" // for DeclContext
#include "clang/AST/HeavyScheme.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/ParserHeavyScheme.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/Scope.h"
#include "llvm/ADT/SmallVector.h"
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
    llvm::APInt RadixVal(BitWidth, Radix, /*IsSigned=*/true);
    llvm::APInt DigitVal(BitWidth, 0, /*IsSigned=*/true);
    llvm::APInt Val(BitWidth, 0, /*IsSigned=*/true);
    llvm::APInt OldVal = Val;
    bool Negate = false;
    bool OverflowOccurred = false;

    // may start with sign
    if (TokenSpan[0] == '+' || TokenSpan[0] == '-') {
      Negate = TokenSpan[0] == '-';
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

// FIXME Breakout this bootstrapping code
//       to make the real parsing stand alone
bool ParserHeavyScheme::Parse() {
  PP.InitHeavySchemeLexer();

  ConsumeToken();
  if (!TryConsumeToken(tok::l_brace)) {
    CxxParser.Diag(Tok, diag::err_expected) << tok::l_brace;
    return true;
  }

  // Load the environment for the current DeclContext
  DeclContext* DC = CxxParser.getActions().CurContext;
  Context.EnvStack = LoadEmbeddedEnv(DC);

  ValueResult Result;
  bool HasError = false;
  while (true) {
    Result = ParseTopLevelExpr();
    // Keep parsing until we find the end
    // brace (represented by ValueEmpty here)
    if (Result.isUnset()) break;
    if (Result.isInvalid()) {
      ConsumeToken();
      HasError = true;
    }
    if (HasError) continue;

    Value* Val = eval(Context, Result.get());

    if (Context.CheckError()) {
      HasError = true;
      CxxParser.Diag(Context.getErrorLocation(), diag::err_heavy_scheme)
        << Context.getErrorMessage();
    } else {
      // TEMP dump the result
      write(llvm::errs(), Val);
      llvm::errs() << '\n';
    }
  };

  // Return control to C++ Lexer
  PP.FinishHeavySchemeLexer();
  return Result.isInvalid();
}

// FIXME Breakout out along with the "Parse" function
heavy::Value*
ParserHeavyScheme::LoadEmbeddedEnv(DeclContext* DC) {
  auto itr = EmbeddedEnvs.find(DC);
  if (itr != EmbeddedEnvs.end()) return itr->second;
  Value* Env;
  if (DC->isTranslationUnit()) {
    Env = Context.SystemEnvironment;
  } else {
    Env = LoadEmbeddedEnv(DC->getParent());
  }
  Env = Context.CreatePair(Context.CreateModule(), Env);
  EmbeddedEnvs[DC] = Env;
  return Env;
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
  case tok::heavy_vector_lparen:
    return ParseVectorStart();
  case tok::numeric_constant:
    return ParseNumber();
  case tok::kw_typename:
  case tok::kw_constexpr:
  case tok::raw_identifier:
    return ParseSymbol();
  case tok::char_constant:
    return ParseCharConstant();
  case tok::heavy_true:
    return Context.CreateBoolean(true);
  case tok::heavy_false:
    return Context.CreateBoolean(false);
  case tok::string_literal:
    return ParseString();
  case tok::heavy_quote:
    return ParseExprAbbrev("quote");
  case tok::heavy_quasiquote:
    return ParseExprAbbrev("quasiquote");
  case tok::heavy_unquote:
    return ParseExprAbbrev("unquote");
  case tok::heavy_unquote_splicing:
    return ParseExprAbbrev("unquote-splicing");
  case tok::r_paren: {
    CxxParser.Diag(Tok, diag::err_heavy_scheme)
      << "extraneous closing paren (')')";
    return ValueError();
  }
  case tok::r_square: {
    CxxParser.Diag(Tok, diag::err_heavy_scheme)
      << "extraneous closing bracket (']')";
    return ValueError();
  }
  case tok::r_brace: {
    // extraneous brace should end parsing
    CxxParser.Diag(Tok, diag::err_heavy_scheme)
      << "extraneous closing brace ('}')";
    return ValueEmpty();
  }
  default: {
    CxxParser.Diag(Tok, diag::err_heavy_scheme)
      << "expected expression";
    return ValueError();
  }
  }
}

// ParseExprAbbrev - Normalizes abbreviated prefix notation to
//                   their equivalent syntax e.g. (quote expr)
ValueResult ParserHeavyScheme::ParseExprAbbrev(char const* Name) {
  Token Abbrev = Tok;
  ConsumeToken();
  ValueResult Result = ParseExpr();
  if (!Result.isUsable()) return Result;

  Value* S = Context.CreateSymbol(Name, Abbrev.getLocation());
  Value* P = Context.CreatePair(S, Context.CreatePair(Result.get()));
  return P;
}

ValueResult ParserHeavyScheme::ParseListStart() {
  // Consume the l_paren
  assert(Tok.is(tok::l_paren));
  Token StartTok = Tok;
  ConsumeToken();
  return ParseList(StartTok);
}

ValueResult ParserHeavyScheme::ParseList(Token const& StartTok) {
  Token CurTok = Tok;
  // TODO Use StartTok to identify the proper
  //      closing token to match with
  if (Tok.is(tok::r_paren)) {
    ConsumeToken();
    return Context.CreateEmpty();
  }

  ValueResult Car = ParseExpr();
  if (!Car.isUsable()) return Car;

  ValueResult Cdr;
  if (Tok.is(tok::period)) {
    Cdr = ParseDottedCdr(StartTok);
  } else {
    Cdr = ParseList(StartTok);
  }

  if (!Cdr.isUsable()) return Cdr;

  return Context.CreatePairWithSource(Car.get(),
                                      Cdr.get(),
                                      CurTok.getLocation());
}

// We have a dot while parsing a list,
// so we expect a single expression
// then the closing r_paren
ValueResult ParserHeavyScheme::ParseDottedCdr(Token const& StartTok) {
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

ValueResult ParserHeavyScheme::ParseVectorStart() {
  // consume the heavy_vector_lparen
  ConsumeToken();
  SmallVector<Value*, 16> Xs;
  return ParseVector(Xs);
}

ValueResult ParserHeavyScheme::ParseVector(SmallVectorImpl<Value*>& Xs) {
  if (Tok.is(tok::r_paren)) {
    ConsumeToken();
    return Context.CreateVector(Xs);
  }
  ValueResult Result = ParseExpr();
  if (!Result.isUsable()) return Result;

  Xs.push_back(Result.get());
  return ParseVector(Xs);
}

ValueResult ParserHeavyScheme::ParseCharConstant() {
  llvm_unreachable("TODO");
}

ValueResult ParserHeavyScheme::ParseNumber() {
  char const* Current = Tok.getLiteralData();
  char const* End = Current + Tok.getLength();
  int BitWidth = Context.GetIntWidth();
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
    return Context.CreateInteger(IntOpt.getValue());
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
  return Context.CreateFloat(FloatVal);
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
  return Context.CreateString(StringRef(LiteralResult));
}

ValueResult ParserHeavyScheme::ParseSymbol() {
  StringRef Str = Tok.getRawIdentifier();
  SourceLocation Loc = Tok.getLocation();
  ConsumeToken();
  return Context.CreateSymbol(Str, Loc);
}

ValueResult ParserHeavyScheme::ParseTypename() {
  llvm_unreachable("TODO");
  return ValueError();
}

ValueResult ParserHeavyScheme::ParseCppDecl() {
  llvm_unreachable("TODO");
  return ValueError();
}

