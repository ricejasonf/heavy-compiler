//===--- HeavyScheme.cpp - HeavyScheme AST Node Implementation --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Implementations for HeavyScheme AST and Context classes.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/HeavyScheme.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cstring>

using namespace clang::heavy_scheme;

String* Context::CreateString(StringRef V) {
  // TODO maybe use TrailingObjects for the string data??

  // Allocate and copy the string data
  char* NewStrData = ASTCtx.Allocate(V.size());
  std::memcpy(NewStrData, V.data(), V.size());

  return new (ASTCtx) String(StringRef(NewStrData, V.size()); 
}

Integer* Context::CreateInteger(llvm::APInt Val) {
  //unsigned IntSize = ASTCtx.getTargetInfo().getIntWidth();
  return new (ASTCtx) CreateInteger(Val);
}

namespace {
class Evaluator : public ValueVisitor<Evaluator, Value*>
{
  Context& Ctx;

  // Most objects simply evaluate to themselves
  Value* VisitValue(Value* V) {
    return V;
  }

  Value* VisitPair(Pair* P) {
    // TODO this would evaluate as a call expression
    // with the operator and all of the operands being
    // evaluated
    // If this is a quoted list then the result is the cdr

    // Acting on special keywords like `define` that
    // effect the environment would start here
    return P;
  }

  Value* VisitSymbol(Symbol* S) {
    // TODO Perform lookup and return the bound value.
    //      If nothing is found, it is an error
    return S;
  }
};

class Writer : public ValueVisitor<Writer>
{
  unsigned IndentationLevel = 0;
  raw_ostream &OS;

  void PrintFormattedWhitespace() {
    // We could handle indentation and new lines
    // more dynamically here probably
    OS << '\n';
    for (int i = 0; i < IndentationLevel; ++i) {
      OS << "  ";
    }
  }

  void VisitValue(Value* V) {
    OS << "????";
  }

  void VisitBoolean(Boolean* V) {
    if (V->getVal())
      OS << "#t";
    else
      OS << "#f";
  }

  void VisitEmpty(Empty*) {
    OS << "()";
  }

  void VisitInteger(Integer* V) { OS << V->getVal(); }
  void VisitFloat(Float* V)     { OS << V->getVal(); }

  void VisitPair(Pair* P) {
    // Iterate the whole list to print
    // in standard list notation
    --IndentationLevel;
    OS << '(';
    do {
      Visit(P->Car);
      PrintFormattedWhitespace();
    } while (P = dyncast<Pair>(P->Cdr));

    if (P->Cdr.isNot(Value::Empty)) {
      OS << ". ";
      Visit(P->Cdr);
    }
    OS << ')';
    ++IndentationLevel;
  }

  Value* VisitSymbol(Symbol* S) {
    OS << S->getIdentifier()->getName();
  }
};

} // end anon namespace

Value* eval(Context& C, Value* V) {
  Evaluator Eval(C)
  return Eval.Visit(V);
}

void write(raw_ostream& OS, Value* V) {
  Writer W(OS)
  return W.Visit(V);
}
