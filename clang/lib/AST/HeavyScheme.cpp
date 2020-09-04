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

#include "clang/AST/ASTContext.h"
#include "clang/AST/HeavyScheme.h"
#include "clang/Basic/LLVM.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cstring>

using namespace clang::heavy_scheme;
using clang::dyn_cast;
using clang::cast;
using clang::isa;

String* Context::CreateString(StringRef V) {
  // TODO maybe use TrailingObjects for the string data??

  // Allocate and copy the string data
  char* NewStrData = (char*)ASTCtx.Allocate(V.size());
  std::memcpy(NewStrData, V.data(), V.size());

  return new (ASTCtx) String(StringRef(NewStrData, V.size())); 
}

Integer* Context::CreateInteger(llvm::APInt Val) {
  return new (ASTCtx) Integer(Val);
}

Float* Context::CreateFloat(llvm::APFloat Val) {
  return new (ASTCtx) Float(Val);
}

namespace {
class Evaluator : public ValueVisitor<Evaluator, ValueResult>
{
  friend class ValueVisitor<Evaluator, ValueResult>;
  Context& Ctx;

public:
  Evaluator(Context& C)
    : Ctx(C)
  { }

private:
  // Most objects simply evaluate to themselves
  ValueResult VisitValue(Value* V) {
    return V;
  }

  ValueResult VisitPair(Pair* P) {
    // TODO this would evaluate as a call expression
    // with the operator and all of the operands being
    // evaluated
    // If this is a quoted list then the result is the cdr

    // Acting on special keywords like `define` that
    // effect the environment would start here
    return P;
  }

  ValueResult VisitSymbol(Symbol* S) {
    // TODO Perform lookup and return the bound value.
    //      If nothing is found, it is an error
    return S;
  }
};

class Writer : public ValueVisitor<Writer>
{
  friend class ValueVisitor<Writer>;
  unsigned IndentationLevel = 0;
  llvm::raw_ostream &OS;

public:
  Writer(llvm::raw_ostream& OS)
    : OS(OS)
  { }

private:
  void PrintFormattedWhitespace() {
    // We could handle indentation and new lines
    // more dynamically here probably
    OS << '\n';
    assert(IndentationLevel < 100 && "IndentationLevel overflow suspected");

    for (unsigned i = 0; i < IndentationLevel; ++i) {
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
  void VisitFloat(Float* V) {
    llvm::SmallVector<char, 16> Buffer;
    V->getVal().toString(Buffer);
    OS << Buffer;
  }

  void VisitPair(Pair* P) {
    // Iterate the whole list to print
    // in standard list notation
    ++IndentationLevel;
    OS << '(';
    Visit(P->Car);
    Value* Cdr = P->Cdr;
    while (isa<Pair>(Cdr)) {
      OS << ' ';
      //PrintFormattedWhitespace();
      P = cast<Pair>(Cdr);
      Visit(P->Car);
      Cdr = P->Cdr;
    };

    if (!Empty::classof(Cdr)) {
      OS << " . ";
      Visit(Cdr);
    }
    OS << ')';
    --IndentationLevel;
  }

  void VisitSymbol(Symbol* S) {
    OS << S->getVal();
  }

  void VisitString(String* S) {
    // TODO we might want to escape special
    // characters other than newline
    OS << '"' << S->Val << '"';
  }
};

} // end anon namespace

namespace clang { namespace heavy_scheme {
ValueResult eval(Context& C, Value* V) {
  Evaluator Eval(C);
  return Eval.Visit(V);
}

void write(llvm::raw_ostream& OS, Value* V) {
  Writer W(OS);
  return W.Visit(V);
}
}} // end of namespace clang::heavy_scheme
