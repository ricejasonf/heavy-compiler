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
#include "clang/Parse/Parser.h"
#include "clang/AST/HeavyScheme.h"
#include "clang/Basic/LLVM.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cstring>

using namespace clang::heavy;
using clang::dyn_cast;
using clang::cast;
using clang::isa;

// called inside GetIntWidth
unsigned Context::GetHostIntWidth() const {
  assert(CxxParser);
  return CxxParser->getActions()
                   .getASTContext()
                   .getTargetInfo()
                   .getIntWidth();
}

String* Context::CreateString(StringRef V) {
  // TODO maybe use TrailingObjects for the string data??

  // Allocate and copy the string data
  char* NewStrData = (char*) TrashHeap.Allocate<char>(V.size());
  std::memcpy(NewStrData, V.data(), V.size());

  return new (TrashHeap) String(StringRef(NewStrData, V.size()));
}

Integer* Context::CreateInteger(llvm::APInt Val) {
  return new (TrashHeap) Integer(Val);
}

Float* Context::CreateFloat(llvm::APFloat Val) {
  return new (TrashHeap) Float(Val);
}

#if 0 // TODO implement creating a Procedure
bool Context::CheckFormals(Value* V, int& Arity) {
  if (isa<Empty>(V)) return;
  if (isa<Symbol>(V)) {
    // If the formals are just a Symbol
    // or an improper list ending with a
    // Symbol, then that Symbol is a "rest"
    // parameter that binds remaining
    // arguments as a list.
    Arity = -1;
    return false;
  }

  Pair* P = dyn_cast<Pair>(V);
  Symbol* S = nullptr;
  if (!P || !isa<Symbol>(P->Car)) {
    llvm::errs() << "\nTODO diagnose invalid formals\n";
    return true;
  }

  ++Arity;
  return CheckFormals(P->Cdr, Arity);
}

Procedure* Context::CreateProcedure(Pair* P) {
  int Arity = 0;
  Value* Formals = P.Car;
  BindingRegion* Region = CreateRegion();
  ProcessFormals(Formals, Region, Arity);

  // The rest are expressions considered as the body
  Procedure* New = new (TrashHeap) Procedure(/*stuff*/);
}
#endif

namespace {
template <typename AllocatorTy>
struct ModuleRegion {
  llvm::StringMap<Value*, AllocatorTy> Map;

  ModuleRegion(ModuleRegion* P,
               AllocatorTy A)
    : Map(A)
  { }
};

class Evaluator : public ValueVisitor<Evaluator, ValueResult> {
  friend class ValueVisitor<Evaluator, ValueResult>;
  Context& Ctx;
  clang::ASTContext* CxxAst = nullptr;

public:
  Evaluator(Context& C)
    : Ctx(C)
  { }

  Evaluator(Context& C, clang::ASTContext& A)
    : Ctx(C)
    , CxxAst(&A)
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

#if 0
  // BindArguments
  // Args should be a list of evaluated inputs
  ValueResult BindArguments(Pair* Region,
                            Pair* Args,
                            Value* Formals) {
    Pair* P;
    switch (Formals.getValueKind()) {
    case Value::Empty: {
      llvm::errs() <<
        "\nTODO Diagnose arity mismatch (too many parameters)\n";
      return true;
    }
    case Value::Symbol:
      // Bind remaining args to "rest" parameter
      Symbol* Name = cast<Symbol>(Formals);
      AddBinding(Region, Formals, Args);
      return false;
    case Value::Pair:
      P = cast<Pair>(Formals);
      Symbol* Name = cast<Symbol>(P->Car);
      AddBinding(Region, Name, Args->Car);
      break;
    default:
      llvm_unreachable("Formals should already be checked");
    };

    Pair* NextArgs = dyn_cast<Pair>(Args->Cdr);
    if (!NextArgs) {
      llvm::errs() <<
        "\nTODO Diagnose arity mismatch (too few parameters)\n";
      return true;
    }

    return BindArguments(Region, NextArgs, P->Cdr);
  }

  void AddBinding(Pair* Region, Symbol* Name, Value* Arg) {
    Binding* NewBinding = Ctx->CreateBinding(Name, Arg);
    Region->Cdr = NewBinding;
  }

  // Arguments are evaluated right to left
  // to prevent accidental reliance on unspecified
  // behaviour that may change for different "backends".
  ValueResult EvalArguments(Value* Args) {
    if (isa<Empty>(Args)) return Args;
    Pair* P = dyn_cast<Pair>(Args);
    if (!P) {
      llvm::errs() <<
        "\nTODO Diagnose invalid syntax for call expression\n";
      return ValueError();
    }
    ValueResult RestResult = EvalArgs(P->Cdr);
    if (!RestResult.isUsable()) return ValueError();
    ValueResult ArgResult = Visit(P->Car);
    if (!RestResult.isUsable()) return ValueError();
    return Ctx.CreatePair(ArgResult.get(), RestResult.get());
  }

  // LookupSymbol - Uses the current Region to start
  //                searching for a symbol, defaulting to
  //                searching the module environment and then
  //                the top level environment
  ValueResult LookupSymbol(Symbol* S) {
    ValueResult R = Region.Lookup(S);
    if (R.isUsable()) return R;
    return Module.Lookup(S);
  }
#endif
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

namespace clang { namespace heavy {
ValueResult eval(Context& C, Value* V) {
  Evaluator Eval(C);
  return Eval.Visit(V);
}

void write(llvm::raw_ostream& OS, Value* V) {
  Writer W(OS);
  return W.Visit(V);
}
}} // end of namespace clang::heavy
