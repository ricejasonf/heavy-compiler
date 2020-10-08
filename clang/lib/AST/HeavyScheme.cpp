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
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cstring>

using namespace clang::heavy;
using clang::dyn_cast;
using clang::cast;
using clang::isa;
using llvm::ArrayRef;

std::unique_ptr<Context> Context::CreateEmbedded(clang::Parser& P) {
  auto Cptr = std::make_unique<Context>();
  Cptr->CxxParser = &P;
  Cptr->InitTopLevel();
  return Cptr;
}

Context::Context()
  : TrashHeap()
  , EvalStack()
  , SystemModule(LoadSystemModule())
  , EnvStack(CreatePair(SystemModule, CreateEmpty()))
  , ErrorHandlerStack(CreateEmpty())
{ }

Context::LoadSystemModule() {
  SystemModule = CreateModule();
  AddBuiltin("+", builtins::add);
}

// called inside GetIntWidth
unsigned Context::GetHostIntWidth() const {
  assert(CxxParser);
  return CxxParser->getActions()
                   .getASTContext()
                   .getTargetInfo()
                   .getIntWidth();
}

String* Context::CreateString(StringRef V) {
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

Vector* Context::CreateVector(ArrayRef<Value*> Xs) {
  // Copy the list of Value* to our heap
  Value** Values = TrashHeap.Allocate<Value*>(Xs.size());
  std::copy(Xs.begin(), Xs.end(), Values);
  return new (TrashHeap) Vector(ArrayRef<Value*>(Values, Xs.size()));
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
// Evaluator
//  - expands syntax
//  - pushes to the evaluation stack
//  - to work with builtins and bytecode
//  - uses RAII to replace the Context.EnvStack
class Evaluator : public ValueVisitor<Evaluator> {
  friend class ValueVisitor<Evaluator>;
  clang::heavy::Context& Context;
  // We use RAII to make the current call to `eval`
  // set the environment in Context
  Value* OldEnvStack;
  clang::ASTContext* CxxAst = nullptr;

public:
  Evaluator(Context& C, Pair* Env) // TODO make Env an `Environment`
    : Ctx(C)
  {
    OldEnvStack = Context.EnvStack;
    Context.EnvStack = Env;
  }

  ~Evaluator() {
    Context.EnvStack = OldEnvStack;
  }

private:
  void push(Value* V) {
    Context.EvalStack.push();
  }
  Value* pop() {
    return Context.EvalStack.pop();
  }
  Value* top() {
  }

  // Most objects simply evaluate to themselves
  void VisitValue(Value* V) {
    push(V);
  }

  void VisitPair(Pair* P) {
    // Visit each element in reverse order and evaluate
    // on the stack.
    int Len = 0;
    EvalArguments(P, Len);
    
    // The Car is the operator of a call expression
    Value* Operator = pop();
    switch (Operator) {
      case Value::Procedure:
        llvm_unreachable("TODO");
        break;
      case Value::Syntax:
        llvm_unreachable("TODO");
        break;
      case Value::Builtin:
        Builtin* B = cast<Builtin>(Operator);
        B->Fn(Context, Len);
        break;
      case Value::BuiltinSyntax:
        // I think this will be just like a builtin function
        // with pointers to unevaluated AST as operands on the
        // call stack (do we even need to use the call stack?)
        llvm_unreachable("TODO");
        break;
      default:
        llvm::errs() << "Expression is not a function\n";
        break;
    }
  }

  void EvalArguments(Value* Args, int& Len) {
    if (isa<Empty>(Args)) return;
    Pair* P = dyn_cast<Pair>(Args);
    if (!P) {
      llvm::errs() <<
        "\nTODO Diagnose invalid syntax for call expression\n";
      return ValueError();
    }
    EvalArguments(P->Cdr, Len);
    Len += 1;
    Visit(P->Car);
  }

  Value* VisitSymbol(Symbol* S) {
    Binding* Result = dyn_cast_or_null<Binding>(Context.Lookup(S));
    if (!Result) {
      llvm::errs() << "Unbound symbol: " << S.getVal() << '\n';
      // erm uhh
    }
    push(Result.getValue());
  }
#if 0
  // BindArguments
  // Args should be a list of evaluated inputs
  // TODO rewrite this to create an EnvFrame
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

  void VisitVector(Vector* Vec) {
    OS << "#(";
    ArrayRef<Value*> Xs = Vec->getElements();
    if (!Xs.empty()) {
      Visit(Xs[0]);
      Xs = Xs.drop_front(1);
      for (Value* X : Xs) {
        OS << ' ';
        Visit(X);
      }
    }
    OS << ')';
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
struct NumberOp {
  // These operations always mutate the first operand
  struct Add {
    static void f(Integer* X, Integer* Y) { X.Val += Y.Val; }
    static void f(Float* X, Float *Y) { X.Val = X.Val + Y.Val; }
  };
  struct Sub {
    static void f(Integer* X, Integer* Y) { X.Val -= Y.Val; }
    static void f(Float* X, Float *Y) { X.Val = X.Val - Y.Val; }
  };
  struct Mul {
    static void f(Integer* X, Integer* Y) { X.Val *= Y.Val; }
    static void f(Float* X, Float *Y) { X.Val = X.Val * Y.Val; }
  };
  struct Div {
    static void f(Integer* X, Integer* Y) { X.Val.sdiv(Y.Val); }
    static void f(Float* X, Float *Y) { X.Val = X.Val / Y.Val; }
  };
};
}} // end namespace clang::heavy

namespace clang { namespace heavy { namespace builtin {
void eval(Context& C, int Len) {
  if (C.CheckArityAtLeast(1, Len)) return;
  Value* ExprOrDef = C.popArg<>();
  Pair* EnvStack = (Len == 1) ? C.popArg<Pair>() : C.EnvStack;
  // TODO Environment should be the result of evaluating an `environment` spec
  //Environment* EnvStack = (Len == 1) ? C.pop<EnvironmentStack>() : C.EnvStack;
  if (C.CheckError()) return;
  Evaluator Eval(C, EnvStack);
  Eval.Visit(V);
}

template <typename Op>
void operator_rec(Context& C, int Len) {
  if (Len == 1) return;
  if (Len == 0) {
    C.EvalStack.push(C.CreateInteger(0));
    return;
  }
  // pop two arguments and push the result of adding them
  Number* X = C.popArg<Number>();
  Number* Y = C.popArg<Number>();
  if (C.CheckError()) return;
  Value::Kind CommonKind = Number::CommonKind(Sum, X);
  Number* Result;
  switch (CommonKind) {
    case Value::Kind::Float: {
      Float* CopyX = C.CreateFloat(X);
      Float* CopyY = C.CreateFloat(Y);
      Op::f(CopyX, CopyY);
      Result = CopyX;
      break;
    }
    case Value::Kind::Integer: {
      // we can assume they are both integers
      Integer* CopyX = C.CreateInteger(cast<Integer>(X).getVal());
      Op::f(CopyX, Y);
      Result = CopyX;
      break;
    }
    default:
      llvm_unreachable("Invalid arithmetic type");
  }
  C.EvalStack.push(Result);
  operator_rec<Op>(C, Len - 1);
}

void forbid_div_zeros(Context&C, int Len) {
  Number* Num = C.popArg<Number>();
  if (Num->isExactZero()) {
    C.SetError("Divide by exact zero", Num);
    return;
  }

  forbid_div_zeros(C, Len - 1);
  // put the value back on the stack
  C.EvalContext.push(Num);
}

void operator_div(Context& C, int Len) {
  Number* Num = C.popArg<Number>();
  if (CheckError()) return;
  if (Num->isExactZero()) {
    C.EvalStack.discard(Len - 1)
    C.EvalStack.push(Num);
    return;  
  }
  forbid_div_zeros(C, Len);
  if (CheckError()) return;
  // put the value back on the stack
  C.EvalContext.push(Num);
  operator_rec<NumberOp::Div>(C, Len);
}

}} // end of namespace clang::heavy::builtin

namespace clang { namespace heavy {
  // This is just a temporary interface for printing
  // top level expressions
  Value* eval(Context& C, Value* V) {
    C.EvalStack.push(V);
    builtin::eval(C, 1);
    return C.EvalStack.pop();
  }

  void write(llvm::raw_ostream& OS, Value* V) {
    Writer W(OS);
    return W.Visit(V);
  }
}} // end namespace clang::heavy
