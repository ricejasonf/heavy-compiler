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

void Value::dump() {
  write(llvm::errs(), this);
}

std::unique_ptr<Context> Context::CreateEmbedded(clang::Parser& P) {
  auto Cptr = std::make_unique<Context>();
  Cptr->CxxParser = &P;
  return Cptr;
}

Context::Context()
  : TrashHeap()
  , EvalStack()
  , SystemModule(CreateModule())
  , EnvStack(CreatePair(CreateEnvironment()))
  , ErrorHandlerStack(CreateEmpty())
{
  LoadSystemModule();
}

// called inside GetIntWidth
unsigned Context::GetHostIntWidth() const {
  assert(CxxParser);
  return CxxParser->getActions()
                   .getASTContext()
                   .getTargetInfo()
                   .getIntWidth();
}

String* Context::CreateString(StringRef S) {
  // Allocate and copy the string data
  char* NewStrData = (char*) TrashHeap.Allocate<char>(S.size());
  std::memcpy(NewStrData, S.data(), S.size());

  return new (TrashHeap) String(StringRef(NewStrData, S.size()));
}

// This is handy for creating error messages that usually involve
// concatenating two string constants, usually a message and a
// value kind.
String* Context::CreateString(StringRef S1, StringRef S2) {
  // Allocate and copy the string data
  unsigned size = S1.size() + S2.size();
  char* NewStrData = (char*) TrashHeap.Allocate<char>(size);
  std::memcpy(NewStrData            , S1.data(), S1.size());
  std::memcpy(NewStrData + S1.size(), S2.data(), S2.size());

  return new (TrashHeap) String(StringRef(NewStrData, size));
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
  Value* Formals = P->Car;
  BindingRegion* Region = CreateRegion();
  ProcessFormals(Formals, Region, Arity);

  // The rest are expressions considered as the body
  Procedure* New = new (TrashHeap) Procedure(/*stuff*/);
}
#endif

Binding* Context::Lookup(Symbol* Name, Value* Stack) {
  if (isa<Empty>(Stack)) return nullptr;
  Value* Result = nullptr;
  Value* V    = cast<Pair>(Stack)->Car;
  Value* Next = cast<Pair>(Stack)->Cdr;
  switch (V->getKind()) {
#if 0
    case Value::Kind::EnvFrame:
      Result = cast<EnvFrame>(V)->Lookup(Name);
      break;
#endif
    case Value::Kind::Module:
      Result = cast<Module>(V)->Lookup(Name);
      break;
    case Value::Kind::Environment:
      Next = cast<Environment>(V)->EnvStack;
      break;
    default:
      llvm_unreachable("Invalid Lookup Type");
  }
  if (Result) {
    if (ForwardRef* F = dyn_cast<ForwardRef>(Result)) {
      Result = F->Val;
    }
    return cast<Binding>(Result);
  }
  return Lookup(Name, Next);
}

namespace clang { namespace heavy {
struct NumberOp {
  // These operations always mutate the first operand
  struct Add {
    static void f(Integer* X, Integer* Y) { X->Val += Y->Val; }
    static void f(Float* X, Float *Y) { X->Val = X->Val + Y->Val; }
  };
  struct Sub {
    static void f(Integer* X, Integer* Y) { X->Val -= Y->Val; }
    static void f(Float* X, Float *Y) { X->Val = X->Val - Y->Val; }
  };
  struct Mul {
    static void f(Integer* X, Integer* Y) { X->Val *= Y->Val; }
    static void f(Float* X, Float *Y) { X->Val = X->Val * Y->Val; }
  };
  struct Div {
    static void f(Integer* X, Integer* Y) { X-> Val = X->Val.sdiv(Y->Val); }
    static void f(Float* X, Float *Y) { X->Val = X->Val / Y->Val; }
  };
};

// GetSingleArg - Given a macro expression (keyword datum)
//                return the first argument iff there is only
//                one argument otherwise returns nullptr
//                (same as `cadr`)
Value* GetSingleSyntaxArg(Pair* P) {
  // P->Car is the syntactic keyword
  Pair* P2 = dyn_cast<Pair>(P->Cdr);
  if (P2 && isa<Empty>(P2->Cdr)) {
    return P2->Car;
  }
  return nullptr;
}
}} // end namespace clang::heavy

namespace {
class SyntaxExpander : public ValueVisitor<SyntaxExpander, Value*> {
  friend class ValueVisitor<SyntaxExpander, Value*>;
  clang::heavy::Context& Context;
  // The "syntactic environment" extends
  // the current top level environment
  Value* OldEnvStack;

public:
  SyntaxExpander(clang::heavy::Context& C)
    : Context(C)
  {
    OldEnvStack = Context.EnvStack;
  }

  ~SyntaxExpander() {
    Context.EnvStack = OldEnvStack;
  }

private:
  Value* VisitValue(Value* V) {
    return V;
  }

  Value* VisitPair(Pair* P) {
    if (Context.CheckError()) return Context.CreateEmpty();
    Binding* B = Context.Lookup(P->Car);
    if (!B) return P;

    // Operator might be some kind of syntax transformer
    Value* Operator = B->getValue();

    switch (Operator->getKind()) {
      case Value::Kind::BuiltinSyntax: {
        BuiltinSyntax* BS = cast<BuiltinSyntax>(Operator);
        return BS->Fn(Context, P);
      }
      case Value::Kind::Syntax:
        llvm_unreachable("TODO");
        return Context.CreateEmpty();
      default:
        // not a syntax operator
        return P;
    }
  }
};

class Quasiquoter : private ValueVisitor<Quasiquoter, Value*> {
  friend class ValueVisitor<Quasiquoter, Value*>;
  clang::heavy::Context& Context;
  // Values captured for hygiene purposes
  Value* Append;
  Value* ConsSource;

public:

  Quasiquoter(clang::heavy::Context& C)
    : Context(C)
    , Append(C.GetBuiltin("append"))
    , ConsSource(C.GetBuiltin("cons-source"))
  { }

  // <quasiquotation> 
  Value* Run(Pair* P) {
    bool Rebuilt = false;
    // <quasiquotation 1>
    return HandleQuasiquote(P, Rebuilt, /*Depth=*/1);
  }

private:

  Value* VisitValue(Value* V, bool& Rebuilt, int Depth) {
    return V;
  }

  // <qq template D>
  Value* HandleQQTemplate(Value* V, bool& Rebuilt, int Depth) {
    assert(Depth >= 0 && "Depth should not be negative");
    if (Depth < 1) {
      // Unquoting requires parents to be rebuilt
      Rebuilt = true;
      return V;
    }
    return Visit(V, Rebuilt, Depth);
  }

  // <quasiquotation D>
  Value* HandleQuasiquote(Pair* P, bool& Rebuilt, int Depth) {
    Value* Input = GetSingleSyntaxArg(P);
    if (!Input) {
      Context.SetError("Invalid quasiquote syntax", P);
      return Context.CreateEmpty();
    }
    Value* Result = Visit(Input, Rebuilt, Depth);
    if (!Rebuilt) return Context.CreateQuote(Input);
    return Result;
  }

  // <unquotation D>
  Value* HandleUnquote(Pair* P, bool& Rebuilt, int Depth) {
    Value* Input = GetSingleSyntaxArg(P);
    if (!Input) {
      Context.SetError("Invalid unquote syntax", P);
      return Context.CreateEmpty();
    }

    Value* Result = HandleQQTemplate(Input, Rebuilt, Depth - 1);
    if (!Rebuilt) return P;
    return Result;
  }

  Value* HandleUnquoteSplicing(Pair* P, Value* Next, bool& Rebuilt,
                               int Depth) {
    Value* Input = GetSingleSyntaxArg(P);
    if (!Input) {
      Context.SetError("Invalid unquote-splicing syntax", P);
      return Context.CreateEmpty();
    }
    Value* Result = HandleQQTemplate(Input, Rebuilt, Depth - 1);
    if (!Rebuilt) {
      return P;
    }

    if (isa<Pair>(Result)) {
      // It is an error if unquote-splicing does not result in a list
      Context.SetError("unquote-splicing must evaluate to a list", P);
      return Context.CreateEmpty();
    }
    // append Next to Input (during evaluation)
    return Context.CreatePair(Append, Context.CreatePair(Result, Next));
  }

  Value* VisitPair(Pair* P, bool& Rebuilt, int Depth) {
    assert(Depth > 0 && "Depth cannot be zero here.");
    if (Context.CheckError()) return Context.CreateEmpty();
    if (P->Car->isSymbol("quasiquote")) {
      return HandleQuasiquote(P, Rebuilt, Depth + 1);
    } else if (P->Car->isSymbol("unquote")) {
      return HandleUnquote(P, Rebuilt, Depth);
    } else if (isa<Pair>(P->Car) &&
               cast<Pair>(P->Car)->Car->isSymbol("unquote-splicing")) {
      Pair* P2 = cast<Pair>(P->Car);
      return HandleUnquoteSplicing(P2, P->Cdr, Rebuilt, Depth);
    } else {
      // Just a regular old pair
      // <list qq template D>
      bool CarRebuilt = false;
      bool CdrRebuilt = false;
      Value* Car = Visit(P->Car, CarRebuilt, Depth);
      Value* Cdr = Visit(P->Cdr, CdrRebuilt, Depth);
      // Portions that are not rebuilt are always literal
      // '<qq template D>
      if (!CarRebuilt && CdrRebuilt) Car = Context.CreateQuote(Car);
      if (!CdrRebuilt && CarRebuilt) Cdr = Context.CreateQuote(Cdr);
      Rebuilt = CarRebuilt || CdrRebuilt;
      if (!Rebuilt) return P;
      return Context.CreatePair(ConsSource,
              Context.CreatePair(Car,
                Context.CreatePair(Cdr,
                  Context.CreatePair(
                    Context.CreateQuote(P)))));
    }
  }

  // TODO VisitVector (it appears to be missing from Evaluator too)
};


// Evaluator
//  - tree evaluator that uses the
//    evaluation stack
//  - to work with builtins and bytecode
//  - uses RAII to replace the Context.EnvStack
class Evaluator : public ValueVisitor<Evaluator> {
  friend class ValueVisitor<Evaluator>;
  clang::heavy::Context& Context;
  // We use RAII to make the current call to `eval`
  // set the environment in Context
  Value* OldEnvStack;

public:
  Evaluator(clang::heavy::Context& C, Value* Env)
    : Context(C)
  {
    OldEnvStack = Context.EnvStack;
    Context.EnvStack = Env;
  }

  ~Evaluator() {
    Context.EnvStack = OldEnvStack;
  }

private:
  void push(Value* V) {
    Context.EvalStack.push(V);
  }
  Value* pop() {
    return Context.EvalStack.pop();
  }
  Value* top() {
    return Context.EvalStack.top();
  }

  // Most objects simply evaluate to themselves
  void VisitValue(Value* V) {
    if (Context.CheckError()) return;
    push(V);
  }

  void VisitPair(Pair* P) {
    if (Context.CheckError()) return;
    // Visit each element in reverse order and evaluate
    // on the stack.
    int Len = 0;
    EvalArguments(P, Len);
    if (Context.CheckError()) return;

    Value* Operator = pop();
    --Len;
    // TODO Check arity here since we have
    //      the caller and callee
    // TODO We could check a "Contract"
    //      defined for builtins
    switch (Operator->getKind()) {
      case Value::Kind::Procedure:
        llvm_unreachable("TODO");
        break;
      case Value::Kind::Builtin: {
        Builtin* B = cast<Builtin>(Operator);
        B->Fn(Context, Len);
        break;
      }
      default: {
        String* Msg = Context.CreateString(
          "Invalid operator for call expression: ",
          Operator->getKindName()
        );
        Context.SetError(P->getSourceLocation(), Msg, Operator);
        return;
      }
    }
  }

  void VisitQuote(Quote* Q) {
    // simply unwrap the quoted value
    push(Q->Val);
  }

  void VisitSymbol(Symbol* S) {
    if (Context.CheckError()) return;
    Binding* Result = Context.Lookup(S);
    if (!Result) {
      String* Msg = Context.CreateString("Unbound symbol: ", S->getVal());
      Context.SetError(Msg, S);
      return;
    }
    push(Result->getValue());
  }

  void EvalArguments(Value* Args, int& Len) {
    if (isa<Empty>(Args)) return;
    Pair* P = dyn_cast<Pair>(Args);
    if (!P) {
      Context.SetError("Call expression must be a proper list", Args);
      return ;
    }
    // Arguments are evaluated right to left
    EvalArguments(P->Cdr, Len);
    if (Context.CheckError()) return;
    Len += 1;
    Visit(P->Car);
  }

#if 0
  // BindArguments
  // Args should be a list of evaluated inputs
  // TODO rewrite this to create an EnvFrame
  ValueResult BindArguments(Pair* Region,
                            Pair* Args,
                            Value* Formals) {
    Pair* P;
    switch (Formals->getKind()) {
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
    OS << "<Value of Kind:"
       << V->getKindName()
       << ">";
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

  void VisitQuote(Quote* Q) {
    OS << "(quote ";
    Visit(Q->Val);
    OS << ")";
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
namespace clang { namespace heavy { namespace builtin {
void eval(Context& C, int Len) {
  assert((Len == 1 || Len == 2) && "Invalid arity to builtin `eval`");
  Value* ExprOrDef = C.EvalStack.pop();
  Environment* Env = (Len == 2) ? C.popArg<Environment>() : nullptr;
  Value* EnvStack = Env ? Env->EnvStack : C.EnvStack;
  if (C.CheckError()) return;
  Evaluator Eval(C, EnvStack);
  Eval.Visit(ExprOrDef);
}

template <typename Op>
void operator_rec(Context& C, int Len) {
  if (Len == 1) return;
  if (Len == 0) {
    C.push(C.CreateInteger(0));
    return;
  }
  // pop two arguments and push the result of adding them
  Number* X = C.popArg<Number>();
  Number* Y = C.popArg<Number>();
  if (C.CheckError()) return;
  Value::Kind CommonKind = Number::CommonKind(X, Y);
  Number* Result;
  switch (CommonKind) {
    case Value::Kind::Float: {
      Float* CopyX = C.CreateFloat(cast<Float>(X)->getVal());
      Float* CopyY = C.CreateFloat(cast<Float>(Y)->getVal());
      Op::f(CopyX, CopyY);
      Result = CopyX;
      break;
    }
    case Value::Kind::Integer: {
      // we can assume they are both integers
      Integer* CopyX = C.CreateInteger(cast<Integer>(X)->getVal());
      Op::f(CopyX, cast<Integer>(Y));
      Result = CopyX;
      break;
    }
    default:
      llvm_unreachable("Invalid arithmetic type");
  }
  C.push(Result);
  operator_rec<Op>(C, Len - 1);
}

void forbid_div_zeros(Context& C, int Len) {
  if (Len == 0) return;
  Number* Num = C.popArg<Number>();
  if (Num->isExactZero()) {
    C.SetError("Divide by exact zero", Num);
    return;
  }

  forbid_div_zeros(C, Len - 1);
  // put the value back on the stack
  C.EvalStack.push(Num);
}

void operator_add(Context&C, int Len) {
  operator_rec<NumberOp::Add>(C, Len);
}
void operator_mul(Context&C, int Len) {
  operator_rec<NumberOp::Mul>(C, Len);
}
void operator_sub(Context&C, int Len) {
  operator_rec<NumberOp::Sub>(C, Len);
}
void operator_div(Context& C, int Len) {
  Number* Num = C.popArg<Number>();
  if (C.CheckError()) return;
  if (Num->isExactZero()) {
    C.discard(Len - 1);
    C.push(Num);
    return;
  }
  forbid_div_zeros(C, Len - 1);
  if (C.CheckError()) return;
  // put the first arg back on the stack
  C.EvalStack.push(Num);
  operator_rec<NumberOp::Div>(C, Len);
}

void list(Context& C, int Len) {
  // Returns a *newly allocated* list of its arguments.
  if (Len == 0) {
    C.push(C.CreateEmpty());
    return;
  };
  Value* Arg = C.pop();
  list(C, Len - 1);
  Value* Next = C.pop();
  C.push(C.CreatePair(Arg, Next));
}

void cons_source(Context& C, int Len) {
  Value* V1 = C.pop();
  Value* V2 = C.pop();
  Value* V3 = C.pop();
  C.push(C.CreatePairWithSource(V1, V2, V3->getSourceLocation()));
}

void append(Context& C, int Len) {
  llvm_unreachable("TODO appendable");
}

}}} // end of namespace clang::heavy::builtin

namespace clang { namespace heavy { namespace builtin_syntax {
Value* quote(Context& C, Pair* P) {
  Value* Result = GetSingleSyntaxArg(P);
  if (!Result) {
    C.SetError("Invalid quote syntax", P);
    return C.CreateEmpty();
  }

  return C.CreateQuote(Result);
}

Value* quasiquote(Context& C, Pair* P) {
  Quasiquoter QQ(C);
  return QQ.Run(P);
}

}}} // end of namespace clang::heavy::builtin_syntax

namespace clang { namespace heavy {
Value* syntax_expand(Context& C, Value* V) {
  SyntaxExpander S(C);
  return S.Visit(V);
}

Value* eval(Context& C, Value* V) {
  C.push(V);
  builtin::eval(C, 1);
  return C.pop();
}

void write(llvm::raw_ostream& OS, Value* V) {
  Writer W(OS);
  return W.Visit(V);
}

}} // end namespace clang::heavy

void Context::LoadSystemModule() {
  // Builtin Syntaxes
  AddBuiltinSyntax("quote", builtin_syntax::quote);
  AddBuiltinSyntax("quasiquote", builtin_syntax::quasiquote);

  // Builtin Procedures
  AddBuiltin("+", builtin::operator_add);
  AddBuiltin("*", builtin::operator_mul);
  AddBuiltin("-", builtin::operator_sub);
  AddBuiltin("/", builtin::operator_div);
  AddBuiltin("list", builtin::list);
  AddBuiltin("cons-source", builtin::cons_source);
  AddBuiltin("append", builtin::append);
}
