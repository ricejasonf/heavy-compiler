//===- HeavyScheme.h - Classes for representing declarations ----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file defines HeavyScheme decalarations for values and evaluation.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_HEAVY_SCHEME_H
#define LLVM_CLANG_AST_HEAVY_SCHEME_H

#include "clang/Basic/SourceLocation.h"
#include "clang/Sema/Ownership.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/TrailingObjects.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <string>
#include <utility>

namespace clang {
class Parser;
}

namespace clang { namespace heavy {
using AllocatorTy = llvm::BumpPtrAllocator;
class Context;
class Value;
using ValueResult = ActionResult<Value*>;
// All builtins should receive a list as args
using ValueFn = void (*)(Context&, int NumArgs);
inline auto ValueError() { return ValueResult(true); }
inline auto ValueEmpty() { return ValueResult(false); }

// The resulting Value* of the eval function
// may be invalidated on a call to garbage
// collection if it is not bound to a variable
// at top level scope
ValueResult eval(Context&, Value* V);
void write(raw_ostream&, Value*);

// Value - A result of an evaluation
class Value {
  friend class Context;
public:
  enum class Kind {
    Undefined = 0,
    Binding,
    Boolean,
    Builtin,
    BuiltinSyntax,
    Char,
    CppDecl, // C++ decl name
    Empty,
    Error,
    Environment,
    Exception,
    Float,
    ForwardRef,
    Integer,
    Module,
    Pair,
    Procedure,
    String,
    Symbol,
    Syntax,
    Typename, // C++ type
    Vector,
  };

private:
  Kind ValueKind;
  bool IsMutable = false;

protected:
  Value (Kind VK)
    : ValueKind(VK)
  { }
public:
  bool isMutable() const { return IsMutable; }
  Kind getKind() const { return ValueKind; }
};

// This type is for internal use only
// specifically for uninitialized bindings
// (didn't want to use NULL and have to
//  check that everywhere)
class Undefined : public Value {
public:
  Undefined()
    : Value(Kind::Undefined)
  { }

  static bool classof(Value const* V) { return V->getKind() == Kind::Undefined; }
};

class Empty : public Value {
public:
  Empty()
    : Value(Kind::Empty)
  { }

  static bool classof(Value const* V) { return V->getKind() == Kind::Empty; }
};

class Error: public Value {
public:
  Value* Message;
  Value* Irritants;

  Error(Value* M, Value* I)
    : Value(Kind::Error)
    , Message(M)
    , Irritants(I)
  { }

  static bool classof(Value const* V) { return V->getKind() == Kind::Error; }
};

// Environment
//  - Represents a Top Level Environment or
//    an Environment Specifier created with (environment ...)
//  - Stacks Modules the bottom of which is the SystemModule.
//  - Only the top module can be mutable
//  - Adding definitions that shadow parent environments
//    is forbidden
class Environment : public Value {
  friend class Context;

  Value* EnvStack;
  bool IsMutable = false;

public:
  Environment(Value* Stack)
    : Value(Kind::Environment)
    , EnvStack(Stack)
  { }

#if 0
  // not used
  Value* Lookup(Symbol* Name) {
    return Context::Lookup(Name, Stack);
  }
#endif

  //Binding* AddDefinition(Symbol* Name, ...
  static bool classof(Value const* V)
  { return V->getKind() == Kind::Environment; }
};

class Exception: public Value {
public:
  Value* Val;
  Exception(Value* Val)
    : Value(Kind::Exception)
    , Val(Val)
  { }

  static bool classof(Value const* V) { return V->getKind() == Kind::Exception; }
};

class Boolean : public Value {
  bool Val;
public:
  Boolean(bool V)
    : Value(Kind::Boolean)
    , Val(V)
  { }

  auto getVal() { return Val; }
  static bool classof(Value const* V) { return V->getKind() == Kind::Boolean; }
};

// Base class for Numeric types
class Float;
class Integer;
class Number : public Value {
protected:
  Number(Kind K)
    : Value(K)
  { }

public:
  static bool classof(Value const* V) {
    return V->getKind() == Kind::Integer ||
           V->getKind() == Kind::Float;
  }

  bool isExact() {
    return getKind() == Kind::Integer;
  }

  bool isExactZero();
  static Value::Kind CommonKind(Number* X, Number* Y);
};

class Integer : public Number {
  friend class Number;
  friend class NumberOp;
  llvm::APInt Val;

public:
  Integer(llvm::APInt V)
    : Number(Kind::Integer)
    , Val(V)
  { }

  auto getVal() { return Val; }

  static bool classof(Value const* V) {
    return V->getKind() == Kind::Integer;
  }
};

class Float : public Number {
  friend class NumberOp;
  llvm::APFloat Val;

public:
  Float(llvm::APFloat V)
    : Number(Kind::Float)
    , Val(V)
  { }

  auto getVal() { return Val; }
  static bool classof(Value const* V) { return V->getKind() == Kind::Float; }
};

inline Value::Kind Number::CommonKind(Number* X, Number* Y) {
  if (isa<Float>(X) || isa<Float>(Y)) {
    return Value::Kind::Float;
  }
  return Value::Kind::Integer;
}
inline bool Number::isExactZero() {
  if (getKind() == Kind::Integer) {
    return cast<Integer>(this)->Val == 0;
  }
  return false;
}

class Char : public Value {
  uint32_t Val;

public:
  Char(char V)
    : Value(Kind::Char)
    , Val(V)
  { }

  auto getVal() { return Val; }
  static bool classof(Value const* V) { return V->getKind() == Kind::Char; }
};

class Symbol : public Value {
  StringRef Val;

public:
  Symbol(StringRef V)
    : Value(Kind::Symbol)
    , Val(V)
  { }

  StringRef getVal() { return Val; }
  static bool classof(Value const* V) { return V->getKind() == Kind::Symbol; }
};

class String : public Value {
public:
  StringRef Val;

  String(StringRef V)
    : Value(Kind::String)
    , Val(V)
  { }

  static bool classof(Value const* V) { return V->getKind() == Kind::String; }
};

class Pair : public Value {
public:
  Value* Car;
  Value* Cdr;

  static bool classof(Value const* V) { return V->getKind() == Kind::Pair; }

  Pair(Value* First, Value* Second)
    : Value(Kind::Pair)
    , Car(First)
    , Cdr(Second)
  { }
};

class Builtin : public Value {
public:
  ValueFn Fn;

  Builtin(ValueFn F)
    : Value(Kind::Builtin)
    , Fn(F)
  { }

  static bool classof(Value const* V) {
    return V->getKind() == Kind::Builtin;
  }
};

class BuiltinSyntax : public Value {
public:
  ValueFn Fn;

  BuiltinSyntax(ValueFn F)
    : Value(Kind::BuiltinSyntax)
    , Fn(F)
  { }

  static bool classof(Value const* V) {
    return V->getKind() == Kind::BuiltinSyntax;
  }
};

class Procedure : public Value {
  Value* Body;
  Value* Bindings;
  unsigned Arity;

public:
  Procedure(Pair* Body, Value* Bindings, unsigned Arity)
    : Value(Kind::Procedure)
    , Body(Body)
    , Bindings(Bindings)
    , Arity(0)
  { }

  Value* getBody() { return Body; }
  Value* getBindings() { return Bindings; }

  static bool classof(Value const* V) {
    return V->getKind() == Kind::Procedure;
  }
};

class Syntax : public Value {
public:
  // TODO ???
  Value* Transformer;
  static bool classof(Value const* V) { return V->getKind() == Kind::Syntax; }
};

class Vector : public Value {
  ArrayRef<Value*> Vals;

public:
  Vector(ArrayRef<Value*> Vs)
    : Value(Kind::Vector)
    , Vals(Vs)
  { }

  ArrayRef<Value*> getElements() const { return Vals; }
  static bool classof(Value const* V) { return V->getKind() == Kind::Vector; }
};

class Binding : public Value {
  friend class Context;
  Symbol* Name;
  Value* Val;

  Binding(Symbol* N, Value* V)
    : Value(Kind::Binding)
    , Name(N)
    , Val(V)
  { }

public:
  Symbol* getName() {
    return Name;
  }

  // TODO not sure if this is needed
  Value* Lookup(Symbol* OtherName) {
    if (Name->getVal() == OtherName->getVal()) {
      return Val;
    }
    return nullptr;
  }

  static bool classof(Value const* V) { return V->getKind() == Kind::Binding; }
};

class Module : public Value {
  friend class Context;

  // TODO An IdentifierTable would probably be
  //      better than using the strings themselves
  //      as keys.
  llvm::StringMap<Value*, AllocatorTy&> Map;

  Module(AllocatorTy& A)
    : Value(Kind::Module)
    , Map(A)
  { }

  Binding* Insert(Binding* B) {
    Map.insert(std::make_pair(B->getName()->getVal(), B));
    return B;
  }
public:
  // Returns nullptr if not found
  Value* Lookup(Symbol* Name) {
    return Map.lookup(Name->getVal());
  }

  static bool classof(Value const* V) { return V->getKind() == Kind::Module; }
};

// ForwardRef - used for garbage collection
class ForwardRef : public Value {
public:
  Value* Val;

  ForwardRef(Value* V)
    : Value(Kind::ForwardRef)
  { }
};

class CppDecl : public Value {
  Decl* Val;
  static bool classof(Value const* V) { return V->getKind() == Kind::CppDecl; }
public:
  CppDecl(Decl* V)
    : Value(Kind::CppDecl)
    , Val(V)
  { }
};

class Typename : public Value {
  QualType Val;
public:
  Typename(QualType V)
    : Value(Kind::Typename)
    , Val(V)
  { }

  static bool classof(Value const* V) { return V->getKind() == Kind::Typename; }
};

class EvaluationStack {
  std::vector<Value*> Storage;
public:
  EvaluationStack(std::size_t Size = 1024)
   : Storage(1, nullptr)
  {
    Storage.reserve(Size);
  }

  void push(Value* V) {
    Storage.push_back(V);
  }

  Value* pop() {
    Value* Back = Storage.back();
    Storage.pop_back();
    return Back;
  }

  void discard(int N = 1) {
    for (int i = 0; i < N; ++i) pop();
  }

  Value* top() {
    return Storage.back();
  }
};

class Context {
  AllocatorTy TrashHeap;

  EvaluationStack EvalStack;
  // EnvStack
  //  - Should be at least one element on top of
  //    an Environment
  //  - Calls to procedures or eval will set the EnvStack
  //    and swap it back upon completion (via RAII)
  Module* SystemModule;
  Value* EnvStack;
  Value* ErrorHandlerStack;
  Value* Err = nullptr;

  //bool ProcessFormals(Value* V, BindingRegion* Region, int& Arity);

  Binding* AddBuiltin(StringRef Str, ValueFn Fn) {
    return SystemModule->Insert(CreateBinding(CreateSymbol(Str),
                                              CreateBuiltin(Fn)));
  }

  Binding* AddBuiltinSyntax(StringRef Str, ValueFn Fn) {
    return SystemModule->Insert(CreateBinding(CreateSymbol(Str),
                                              CreateBuiltinSyntax(Fn)));
  }
public:
  static std::unique_ptr<Context> CreateEmbedded(Parser& P);

  Parser* CxxParser = nullptr;

  Context();

  void Init() { } // TODO Remove
  Module* LoadSystemModule();

  unsigned GetHostIntWidth() const;
  unsigned GetIntWidth() const {
    if (CxxParser) {
      return GetHostIntWidth();
    }
    return sizeof(int) * 8; // ???
  }

  static Binding* Lookup(Symbol* Name, Value* Stack);
  Binding* Lookup(Symbol* Name) {
    return Lookup(Name, EnvStack);
  }

  // Check Error
  //  - Returns true if there is an error or exception
  //  - Builtins will have to check this to stop evaluation
  //    when errors occur
  bool CheckError() {
    return Err ? true : false;
  }

  void SetError(Value* E) {
    assert(isa<Error>(E) || isa<Exception>(E));
    Err = E;
  }

  void SetError(StringRef S, Value* V) {
    SetError(CreateError(S, CreatePair(V)));
  }

  template <typename T>
  T* popArg() {
    T* V = dyn_cast<T>(EvalStack.pop());
    if (V == nullptr) {
      SetError("Contract violation: ???", CreateEmpty());
    }

    return V;
  }

  Boolean*  CreateBoolean(bool V) { return new (TrashHeap) Boolean(V); }
  Char*     CreateChar(char V) { return new (TrashHeap) Char(V); }
  CppDecl*  CreateCppDecl(Decl* V) { return new (TrashHeap) CppDecl(V); }
  Empty*    CreateEmpty() { return new (TrashHeap) Empty(); }
  Integer*  CreateInteger(llvm::APInt V);
  Float*    CreateFloat(llvm::APFloat V);
  Pair*     CreatePair(Value* V1, Value* V2) {
    return new (TrashHeap) Pair(V1, V2);
  }
  Pair*     CreatePair(Value* V1) {
    return new (TrashHeap) Pair(V1, CreateEmpty());
  }
  String*   CreateString(StringRef V);
  Symbol*   CreateSymbol(StringRef V) { return new (TrashHeap) Symbol(V); }
  Vector*   CreateVector(ArrayRef<Value*> Xs);
  Environment* CreateEnvironment() {
    return new (TrashHeap) Environment(CreatePair(SystemModule));
  }
  Typename* CreateTypename(QualType QT) {
    return new (TrashHeap) Typename(QT);
  }

  String* CreateMutableString(StringRef V) {
    String* New = CreateString(V);
    New->IsMutable = true;
    return New;
  }

  Vector* CreateMutableVector(ArrayRef<Value*> Vs) {
    Vector* New = CreateVector(Vs);
    New->IsMutable = true;
    return New;
  }

  Builtin* CreateBuiltin(ValueFn Fn) {
    return new (TrashHeap) Builtin(Fn);
  }
  BuiltinSyntax* CreateBuiltinSyntax(ValueFn Fn) {
    return new (TrashHeap) BuiltinSyntax(Fn);
  }

  Error* CreateError(Value* Message, Pair* Irritants) {
    return new (TrashHeap) Error(Message, Irritants);
  }

  Error* CreateError(StringRef S, Pair* P) {
    return CreateError(CreateString(S), P);
  }

  Exception* CreateException(Value* V) {
    return new (TrashHeap) Exception(V);
  }

  Module*  CreateModule();
  Binding* CreateBinding(Symbol* S, Value* V) {
    return new (TrashHeap) Binding(S, V);
  }
};

// ValueVisitor
// This will be the base class for evaluation and printing
template <typename Derived, typename RetTy = void>
class ValueVisitor {
#define DISPATCH(NAME) \
  return getDerived().Visit ## NAME(static_cast<NAME*>(V))
#define VISIT_FN(NAME) \
  template <typename T> \
  RetTy Visit ## NAME(T* V) { return getDerived().VisitValue(V); }

  Derived& getDerived() { return static_cast<Derived&>(*this); }
  Derived const& getDerived() const { return static_cast<Derived>(*this); }

protected:
  // Derived must implement VisitValue OR all of the
  // concrete visitors
  template <typename T>
  RetTy VisitValue(T* V) = delete;

  // The default implementations for visiting
  // nodes is to call Derived::VisitValue
  VISIT_FN(Boolean)
  VISIT_FN(Char)
  VISIT_FN(CppDecl)
  VISIT_FN(Empty)
  VISIT_FN(Integer)
  VISIT_FN(Float)
  VISIT_FN(Pair)
  VISIT_FN(Procedure)
  VISIT_FN(String)
  VISIT_FN(Symbol)
  VISIT_FN(Typename)
  VISIT_FN(Vector)

public:
  RetTy Visit(Value* V) {
    switch (V->getKind()) {
    case Value::Kind::Boolean:    DISPATCH(Boolean);
    case Value::Kind::Char:       DISPATCH(Char);
    case Value::Kind::CppDecl:    DISPATCH(CppDecl);
    case Value::Kind::Empty:      DISPATCH(Empty);
    case Value::Kind::Integer:    DISPATCH(Integer);
    case Value::Kind::Float:      DISPATCH(Float);
    case Value::Kind::Pair:       DISPATCH(Pair);
    case Value::Kind::Procedure:  DISPATCH(Procedure);
    case Value::Kind::String:     DISPATCH(String);
    case Value::Kind::Symbol:     DISPATCH(Symbol);
    case Value::Kind::Typename:   DISPATCH(Typename);
    case Value::Kind::Vector:     DISPATCH(Vector);
    default:
      llvm_unreachable("Invalid Value Kind");
    }
  }

#undef DISPATCH
#undef VISIT_FN
};

}} // namespace clang::heavy

#endif // LLVM_CLANG_AST_HEAVY_SCHEME_H
