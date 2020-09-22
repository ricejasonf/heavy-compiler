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

namespace clang { namespace heavy_scheme {
class Context;
class Value;
using ValueResult = ActionResult<Value*>;
inline auto ValueError() { return ValueResult(true); }
inline auto ValueEmpty() { return ValueResult(false); }

// The resulting Value* of the eval function
// may be invalidated on a call to garbage
// collection if it is not bound to a variable
// at top level scope
ValueResult eval(Context&, Value*);
void write(raw_ostream&, Value*);

// Value - A result of an evaluation
class Value {
  friend class Context;
public:
  enum class Kind {
    Undefined = 0,
    Boolean,
    Char,
    CppDecl, // C++ decl name
    Empty,
    Integer,
    Float,
    Pair,
    Procedure,
    String,
    Symbol,
    Typename, // C++ type
    Vector,
    Binding,
    ModuleRegion
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
class Number : public Value {
protected:
  Number(Kind K)
    : Value(K)
  { }
public:

  // maybe arithmetic functions go here?
  static bool classof(Value const* V) {
    return V->getKind() == Kind::Integer ||
           V->getKind() == Kind::Float;
  }
};

class Integer : public Number {
  llvm::APInt Val;
public:
  Integer(llvm::APInt V)
    : Number(Kind::Integer)
    , Val(V)
  { }

  auto getVal() { return Val; }
  static bool classof(Value const* V) { return V->getKind() == Kind::Integer; }
};

class Float : public Number {
  llvm::APFloat Val;

public:
  Float(llvm::APFloat V)
    : Number(Kind::Float)
    , Val(V)
  { }

  auto getVal() { return Val; }
  static bool classof(Value const* V) { return V->getKind() == Kind::Float; }
};

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

  static bool classof(Value const* V) { return V->getKind() == Kind::Procedure; }
};

class Vector : public Value {
  ArrayRef<Value*> Vals;

public:
  Vector(ArrayRef<Value*> Vs)
    : Value(Kind::Vector)
    , Vals(Vs)
  { }

  ArrayRef<Value*> getInternal() { return Vals; }
  static bool classof(Value const* V) { return V->getKind() == Kind::Vector; }
};

class Binding : public Value {
  friend class Context;
  // Binding is just a tagged list
  //
  // Possible representation:
  // (symbol value)
  Pair* P;

  Binding(Pair* P)
    : Value(Kind::Binding)
    , P(P)
  { }
public:
  Symbol* getName() {
    return cast<Symbol>(P->Car);
  }

  ValueResult Lookup(Symbol* Name) {
    if (Name->getVal() == getName()->getVal()) {
      return cast<Pair>(P->Cdr)->Car;
    }
    return ValueEmpty();
  }
};

template <typename AllocatorTy>
class ModuleRegion : public Value {
  friend class Context;

  // TODO maybe allow specifying a prefix
  //      for lookup
  llvm::StringMap<Value*, AllocatorTy> Map;

  ModuleRegion(AllocatorTy A)
    : Value(Kind::ModuleRegion)
    , Map(A)
  { }

public:
  ValueResult Lookup(Symbol* Name);
  static bool classof(Value const* V) { return V->getKind() == Kind::ModuleRegion; }
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

class Context {
  using AllocatorTy = llvm::BumpPtrAllocator;
  // TODO eventually we can make our own allocator
  //      that has space for garbage collection
  AllocatorTy TrashHeap;
  Parser* CxxParser = nullptr;

  //bool ProcessFormals(Value* V, BindingRegion* Region, int& Arity);
  //bool AddBinding(Pair* Region, Value* V);
  //BindingRegion* CreateRegion();

public:

  Context(Parser& P)
    : CxxParser(&P)
  { }

  unsigned GetHostIntWidth() const;
  unsigned GetIntWidth() const {
    if (CxxParser) {
      return GetHostIntWidth();
    }
    return sizeof(int) * 8; // ???
  }

  Boolean* CreateBoolean(bool V) { return new (TrashHeap) Boolean(V); }
  Char* CreateChar(char V) { return new (TrashHeap) Char(V); }
  CppDecl* CreateCppDecl(Decl* V) { return new (TrashHeap) CppDecl(V); }
  Empty* CreateEmpty() { return new (TrashHeap) Empty(); }
  Integer* CreateInteger(llvm::APInt V);
  Float* CreateFloat(llvm::APFloat V);
  Pair* CreatePair(Value* V1, Value* V2) { return new (TrashHeap) Pair(V1, V2); }
  String* CreateString(StringRef V);
  Symbol* CreateSymbol(StringRef V) { return new (TrashHeap) Symbol(V); }
  Typename* CreateTypename(QualType QT) {
    return new (TrashHeap) Typename(QT);
  }
  Vector* CreateVector(ArrayRef<Value*> Vs) {
    return new (TrashHeap) Vector(Vs);
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

  Binding* CreateModuleRegion(Symbol* S, Value*);
  Binding* CreateBinding(Symbol* S, Value*);
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

}} // namespace clang::heavy_scheme

#endif // LLVM_CLANG_AST_HEAVY_SCHEME_H
