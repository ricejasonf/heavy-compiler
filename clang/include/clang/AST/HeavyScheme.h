//===- Decl.h - Classes for representing declarations -----------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Decl subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_HEAVY_SCHEME_H
#define LLVM_CLANG_AST_HEAVY_SCHEME_H

#include "clang/AST/ASTContextAllocate.h"
#include "clang/AST/Type.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Sema/Ownership.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
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

namespace clang::heavy_scheme {
class Context;
class Value;

// The resulting Value* of the eval function
// may be invalidated on a call to garbage
// collection if it is not bound to a variable
// at top level scope
Value* eval(Context&, Value*);
void write(raw_ostream&, Value*);

// Value - A result of an evaluation
class Value {
  friend class Context;
public:
  enum Kind {
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
    Vector
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
  bool is(Kind K) const { return getKind() == K; }
};

class Empty : Value {
  // uhhh nothing?
  static bool classof(const Value* V) { return V == Value::Empty; }
public:
  Empty()
    : Value(Value::Empty)
  { }
};

class Boolean : public Value {
  bool Val;
public:
  Boolean(bool V)
    : Value(Value::Boolean)
    , Val(V)
  { }

  auto getVal() { return Val; }
  static bool classof(const Value* V) { return V == Value::Boolean; }
};

// Base class for Numeric types
class Number : public Value {
public:
  // maybe arithmetic functions go here?
  static bool classof(const Value* V) {
    return V == Value::Integer ||
           V == Value::Float;
  }
};

class Integer : public Number {
  llvm::APInt Val;
public:
  Integer(int V)
    : Value(Value::Integer)
    , Val(V)
  { }

  auto getVal() { return Val; }
  static bool classof(const Value* V) { return V == Value::Integer; }
};

class Float : public Number {
  llvm::APFloat Val;

public:
  Float(float V)
    : Value(Value::Float)
    , Val(V)
  { }

  auto getVal() { return Val; }
  static bool classof(const Value* V) { return V == Value::Float; }
};

class Char : public Value {
  uint32_t Val;

public:
  Char(char V)
    : Value(Value::Char)
    , Val(V)
  { }

  auto getVal() { return Val; }
  static bool classof(const Value* V) { return V == Value::Char; }
};

class Symbol : public Value {
  IdentifierInfo* Name;

public:
  Symbol(IdentifierInfo* II)
    : Value(Value::Symbol)
    , Name(II)
  { }

  IdentifierInfo* getIdentifier() { return Name; }
  static bool classof(const Value* V) { return V == Value::Symbol; }
};

class String : public Value {
public:
  StringRef Val;
  static bool classof(const Value* V) { return V == Value::String; }

  String(StringRef V)
    : Value(Value::String)
    , Val(V)
  { }
};

class Pair : public Value {
public:
  Value* Car;
  Value* Cdr;

  static bool classof(const Value* V) { return V == Value::Pair; }

  Pair(Value* First, Value* Second)
    : Value(Value::Pair)
    , Car(First)
    , Cdr(Second)
  { }
};

class Procedure : public Value {
  Pair* Val;

public:
  // Just store the external representation I think
  // (name . (formals . (body . ()))
  // name    (car x)
  // formals (cadr x)
  // body    (caadr x)
  // ... I think that is right

  Procedure(Pair* V)
    : Value(Value::Procedure)
    , Val(V)
  { }

  Pair* getVal() { return Val; }
  static bool classof(const Value* V) { return V == Value::Procedure; }
};

class Vector : public Value {
  ArrayRef<Value*> Vals;

public:
  Vector(ArrayRef<Value*> V)
    : Value(Value::Vector)
    , Val(V)
  { }

  ArrayRef<Value*> getInternal() { return Vals; }
  static bool classof(const Value* V) { return V == Value::Vector; }
};

class CppDecl : Value {
  Decl* Val;
  static bool classof(const Value* V) { return V == Value::CppDecl; }
public:
  CppDecl(Decl* V)
    : Value(Value::CppDecl)
    , Val(V)
  { }
};

class Typename : Value {
  QualType Val;
  static bool classof(const Value* V) { return V == Value::Typename; }
public:
  Typename(QualType V)
    : Value(Value::Typename)
    , Val(V)
  { }
};

class Context {
  ASTContext& ASTCtx;

public:
  // TODO Eventually we want to hide how we
  //      allocate memory so we can add garbage
  //      collection.
  ASTContext& getASTContext() { return ASTCtx; }

  Boolean* CreateBoolean(bool V) { return new (ASTCtx) Boolean(V); }
  Char* CreateChar(char V) { return new (ASTCtx) Char(V); }
  CppDecl* CreateCppDecl(Decl* V) { return new (ASTCtx) Cppdecl(V); }
  Empty* CreateEmpty() { return new (ASTCtx) Empty(); }
  Integer* CreateInteger(llvm::APInt V);
  Float* CreateFloat(llvm::APFloat V);
  Pair* CreatePair(Value* V1, Value* V2) { return new (ASTCtx) Pair(V1, V2); }
  Procedure* CreateProcedure(Value* Pair) {
    return new (ASTCtx) Procedure(Pair);
  }
  String* CreateString(StringRef V);
  Symbol* CreateSymbol(IdentifierInfo* II) { return new (ASTCtx) Symbol(II); }
  Typename* CreateTypename(QualType* V) { return new (ASTCtx) Typename(V); }
  Vector* CreateVector(ArrayRef<Value*> Vs) { return new (ASTCtx) Vector(V); }

  String* CreateMutableString(StringRef V) {
    String* S = CreateString(V);
    New.IsMutable = true;
    return S;
  }

  Vector* CreateMutableVector(ArrayRef<Value*> Vs) {
    Vector* New = CreateVector(Vs);
    New.IsMutable = true;
    return New;
  }
};

using ValueResult = ActionResult<Value *>;

inline ValueError() { return ValueResult(true); }
inline ValueEmpty() { return ValueResult(false); }

// ValueVisitor
// This will be the base class for evaluation and printing
template <typename Derived, typename RetTy = void>
class ValueVisitor {
#define DISPATCH(NAME) \
  return getDerived().Visit ## NAME(static_cast<NAME*>(V))
#define VISIT_FN(NAME) \
  template <typename T> \
  RetTy Visit ## NAME(T* V) { return getDerived().VisitValue(V); }

  Derived& getDerived() { return static_cast<Derived>(*this); }
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
    case Value::Boolean:    DISPATCH(Boolean);
    case Value::Char:       DISPATCH(Char);
    case Value::CppDecl:    DISPATCH(CppDecl);
    case Value::Empty:      DISPATCH(Empty);
    case Value::Integer:    DISPATCH(Integer);
    case Value::Float:      DISPATCH(Float);
    case Value::Pair:       DISPATCH(Pair);
    case Value::Procedure:  DISPATCH(Procedure);
    case Value::String:     DISPATCH(String);
    case Value::Symbol:     DISPATCH(Symbol);
    case Value::Typename:   DISPATCH(Typename);
    case Value::Vector:     DISPATCH(Vector);
    }
  }

#undef DISPATCH
#undef VISIT_FN
};

} // namespace clang::heavy_scheme

#endif // LLVM_CLANG_AST_HEAVY_SCHEME_H
