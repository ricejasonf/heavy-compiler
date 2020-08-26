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
};

class Empty : Value {
  // uhhh nothing?
  static bool classof(const Value* V) { return V == Value::Empty; }
public:
  Empty()
    : Value(Value::Empty)
  { }
};

class Boolean : Value {
  bool Val;
  static bool classof(const Value* V) { return V == Value::Boolean; }
public:
  Boolean(bool V)
    : Value(Value::Boolean)
    , Val(V)
  { }
};

// Base class for Numeric types
class Number : Value {
  // maybe arithmetic functions go here?
  static bool classof(const Value* V) {
    return V == Value::Integer ||
           V == Value::Float;
  }
};

class Integer : Number {
  int Val;
  static bool classof(const Value* V) { return V == Value::Integer; }
public:
  Integer(int V)
    : Value(Value::Integer)
    , Val(V)
  { }
};

class Float : Number {
  float Val;
  static bool classof(const Value* V) { return V == Value::Float; }
public:
  Float(float V)
    : Value(Value::Float)
    , Val(V)
  { }
};

class Char : Value {
  char Val;
  static bool classof(const Value* V) { return V == Value::Char; }
public:
  Char(char V)
    : Value(Value::Char)
    , Val(V)
  { }
};

class Symbol : Value {
  IdentifierInfo* Name;
  static bool classof(const Value* V) { return V == Value::Symbol; }
public:
  Symbol(IdentifierInfo* II)
    : Value(Value::Symbol)
    , Name(II)
  { }
};

class String {
  StringRef Val;
  static bool classof(const Value* V) { return V == Value::String; }
public:
  String(StringRef V)
    : Value(Value::String)
    , Val(V)
  { }
};

class Pair : Value {
  Value* Car;
  Value* Cdr;
  static bool classof(const Value* V) { return V == Value::Pair; }
public:
  Pair(Value* First, Value* Second)
    : Value(Value::Pair)
    , Car(First)
    , Cdr(Second)
  { }
};

class Procedure : Value {
  // Just store the external representation I think
  // or maybe a pointer to code in an interpreter
  // (name . (formals . (body . ()))
  // name    (car x)
  // formals (cadr x)
  // body    (caadr x)
  // ... I think that is right
  Pair Val;
  static bool classof(const Value* V) { return V == Value::Procedure; }
public:
  Procedure(Pair* V)
    : Value(Value::Procedure)
    , Val(V)
  { }
};

class Vector : Value {
  ArrayRef<Value*> Vals;
  static bool classof(const Value* V) { return V == Value::Vector; }
public:
  Vector(ArrayRef<Value*> V)
    : Value(Value::Vector)
    , Val(V)
  { }
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
  ASTContext& Ctx;

public:
  ASTContext& getASTContext() { return Ctx; }

  Boolean* CreateBoolean(bool V) { return new (Ctx) Boolean(V); }
  Char* CreateChar(char V) { return new (Ctx) Char(V); }
  CppDecl* CreateCppDecl(Decl* V) { return new (Ctx) Cppdecl(V); }
  Empty* CreateEmpty() { return new (Ctx) Empty(); }
  Integer* CreateInteger(llvm::APInt V);
  Float* CreateFloat(llvm::APFloat V);
  Pair* CreatePair(Value* V1, Value* V2) { return new (Ctx) Pair(V1, V2); }
  Procedure* CreateProcedure(Value* Pair) {
    return new (Ctx) Procedure(Pair);
  }
  String* CreateString(StringRef V);
  Symbol* CreateSymbol(IdentifierInfo* II) { return new (Ctx) Symbol(II); }
  Typename* CreateTypename(QualType* V) { return new (Ctx) Typename(V); }
  Vector* CreateVector(ArrayRef<Value*> Vs) { return new (Ctx) Vector(V); }

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

#undef DISPATCH
#undef VISIT_FN
};

} // namespace clang::heavy_scheme

#endif // LLVM_CLANG_AST_HEAVY_SCHEME_H
