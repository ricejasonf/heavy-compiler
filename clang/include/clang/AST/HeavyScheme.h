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
  // Value - A result of an evaluation
  class Value {
    enum Kind {
      Boolean,
      Char,
      CppDecl, // C++ decl name
      Empty,
      Integer,
      Float,
      Pair,
      Procedure,
      StringConst,
      StringMutable,
      Symbol,
      Typename, // C++ type
      Vector
    };

    Kind ValueKind;

  protected
    Value (Kind VK)
      : ValueKind(VK)
    { }
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
    static bool classof(const Value* V) {
      return V == Value::StringConst ||
             V == Value::StringMutable;
    }
  };

  class StringConst : String {
    StringRef Val;
    static bool classof(const Value* V) { return V == Value::StringConst; }
  public:
    StringConst(StringRef V)
      : Value(Value::StringConst)
      , Val(V)
    { }
  };

  class StringMutable : String {
    std::string Val;
    static bool classof(const Value* V) { return V == Value::StringMutable; }
  public:
    StringMutable(std::string V)
      : Value(Value::StringMutable)
      , Val(V)
    { }
  }

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

    Boolean* CreateBoolean(bool V) { return new (Ctx) Boolean(V); }
    Char* CreateChar(char V) { return new (Ctx) Char(V); }
    CppDecl* CreateCppDecl(Decl* V) { return new (Ctx) Cppdecl(V); }
    Empty* CreateEmpty() { return new (Ctx) Empty(); }
    Integer* CreateInteger(int V) { return new (Ctx) Integer(V); }
    Float* CreateFloat(float V) { return new (Ctx) Float(V); }
    Pair* CreatePair(Value* V1, Value* V2) { return new (Ctx) Pair(V1, V2); }
    Procedure* Procedure(Value* Pair) { return new (Ctx) Procedure(Pair); }
    StringConst* CreateStringConst(StringRef) {
      return new (Ctx) StringConst(V);
    }
    StringMutable* CreateStringMutable(std::string);
    Symbol* CreateSymbol(IdentifierInfo*) { return new (Ctx) Symbol(II); }
    Typename* CreateTypename(QualType* V)  { return new (Ctx) Typename(V); }
    VectorConst* CreateVectorConst(ArrayRef<Value*> Vs);
  };
} // namespace clang::heavy_scheme

#endif // LLVM_CLANG_AST_HEAVY_SCHEME_H
