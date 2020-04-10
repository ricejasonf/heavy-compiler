//===- ExprCXX.h - Classes for representing expressions ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// Defines the clang::Expr interface and subclasses for C++ expressions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_EXPRHEAVY_H
#define LLVM_CLANG_AST_EXPRHEAVY_H

#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclHeavy.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Expr.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/TemplateBase.h"
#include "clang/AST/Type.h"
#include "clang/AST/UnresolvedSet.h"
#include "clang/Basic/ExceptionSpecificationType.h"
#include "clang/Basic/ExpressionTraits.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/Lambda.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/OperatorKinds.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/Specifiers.h"
#include "clang/Basic/TypeTraits.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/TrailingObjects.h"
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>

namespace clang {

class ASTContext;

//===--------------------------------------------------------------------===//
// Heavy Expressions.
//===--------------------------------------------------------------------===//

// HeavyMacroIdExpr - A placeholder for the identifier when invoking
//                              a HeavyMacro
class HeavyMacroIdExpr : public Expr {
  SourceLocation BeginLoc;
  HeavyMacroDecl *DefinitionDecl;

  HeavyMacroIdExpr(SourceLocation BL, QualType QT,
                   HeavyMacroDecl *D)
    : Expr(HeavyMacroIdExprClass, QT, VK_RValue, OK_Ordinary,
           false, false, false, false),
      BeginLoc(BL),
      DefinitionDecl(D) {}

public:
  static HeavyMacroIdExpr *Create(ASTContext &C, SourceLocation BL,
                                            HeavyMacroDecl *D) {
    return new (C) HeavyMacroIdExpr(BL, C.HeavyMacroIdTy, D);
  }

  HeavyMacroDecl *getDefinitionDecl() { return DefinitionDecl; }

  // Iterators
  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  SourceLocation getBeginLoc() const LLVM_READONLY { return BeginLoc; }
  SourceLocation getEndLoc() const LLVM_READONLY { return BeginLoc; }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == HeavyMacroIdExprClass;
  }
};

// HeavyMacroCallExpr - A compound statement with RAII scope that
//                                evaluates as an expression based on its
//                                return value
//
class HeavyMacroCallExpr : public Expr {
  SourceLocation BeginLoc;
  HeavyMacroDecl* DefinitionDecl;
  Expr** ArgInfo;
  Expr* Body;
  unsigned NumArgs = 0;

  HeavyMacroCallExpr(SourceLocation BL, HeavyMacroDecl *D,
                     QualType QT, ExprValueKind VK)
    : Expr(HeavyMacroCallExprClass, QT, VK, OK_Ordinary,
           false, false, false, false),
      BeginLoc(BL),
      DefinitionDecl(D) {}
public:
  static HeavyMacroCallExpr *Create(
                  ASTContext &C, SourceLocation BL,
                  HeavyMacroDecl* D,
                  Expr *Body,
                  ArrayRef<Expr*> Args);

  static bool hasDependentArgs(ArrayRef<Expr*> Args);
  HeavyMacroDecl *getDefinitionDecl() { return DefinitionDecl; }

  Expr* getBody() const { return Body; }

  unsigned getNumArgs() const { return NumArgs; }

  ArrayRef<Expr *> getArgs() const {
    return {ArgInfo, NumArgs};
  }
  MutableArrayRef<Expr *> getArgs() {
    return {ArgInfo, NumArgs};
  }

  // Iterators
  child_range children() {
    return child_range(reinterpret_cast<Stmt**>(Body),
                       reinterpret_cast<Stmt**>(Body) + 1);
  }

  SourceLocation getBeginLoc() const LLVM_READONLY { return BeginLoc; }
  SourceLocation getEndLoc() const LLVM_READONLY { return BeginLoc; }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == HeavyMacroCallExprClass;
  }
};

// HeavyAliasIdExpr - The identifier that names an alias to be substituted
//
class HeavyAliasIdExpr : public Expr {
  SourceLocation BeginLoc;
  HeavyAliasDecl *DefinitionDecl;

  HeavyAliasIdExpr(SourceLocation BL, QualType QT,
                   HeavyAliasDecl *D)
    : Expr(HeavyAliasIdExprClass, QT, VK_RValue, OK_Ordinary,
           false, false, false, false),
      BeginLoc(BL),
      DefinitionDecl(D) {}

public:
  static HeavyAliasIdExpr *Create(ASTContext &C, SourceLocation BL,
                                            HeavyAliasDecl *D) {
    return new (C) HeavyAliasIdExpr(BL, C.HeavyAliasIdTy, D);
  }

  HeavyAliasDecl *getDefinitionDecl() { return DefinitionDecl; }

  Expr *getBody() const {
    return DefinitionDecl->getBody();
  };

  // Iterators
  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  SourceLocation getBeginLoc() const LLVM_READONLY { return BeginLoc; }
  SourceLocation getEndLoc() const LLVM_READONLY { return BeginLoc; }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == HeavyAliasIdExprClass;
  }
};

} // namespace clang

#endif // LLVM_CLANG_AST_EXPRHEAVY_H
