//===- DeclHeavy.h - Classes for representing Heavy declarations -=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// Defines the Heavy Decl subclasses
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_DECLHEAVY_H
#define LLVM_CLANG_AST_DECLHEAVY_H

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExternalASTSource.h"
#include "clang/AST/Stmt.h"
#include "llvm/ADT/ArrayRef.h"
#include <cassert>
#include <cstddef>
#include <iterator>
#include <memory>
#include <vector>

namespace clang {

class HeavyAliasDecl : public VarDecl {
  Expr* Body = nullptr;

  HeavyAliasDecl(ASTContext& C, DeclContext* DC, IdentifierInfo *I,
                 TypeSourceInfo *TI, QualType T, SourceLocation SL)
    : VarDecl(HeavyAlias, C, DC, SL, SL, I, T, TI, SC_Auto)
  {}
public:
  static HeavyAliasDecl *Create(ASTContext &C, DeclContext *DC,
                                IdentifierInfo* I, SourceLocation SL,
                                bool IsPack = false);

  void setBody(Expr *S) {
    Body = S;
  }

  Expr* getBody() const {
    return Body;
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return classofKind(D->getKind()); }
  static bool classofKind(Kind K) { return K == HeavyAlias; }
};

class HeavyMacroDecl : public NamedDecl,
                       public DeclContext {
  Expr *Body = nullptr;
  HeavyAliasDecl **ParamInfo = nullptr;
  unsigned NumParams = 0;

  HeavyMacroDecl(DeclContext *DC, DeclarationName DN,
                 SourceLocation StartL)
    : NamedDecl(HeavyMacro, DC, StartL, DN)
    , DeclContext(HeavyMacro) {}

public:
  static HeavyMacroDecl *Create(ASTContext &C, DeclContext *DC,
                                DeclarationName DN, SourceLocation SL);

  static HeavyMacroDecl *Create(ASTContext &C, DeclContext *DC,
                                HeavyMacroDecl* Old);

  void setBody(Expr *E) {
    Body = E;
  }

  Expr* getBody() const {
    return Body;
  }

  void setParams(ASTContext &C, ArrayRef<HeavyAliasDecl *> NewParamInfo);
  unsigned getNumParams() const { return NumParams; }

  // ArrayRef interface to parameters.
  ArrayRef<HeavyAliasDecl *> parameters() const {
    return {ParamInfo, getNumParams()};
  }
  MutableArrayRef<HeavyAliasDecl *> parameters() {
    return {ParamInfo, getNumParams()};
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return classofKind(D->getKind()); }
  static bool classofKind(Kind K) { return K == HeavyMacro; }
};

} // namespace clang

#endif // LLVM_CLANG_AST_DECLHEAVY_H
