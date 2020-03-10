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

// HeavyMacroDecl
class HeavyMacroDecl : public NamedDecl,
                       public DeclContext {
  Expr *Body = nullptr;
  HeavyAlias **ParamInfo = nullptr;
  unsigned NumParams = 0;

  HeavyMacroDecl(DeclContext *DC, DeclarationName DN,
                 SourceLocation StartL)
    : NamedDecl(HeavyMacro, DC, StartL, DN)
    , DeclContext(HeavyMacro) {}

public:
  static HeavyMacroDecl *Create(ASTContext &C, DeclContext *DC,
                                DeclarationName DN);

  static HeavyMacroDecl *Create(ASTContext &C, DeclContext *DC,
                                HeavyMacroDecl* Old);

  void setBody(Stmt *S) {
    Body = S;
  }

  Stmt* getBody() const {
    return Body;
  }

  void setParams(ASTContext &C, ArrayRef<HeavyAlias *> NewParamInfo);
  unsigned getNumParams() const { return NumParams; }

  // ArrayRef interface to parameters.
  ArrayRef<ParmVarDecl *> parameters() const {
    return {ParamInfo, getNumParams()};
  }
  MutableArrayRef<ParmVarDecl *> parameters() {
    return {ParamInfo, getNumParams()};
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return classofKind(D->getKind()); }
  static bool classofKind(Kind K) { return K == HeavyMacro; }
};

class HeavyAliasDecl : public NamedDecl {
  Expr* Body;

  HeavyAliasDecl(DeclContext* DC, DeclarationName DN, SourceLocation SL
    : NamedDecl(HeavyAlias, DC, SL, DN)
  {}
public:
  static HeavyMacroDecl *Create(ASTContext &C, DeclContext *DC,
                                DeclarationName DN);

  void setBody(Expr *S) {
    Expr = S;
  }

  Expr* getBody() const {
    return Body;
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return classofKind(D->getKind()); }
  static bool classofKind(Kind K) { return K == HeavyAlias; }
}

} // namespace clang

#endif // LLVM_CLANG_AST_DECLHEAVY_H
