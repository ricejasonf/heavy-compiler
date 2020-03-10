//===- ExprHeavy.cpp - (Heavy) Expression AST Node Implementation -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the subclesses of Expr class declared in ExprHeavy.h
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ExprHeavy.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclAccessPair.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclHeavy.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Expr.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/Type.h"
#include "clang/AST/TypeLoc.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/Specifiers.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include <cassert>
#include <cstddef>
#include <cstring>
#include <memory>

using namespace clang;

HeavyMacroCallExpr*
HeavyMacroCallExpr::Create(
                    ASTContext &C, SourceLocation BL,
                    HeavyMacroDecl* D
                    Expr* Body,
                    QualType QT, ExprValueKind VK,
                    ArrayRef<HeavyAlias*> Args) {
  HeavyMacroCallExpr* New = new (C) HeavyMacroCallExpr(BL, QT, VK);
  New->NumArgs = Args.size();

  if (!Args.empty()) {
    New->ArgInfo = new (C) HeavyAlias*[Args.size()];
    std::copy(Args.begin(), Args.end(), New->ArgInfo);
  }

  New->Args = new (C) Expr*[Args.size()];

  New->Body = Body;
  for (unsigned I = 0; I < Args.size(); ++I) {
    New->Args[I] = Args[I]->getInit();
    if (New->Args[I]->containsUnexpandedParameterPack())
      New->setContainsUnexpandedParameterPack(true);
  }

  return New;
}

HeavyMacroCallExpr*
HeavyMacroCallExpr::CreateDependent(
                    ASTContext &C, SourceLocation BL,
                    HeavyMacroDecl* D,
                    ArrayRef<Expr*> CallArgs) {
  HeavyMacroCallExpr*
  New = new (C) HeavyMacroCallExpr(BL, C.DependentTy, D,
                                   CallArgs);

  for (unsigned I = 0; I < CallArgs.size(); ++I) {
    if (CallArgs[I]->isTypeDependent())
      New->setTypeDependent(true);
    if (CallArgs[I]->isValueDependent())
      New->setValueDependent(true);
    if (CallArgs[I]->isInstantiationDependent())
      New->setInstantiationDependent(true);
    if (CallArgs[I]->containsUnexpandedParameterPack())
      New->setContainsUnexpandedParameterPack(true);
  }

  return New;
}

bool HeavyMacroCallExpr::hasDependentArgs(ArrayRef<Expr*> Args) {
  for (unsigned I = 0; I < Args.size(); ++I) {
    if (Args[I]->isTypeDependent() ||
        Args[I]->isValueDependent() ||
        Args[I]->isInstantiationDependent() ||
        Args[I]->containsUnexpandedParameterPack())
      return true;
  }

  return false;
}