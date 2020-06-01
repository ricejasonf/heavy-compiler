//===--- SemaExprHeavy.cpp - Semantic Analysis for Expressions --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements semantic analysis for Heavy expressions.
///
//===----------------------------------------------------------------------===//

#include "clang/Sema/Template.h"
#include "clang/Sema/SemaInternal.h"
#include "TreeTransform.h"
#include "TypeLocBuilder.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTLambda.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/CharUnits.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprObjC.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeLoc.h"
#include "clang/Basic/AlignedAllocation.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/ScopeInfo.h"
#include "clang/Sema/SemaLambda.h"
#include "clang/Sema/TemplateDeduction.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/ErrorHandling.h"
using namespace clang;
using namespace sema;

namespace {
  // used in ActOnHeavyMacroCallExpr
  HeavyAliasDecl *BuildHeavyMacroParam(Sema &S, HeavyAliasDecl *Old,
                                       Expr *ArgExpr) {
    IdentifierInfo* Id = Old->getDeclName().getAsIdentifierInfo();
    HeavyAliasDecl *New = HeavyAliasDecl::Create(S.Context,
                                                 S.CurContext, Id,
                                                 Old->getLocation());
    New->setBody(ArgExpr);
    return New;
  }
}

ExprResult Sema::ActOnHeavyMacroCallExpr(HeavyMacroIdExpr *Id,
                                         ArrayRef<Expr*> CallArgExprs,
                                         SourceLocation LParenLoc) {
  assert(isa<HeavyMacroIdExpr>(Id) &&
      "Expecting only HeavyMacroIdExpr right now");
  HeavyMacroDecl *D =
    static_cast<HeavyMacroIdExpr*>(Id)->getDefinitionDecl();

  return ActOnHeavyMacroCallExpr(D, CallArgExprs, LParenLoc);
}

ExprResult Sema::ActOnHeavyMacroCallExpr(HeavyMacroDecl* D,
                                         ArrayRef<Expr*> ArgExprs,
                                         SourceLocation Loc) {
  // Defer instantiation if the args are dependent
  // This includes any unexpanded parameter pack
  if (CurContext->isDependentContext() ||
      HeavyMacroCallExpr::hasDependentArgs(ArgExprs)) {
    return HeavyMacroCallExpr::Create(Context, Loc, D,
                                      nullptr, ArgExprs);
  }

  ArrayRef<HeavyAliasDecl*> OldParams = D->parameters();

  Expr *OutputExpr = D->getBody();
  if (!OutputExpr) {
    return ExprError();
  }

  // heavy_macro is a top level declaration only so we
  // do not combine with outer scope
  LocalInstantiationScope Scope(*this, /*CombineWithOuterScope=*/false);
  InstantiatingTemplate Inst(*this, Loc, D);

  // We already know there is at most one param pack
  int PackSize = ArgExprs.size() - OldParams.size() + 1;
  int PackCount = (std::find_if(OldParams.begin(),
                                OldParams.end(),
                                [](HeavyAliasDecl* AD) {
                                  return AD->isParameterPack(); })
                  != OldParams.end()) ? 1 : 0;

  // If PackSize is negative then there must not be a param pack
  // Or the user gave us an invalid arity
  if ((PackCount == 1 && PackSize < 0) ||
      (PackCount == 0 && ArgExprs.size() != OldParams.size())) {
    Diag(Loc, diag::err_heavy_macro_arg_list_different_arity)
      << (((PackCount == 1) || ArgExprs.size() > OldParams.size()) ? 1 : 0);
    Diag(D->getLocation(), diag::note_previous_decl)
      << D->getDeclName();
    return ExprError();
  }

  auto ArgExprsItr = ArgExprs.begin();

  for (HeavyAliasDecl *P : OldParams) {
    if (P->isParameterPack()) {
      assert(PackSize >= 0 && "Pack size is negative!");

      Scope.MakeInstantiatedLocalArgPack(P);
      for (int J = 0; J < PackSize; ++J) {
        assert(ArgExprsItr < ArgExprs.end() && "ArgExprsItr out of range");
        HeavyAliasDecl *New = BuildHeavyMacroParam(*this, P, *ArgExprsItr);
        if (!New) return ExprError();
        Scope.InstantiatedLocalPackArg(P, New);
        ++ArgExprsItr;
      }
    } else {
      assert(ArgExprsItr < ArgExprs.end() && "ArgExprsItr out of range");
      HeavyAliasDecl *New = BuildHeavyMacroParam(*this, P, *ArgExprsItr);
      if (!New) return ExprError();
      Scope.InstantiatedLocal(P, New);
      ++ArgExprsItr;
    }
  }

  TemplateArgumentList Innermost(TemplateArgumentList::OnStack, {});
  MultiLevelTemplateArgumentList TemplateArgs = getTemplateInstantiationArgs(
                                                                D, &Innermost);

  // Instantiate the body
  //
  // Again... note that we keep ArgExprs
  // for use with refactoring/rewrite tools
  ExprResult BodyResult = SubstExpr(OutputExpr, TemplateArgs);

  // wrap the body in ParenExpr for rewriting
  if (BodyResult.isUsable()) {
    BodyResult = ActOnParenExpr(Loc, Loc, BodyResult.getAs<Expr>());
  }

  if (BodyResult.isInvalid()) {
    return ExprError();
  }

  return BuildHeavyMacroCallExpr(Loc, D, BodyResult.getAs<Expr>(),
                                 ArgExprs);
}

ExprResult Sema::BuildHeavyMacroCallExpr(SourceLocation BeginLoc,
                                         HeavyMacroDecl* OrigDecl,
                                         Expr *Body,
                                         ArrayRef<Expr*> Args) {
  // The Body is not instantiated until all args are non-dependent
  return HeavyMacroCallExpr::Create(Context, BeginLoc, OrigDecl, Body, Args);
}
