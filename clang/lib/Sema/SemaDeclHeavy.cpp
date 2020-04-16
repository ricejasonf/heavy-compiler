//===------ SemaDeclHeavy.cpp - Semantic Analysis for Heavy Declarations ------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic analysis for Heavy declarations.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTLambda.h"
#include "clang/AST/ASTMutationListener.h"
#include "clang/AST/CharUnits.h"
#include "clang/AST/ComparisonCategories.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/AST/ExprHeavy.h"
#include "clang/AST/RecordLayout.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TypeLoc.h"
#include "clang/AST/TypeOrdering.h"
#include "clang/Basic/AttributeCommonInfo.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/LiteralSupport.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/CXXFieldCollector.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/ScopeInfo.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/Sema/Template.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include <map>
#include <set>

using namespace clang;

HeavyMacroDecl *Sema::ActOnHeavyMacroDecl(Scope *S, SourceLocation BeginLoc,
                                          DeclarationName Name) {
  LookupResult Previous(*this, Name, BeginLoc, LookupOrdinaryName,
                        NotForRedeclaration);
  LookupName(Previous, S);
  FilterLookupForScope(Previous, CurContext, S, /*ConsiderLinkage*/false,
                       /*AllowInlineNamespace*/false);

  if (!Previous.empty()) {
    NamedDecl *PreviousDecl = Previous.getRepresentativeDecl();
    if (PreviousDecl->getKind() == Decl::HeavyMacro) {
      Diag(BeginLoc, diag::err_redefinition_of_heavy_macro)
        << Name;
      notePreviousDefinition(PreviousDecl,
                             BeginLoc);
    } else {
      Diag(BeginLoc, diag::err_redefinition_different_kind)
        << Name;
      notePreviousDefinition(PreviousDecl,
                             BeginLoc);
    }
    return nullptr;
  }

  HeavyMacroDecl *
  New = HeavyMacroDecl::Create(Context, CurContext, Name, BeginLoc);
  assert(New && "HeavyMacroDecl::Create failed??");
  PushOnScopeChains(New, S);

  return New;
}

HeavyMacroDecl *Sema::ActOnFinishHeavyMacroDecl(
                          Scope *BodyScope, HeavyMacroDecl* New,
                          const SmallVectorImpl<HeavyAliasDecl*>& ParamInfo,
                          ExprResult BodyResult) {
  // Body

  if (!New || BodyResult.isInvalid())
    return nullptr;

  Expr *Body = BodyResult.getAs<Expr>();

  if (Body->containsUnexpandedParameterPack()) {
    DiagnoseUnexpandedParameterPack(Body, UPPC_Expression);
  }
  New->setBody(Body);

  // Params

  New->setParams(Context, ParamInfo);
  for (auto& P : ParamInfo) {
    assert(P && "HeavyMacro param is nullptr??");
    //PushOnScopeChains(P, BodyScope);
  }
  

  return New;
}
