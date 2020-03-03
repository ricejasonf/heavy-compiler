//===- DeclHeavy.cpp - Heavy Declaration AST Node Implementation --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the Heavy related Decl classes.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/DeclHeavy.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclBase.h"
#if 0
#include "clang/AST/ASTLambda.h"
#include "clang/AST/ASTMutationListener.h"
#include "clang/AST/ASTUnresolvedSet.h"
#include "clang/AST/Attr.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/LambdaCapture.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/ODRHash.h"
#include "clang/AST/Type.h"
#include "clang/AST/TypeLoc.h"
#include "clang/AST/UnresolvedSet.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/OperatorKinds.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/Specifiers.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#endif
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>

using namespace clang;

HeavyMacroDecl *HeavyMacroDecl::Create(
                            ASTContext &C, DeclContext *DC,
                            DeclarationName DN,
                            SourceLocation StartL,
                            unsigned TemplateDepth,
                            bool IsStatic,
                            bool IsPackOp) {
  HeavyMacroDecl *New =
      new (C, DC) HeavyMacroDecl(DC, DN, StartL);
  return New;
}

// This is for cloning  for use in template instantiations.
HeavyMacroDecl *HeavyMacroDecl::Create(
                            ASTContext &C, DeclContext *DC,
                            HeavyMacroDecl *Old) {
  HeavyMacroDecl *New =
      new (C, DC) HeavyMacroDecl(DC, Old->getDeclName(),
                                     Old->getBeginLoc());
  return New;
}

void HeavyMacroDecl::setParams(ASTContext &C,
                               ArrayRef<HeavyAlias *> NewParamInfo) {
  assert(!ParamInfo && "Already has param info!");
  NumParams = NewParamInfo.size();

  // Zero params -> null pointer.
  if (!NewParamInfo.empty()) {
    ParamInfo = new (C) ParmVarDecl*[NewParamInfo.size()];
    std::copy(NewParamInfo.begin(), NewParamInfo.end(), ParamInfo);
  }
}

