//===--- ParseDeclHeavy.cpp - Heavy Declaration Parsing -------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Heavy Declaration portions of the Parser interfaces.
//
//===----------------------------------------------------------------------===//

#include "clang/Parse/Parser.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/PrettyDeclStackTrace.h"
#include "clang/Basic/Attributes.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/SemaDiagnostic.h"
#include "llvm/ADT/SmallString.h"

using namespace clang;

// TODO 
//      - Update calls to Actions when Sema is ready
//

Parser::DeclGroupPtrTy
Parser::ParseHeavyMacroDeclaration(DeclaratorContext Context) {
  SourceLocation BeginLoc;

  // Keyword heavy_macro

  SourceLocation UsingLoc;
  if (!TryConsumeToken(tok::kw_heavy_macro, UsingLoc)) {
    llvm_unreachable("Token was expected to be kw_heavy_macro");
  }

  // Name

  if (Tok.isNot(tok::identifier)) {
    Diag(Tok, diag::err_expected) << tok::identifier;
    SkipUntil(tok::r_paren, StopAtSemi | StopBeforeMatch);
    return;
  }
  DeclarationName Name(Tok.getIdentifierInfo());
  ConsumeToken();

  if (Name.getKind() != UnqualifiedIdKind::IK_Identifier) {
    Diag(Name.getBeginLoc(), diag::err_heavy_macro_name_invalid)
      << FixItHint::CreateRemoval(Name.getSourceRange());
    SkipMalformedDecl();
    return nullptr;
  }

  Scope* S = getCurScope();
  HeavyMacroDecl *New = Actions.ActOnHeavyMacroDecl(S, getCurScope(), Name,
                                                    BeginLoc);

  // Params

  BalancedDelimiterTracker T(*this, tok::l_paren);
  T.consumeOpen();

  // Parse parameter-declaration-clause.
  SmallVector<HeavyAliasDecl, 16> ParamInfo;

  if (!New)
    return nullptr;

  Actions.PushDeclContext(Actions.getCurScope(), New);
  ParseHeavyMacroParamList(New, ParamInfo);
  T.consumeClose();

  // Body

  // Fake the func (scope that is)
  Actions.PushFunctionScope();
  ParseScope BodyScope(this, Scope::FnScope | Scope::DeclScope);

  ExprResult BodyResult = ParseExpression();

  Decl *TheDecl = Actions.ActOnFinishHeavyMacroDecl(New, ParamInfo,
                                                    BodyResult);
  Actions.PopDeclContext();
  BodyScope.Exit();
  Actions.PopFunctionScopeInfo();

  // Semicolon

  ExpectAndConsumeSemi(diag::err_expected_semi_after_expr);

  return Actions.ConvertDeclToDeclGroup(TheDecl);
}

void Parser::ParseHeavyMacroParamList(
    DeclaratorContext DeclaratorCtx,
    SmallVectorImpl<HeavyAliasDecl> &ParamInfo) {
  if (ExpectAndConsume(tok::l_paren)) {
    return nullptr;
  }

  // Maintain an efficient lookup of params we have seen so far.
  llvm::SmallSet<const IdentifierInfo*, 16> ParamsSoFar;

  do {
    // If this isn't an identifier, report the error and skip until ')'.
    if (Tok.isNot(tok::identifier)) {
      Diag(Tok, diag::err_expected) << tok::identifier;
      SkipUntil(tok::r_paren, StopAtSemi | StopBeforeMatch);
      // Forget we parsed anything.
      ParamInfo.clear();
      return;
    }

    IdentifierInfo *ParmII = Tok.getIdentifierInfo();

    // Verify that the argument identifier has not already been mentioned.
    if (!ParamsSoFar.insert(ParmII).second) {
      Diag(Tok, diag::err_param_redefinition) << ParmII;
    } else {
      // TODO call ActOnHeavyAliasDecl so it adds it to scope (I think)
      ParamInfo.push_back(HeavyAliasDecl::create(Actions, DC,
                                                 DeclarationName(ParmII)
                                                 Tok.getLocation()));
    }

    // Eat the identifier.
    ConsumeToken();
    // The list continues if we see a comma.
  } while (TryConsumeToken(tok::comma));

  if (ExpectAndConsume(tok::r_paren)) {
    return nullptr;
  }
}
