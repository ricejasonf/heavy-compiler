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
#include "clang/AST/Decl.h"
#include "clang/AST/PrettyDeclStackTrace.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/SemaDiagnostic.h"
#include "llvm/ADT/SmallString.h"

using namespace clang;

Decl*
Parser::ParseHeavyMacroDeclaration(DeclaratorContext Context) {
  SourceLocation BeginLoc;

  // Keyword heavy_macro

  if (!TryConsumeToken(tok::kw_heavy_macro, BeginLoc)) {
    llvm_unreachable("Token was expected to be kw_heavy_macro");
  }

  // Name

  if (Tok.isNot(tok::identifier)) {
    Diag(Tok, diag::err_expected) << tok::identifier;
    //SkipUntil(tok::r_paren, StopAtSemi | StopBeforeMatch);
    SkipUntil(tok::semi);
    return nullptr;
  }
  DeclarationName Name(Tok.getIdentifierInfo());
  ConsumeToken();

  HeavyMacroDecl *New = Actions.ActOnHeavyMacroDecl(getCurScope(),
                                                    BeginLoc, Name);

  // Params

  // Parse parameter-declaration-clause.
  SmallVector<HeavyAliasDecl*, 16> ParamInfo;

  if (!New) {
    SkipUntil(tok::semi);
    return nullptr;
  }

  ParseScope PrototypeScope(this, Scope::DeclScope);

  Actions.PushDeclContext(Actions.getCurScope(), New);
  ParseHeavyMacroParamList(ParamInfo);
  PrototypeScope.Exit();

  // Body

  if (ExpectAndConsume(tok::equal)) {
    Actions.PopDeclContext();
    SkipUntil(tok::semi);
    return nullptr;
  }

  // Fake the func (scope that is)
  Actions.PushFunctionScope();
  ParseScope BodyScope(this, Scope::FnScope | Scope::DeclScope);

  ExprResult BodyResult = ParseExpression();

  Decl *TheDecl = Actions.ActOnFinishHeavyMacroDecl(getCurScope(), New, ParamInfo,
                                                    BodyResult);
  BodyScope.Exit();
  Actions.PopFunctionScopeInfo();
  Actions.PopDeclContext();

  // Semicolon

  ExpectAndConsumeSemi(diag::err_expected_semi_after_expr);

  return TheDecl;
}

void Parser::ParseHeavyMacroParamList(
    SmallVectorImpl<HeavyAliasDecl*> &ParamInfo) {
  if (ExpectAndConsume(tok::l_paren)) {
    return;
  }

  BalancedDelimiterTracker T(*this, tok::l_paren);
  T.consumeOpen();

  int PackCount = 0;

  // Maintain an efficient lookup of params we have seen so far.
  llvm::SmallSet<const IdentifierInfo*, 16> ParamsSoFar;

  do {
    bool IsPack = false;

    if (Tok.is(tok::r_paren)) {
      T.consumeClose();
      return;
    }

    if (Tok.is(tok::ellipsis)) {
      IsPack = true;
      ++PackCount;
      ConsumeToken();
    }

    // If this isn't an identifier, report the error and skip until ')'.
    if (Tok.isNot(tok::identifier)) {
      Diag(Tok, diag::err_expected) << tok::identifier;
      SkipUntil(tok::r_paren, StopAtSemi | StopBeforeMatch);
      // Forget we parsed anything.
      ParamInfo.clear();
      T.consumeClose();
      return;
    }

    IdentifierInfo *ParmII = Tok.getIdentifierInfo();

    if (PackCount > 1) {
      Diag(Tok, diag::err_heavy_macro_multiple_parameter_packs) << ParmII;
    }

    // Verify that the argument identifier has not already been mentioned.
    if (!ParamsSoFar.insert(ParmII).second) {
      Diag(Tok, diag::err_param_redefinition) << ParmII;
    } else {
      // TODO call ActOnHeavyAliasDecl so it adds it to scope (I think)
      ParamInfo.push_back(HeavyAliasDecl::Create(Actions.getASTContext(), 
                                                 Actions.CurContext,
                                                 ParmII,
                                                 Tok.getLocation(),
                                                 IsPack));
    }

    // Eat the identifier.
    ConsumeToken();
    // The list continues if we see a comma.
  } while (TryConsumeToken(tok::comma));

  if (ExpectAndConsume(tok::r_paren)) {
    T.consumeClose();
    return;
  }
  T.consumeClose();
}
