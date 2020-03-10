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
//      - Add keywords for Heavy specific declarations
//      - Remove compound statement stuff
//      - Remove static stuff
//      - Add ParseHeavyMacroParamList()
//

Parser::DeclGroupPtrTy
Parser::ParseHeavyMacroDeclaration(
                              DeclaratorContext Context,
                              AccessSpecifier AS) {
  if (!getLangOpts().CPlusPlus2a) {
    Diag(Tok.getLocation(),
         diag::warn_cxx2a_compat_heavy_macro_declaration);
  }

  // Declaration Specifiers
  //
  // must be `using` or `static using` for static members
  // Consider adding this to ParseDeclarationSpecifiers.

  SourceLocation BeginLoc;

  // consume kw_using
  SourceLocation UsingLoc;
  if (!TryConsumeToken(tok::kw_using, UsingLoc)) {
    llvm_unreachable("Token was expected to be kw_using");
  }


  // I don't think we need Declarator
  DeclSpec DS(AttrFactory);
  Declarator D(DS, DeclaratorContext::HeavyMacroContext);

  // Name
  //
  // Either an identifier or an operator function id

  CXXScopeSpec Spec;
  UnqualifiedId &Name = D.getName();
  if (ParseUnqualifiedId(
          Spec,
          /*EnteringContext=*/false,
          /*AllowDestructorName=*/false,
          /*AllowConstructorName=*/false,
          /*AllowDeductionGuide=*/false,
          nullptr, nullptr, Name)) {
    return nullptr;
  }

  if (Name.getKind() != UnqualifiedIdKind::IK_Identifier) {
    Diag(Name.getBeginLoc(), diag::err_heavy_macro_name_invalid)
      << FixItHint::CreateRemoval(Name.getSourceRange());
    SkipMalformedDecl();
    return nullptr;
  }

  // Params

  Scope* S = getCurScope();

  // TODO ascertain whether we still need to fake a function scope
  ParseScope PrototypeScope(this,
                            Scope::FunctionPrototypeScope |
                            Scope::FunctionDeclarationScope |
                            Scope::DeclScope);

  BalancedDelimiterTracker T(*this, tok::l_paren);
  T.consumeOpen();

  // Parse parameter-declaration-clause.
  SmallVector<DeclaratorChunk::ParamInfo, 16> ParamInfo;

  HeavyMacroDecl *New = Actions.ActOnHeavyMacroDecl(
                                               S, getCurScope(), AS,
                                               BeginLoc,
                                               TemplateParameterDepth, D,
                                               IsPackOp);

  if (!New)
    return nullptr;

  Actions.PushDeclContext(Actions.getCurScope(), New);
  ParseHeavyMacroParamList(New, ParamInfo);
  T.consumeClose();
  PrototypeScope.Exit();

  // Body

  Actions.PushFunctionScope();
  ParseScope BodyScope(this, Scope::FnScope | Scope::DeclScope |
                             Scope::CompoundStmtScope);
  // TODO parse just an expr (not sure which function to call yet)
  StmtResult CSResult = ParseCompoundStatement();
  Decl *TheDecl = Actions.ActOnFinishHeavyMacroDecl(New, NeedsRAII, CSResult);
  Actions.PopDeclContext();
  BodyScope.Exit();
  Actions.PopFunctionScopeInfo();

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
