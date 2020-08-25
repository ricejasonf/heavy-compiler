//===--- HeavyScheme.cpp - HeavyScheme AST Node Implementation --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Implementations for HeavyScheme AST and Context classes.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/HeavyScheme.h"
#include <algorithm>
#include <cstring>
using namespace clang::heavy_scheme;

String* Context::CreateString(StringRef V) {
  // TODO maybe use TrailingObjects for the string data??

  // Allocate and copy the string data
  char* NewStrData = Ctx.Allocate(V.size());
  std::memcpy(NewStrData, V.data(), V.size());

  return new (Ctx) String(StringRef(NewStrData, V.size()); 
}

Integer* Context::CreateInteger(llvm::APInt Val) {
  //unsigned IntSize = Ctx.getTargetInfo().getIntWidth();
  return new (Ctx) CreateInteger(Val);
}
