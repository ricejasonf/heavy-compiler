// RUN: %clang_cc1 -fsyntax-only -verify -std=heavy %s

heavy_macro foo() = 42; // expected-note {{previous definition is here}}
heavy_macro foo() = 24; // expected-error {{redefinition of heavy macro 'foo'}}

void bar() { } // expected-note {{previous definition is here}}
heavy_macro bar() = 0; // expected-error {{redefinition of 'bar' as different kind of symbol}}

heavy_macro id(x) = x
