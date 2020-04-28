// RUN: %clang_cc1 -fsyntax-only -verify -verify-ignore-unexpected=note -std=heavy %s

heavy_macro add(x, y) = x + y;

struct not_addable_type { };

template <typename T>
auto check_sfinae(T t) -> decltype(add(t, 5)) { return add(t, 5); };

void run_check_sfinae() {
  auto a = check_sfinae(2);
  auto b = check_sfinae(not_addable_type{}); // expected-error {{no matching function for call to 'check_sfinae'}}
}
