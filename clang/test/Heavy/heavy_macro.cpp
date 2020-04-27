// RUN: %clang_cc1 -fsyntax-only -verify -verify-ignore-unexpected=note -std=heavy %s

// Declarations

heavy_macro foo() = 42; // expected-note {{previous definition is here}}
heavy_macro foo() = 24; // expected-error {{redefinition of heavy macro 'foo'}}

void bar() { } // expected-note {{previous definition is here}}
heavy_macro bar() = 0; // expected-error {{redefinition of 'bar' as different kind of symbol}}
heavy_macro bar1(); // expected-error {{expected '='}}
heavy_macro bar2(;); // expected-error {{expected identifier}} \
                        expected-error {{unexpected ';' before ')'}}
heavy_macro bar3(...); // expected-error {{expected identifier}}

heavy_macro id(x) = x;
heavy_macro double_name(x, x) = x; // expected-error {{redefinition of parameter 'x'}}
heavy_macro noop(x) = (0);
heavy_macro double_(x) = x + x;
heavy_macro add(x, y) = x + y;
heavy_macro sum(...xs) = (xs + ...);
heavy_macro pack_front(x, ...rest) = x;
heavy_macro pack_back(...rest, x) = x;
heavy_macro pack_middle(x, ...rest, y) = (rest + ...);
heavy_macro lambda(x) = [] { return x; };
heavy_macro capture(x) = [x] { return x; };
heavy_macro capture_value(x) = [=] { return x; };
heavy_macro lazy_sum(...xs) = [xs...] { return (xs + ...); };

// Call Expressions

static_assert(id(5) == 5);
static_assert(5 * add(2, 3) == 25);
static_assert(pack_front(1, 2, 3) == 1);
static_assert(pack_back(1, 2, 3) == 3);
static_assert(pack_middle(1, 2, 3) == 2);

// Instantiating Dependent Call Expressions

template <typename X>
auto id_f(X x) {
  return id(x);
}
static_assert(id_f(42) == 42);

template <typename ...X>
auto sum_f(X ...x) {
  // non-dependent static_assert
  static_assert(sum(1, 2, 3) == 6);
  static_assert(lazy_sum(1, 2, 3)() == 6);
  // dependent static_assert
  static_assert(sum(x...) == 6);
  static_assert(lambda(sum(x...))() == 6);
  static_assert(lazy_sum(x...)() == 6);
  static_assert(capture(lambda(sum(x...))())() == 6);
  return sum(x...);
}
static_assert(sum_f(1, 2, 3) == 6);

auto check_local_capture() {
  int a = 0;
  auto f1 = capture(a); // expected-error {{variable 'a' cannot be implicitly captured in a lambda with no capture-default specified}}
  auto f2 = lambda(a);  // expected-error {{variable 'a' cannot be implicitly captured in a lambda with no capture-default specified}}
  auto f3 = capture_value(a);
}
