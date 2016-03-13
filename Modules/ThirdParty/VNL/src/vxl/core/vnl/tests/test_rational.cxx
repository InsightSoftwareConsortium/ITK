#include <iostream>
#include <iomanip>
#include <complex>
#include <vcl_compiler.h>
#include <vnl/vnl_rational.h>
#include <vnl/vnl_rational_traits.h>
#ifdef NEED_COMPLEX_RATIONAL
# include <vnl/vnl_complex.h>
#endif
#include <testlib/testlib_test.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_det.h>
#include <vxl_config.h> // for VXL_INT_64_IS_LONG

inline vnl_rational vnl_sqrt(vnl_rational x) { return vnl_rational(std::sqrt(double(x))); }

static void test_operators()
{
  vnl_rational a(-5L), b(7,-1), c, d(3,7), e(2,0);
  TEST("==", a==-5L, true);
  TEST("==", 5L==-a, true);
  TEST("==", b==-7, true);
  TEST("==", -7==b, true);
  c = a + b; TEST("+", c, -12L);
  c = a - b; TEST("-", c, 2L);
  c = a * b; TEST("*", c, 35L);
  c = a / b; TEST("/", c, vnl_rational(5,7));
  c = c % d; TEST("%", c, vnl_rational(2,7));
  c = a % b; TEST("%", c, -5L);
  c = a % d; TEST("%", c, vnl_rational(-2,7));
  c = d % a; TEST("%", c, d);
  c = a + 5L; TEST("+", c, 0L);
  c = a - 5L; TEST("-", c, -10L);
  c = a * 5L; TEST("*", c, -25L);
  c = a / 5L; TEST("/", c, -1L);
  c = a % 5L; TEST("%", c, 0L);
  c = 5L + a; TEST("+", c, 0L);
  c = 5L - a; TEST("-", c, 10L);
  c = 5L * a; TEST("*", c, -25L);
  c = 5L / a; TEST("/", c, -1L);
  c = 5L % a; TEST("%", c, 0L);
  c = 5 + a; TEST("+", c, 0L);
  c = 5 - a; TEST("-", c, 10L);
  c = 5 * a; TEST("*", c, -25L);
  c = 5 / a; TEST("/", c, -1L);
  c = 5 % a; TEST("%", c, 0L);
  c = a + 5; TEST("+", c, 0L);
  c = a - 5; TEST("-", c, -10L);
  c = a * 5; TEST("*", c, -25L);
  c = a / 5; TEST("/", c, -1L);
  TEST("<", a<d, true);
  TEST("<", a<1L, true);
  TEST("<", a<-4.9, true);
  TEST(">", -b>d, true);
  TEST(">", b>-8, true);
  TEST(">", b>-7.1, true);
  TEST("<=", c<=e, true);
  TEST(">=", b>=-7L, true);
  TEST("<=", 2L<=e, true);
  TEST(">=", 1>=d, true);
  TEST("truncate", truncate(1L+d), 1L);
  TEST("truncate", truncate(-d-1L), -1L);
  TEST("round", round(1L+d), 1L);
  TEST("round", round(-d-1L), -1L);
  TEST("round", round(1L-d), 1L);
  TEST("round", round(d-1L), -1L);
  TEST("floor", floor(1L+d), 1L);
  TEST("floor", floor(-d-1L), -2L);
  TEST("ceil", ceil(1L+d), 2L);
  TEST("ceil", ceil(-d-1L), -1L);
  TEST("abs", vnl_math::abs(d), d);
  TEST("abs", vnl_math::abs(b), -b);
  TEST("sqr mag", vnl_math::squared_magnitude(d), vnl_rational(9,49));
  a += b;
  a -= b;
  a *= b;
  a /= b;
  a %= b;
  std::cout << std::setprecision(20)
           << "a=" << a << '=' << (double)a << '\n'
           << "b=" << b << '=' << (double)b << '\n'
           << "c=" << c << '=' << (double)c << '\n'
           << "d=" << d << '=' << (double)d << '\n'
           << "e=" << e << std::endl; // (double)d ==> floating exception
  d = -7;
  d = -7L;
  std::cout << std::endl;
}

static void test_infinite()
{
  vnl_rational Inf(1,0); ++Inf;
  TEST("Inf+1", Inf.numerator() == 1 && Inf.denominator() == 0, true);
  Inf = -Inf;
  TEST("-Inf", Inf.numerator() == -1 && Inf.denominator() == 0, true);
  TEST("vnl_math::isfinite", vnl_math::isfinite(Inf), false);
  TEST("vnl_math::isnan", vnl_math::isnan(Inf), false);
}

static void test_frac()
{
  vnl_rational r(-15,-20), s(1234321L,-1111111L), p;
  TEST("vnl_math::isfinite", vnl_math::isfinite(r), true);
  TEST("vnl_math::isnan", vnl_math::isnan(r), false);
  TEST("simplify", r.numerator() == 3 && r.denominator() == 4, true);
  TEST("sign in numerator", s.numerator() == -1234321L && s.denominator() == 1111111L, true);
  // All 5-digit numbers below are prime numbers, and small enough so that the multiplications in the constructors do not overflow
  long p1=46309L, p2=46349L, p3=46327L, p4=46337L, p5=46351L;
  r = vnl_rational(p1 * p2, p3 * p4);
  s = vnl_rational(p3 * p4, p1 * p5);
  p = r * s;
  TEST("large multiplication without overflow", p.numerator() == p2 && p.denominator() == p5, true);
  r = vnl_rational(p1 * p2, p3 * p4);
  s = vnl_rational(p1 * p5, p3 * p4);
  p = r * s;
  TEST_NEAR("large multiplication with overflow", p, double(r) * double(s), 1e-12);
  r = vnl_rational(p1 * p2, p3 * p4);
  s = vnl_rational(p1 * p5, p3 * p4);
  p = r / s;
  TEST("large division without overflow", p.numerator() == p2 && p.denominator() == p5, true);
  r = vnl_rational(p1 * p2, p3 * p4);
  s = vnl_rational(p3 * p4, p1 * p5);
  p = r / s;
  TEST_NEAR("large division with overflow", p, double(r) / double(s), 1e-12);
}

#if VXL_INT_64_IS_LONG
static void test_long_64()
{
  long l1 = 1234321234321L, l2 = 2*l1, l3 = 123456787654321L, l4 = l3+1;
  vnl_rational r(-l1,-l2) /* denom = 2*num */, s(l3,-l4) /* relatively prime */, p;
  TEST("vnl_math::isfinite", vnl_math::isfinite(r), true);
  TEST("vnl_math::isnan", vnl_math::isnan(s), false);
  TEST("simplify", r.numerator() == 1 && r.denominator() == 2, true);
  TEST("sign in numerator", s.numerator() == -l3 && s.denominator() == l4, true);
  // The 10-digit numbers below are prime numbers, and small enough so that the multiplications in the constructors do not overflow (at least, on systems where "long" is 64 bit)
  long p1=1999999117L, p2=1999999121L, p3=1999999151L, p4=1999999171L, p5=1999999207L;
  r = vnl_rational(p1 * p2, p3 * p4);
  s = vnl_rational(p4 * p3, p1 * p5);
  p = r * s;
  TEST("large multiplication without overflow", p.numerator() == p2 && p.denominator() == p5, true);
  r = vnl_rational(p1 * p2, p3 * p4);
  s = vnl_rational(p1 * p5, p4 * p3);
  p = r * s;
  TEST_NEAR("large multiplication with overflow", p, double(r) * double(s), 1e-7);
  r = vnl_rational(p1 * p2, p3 * p4);
  s = vnl_rational(p1 * p5, p4 * p3);
  p = r / s;
  TEST("large division without overflow", p.numerator() == p2 && p.denominator() == p5, true);
  r = vnl_rational(p1 * p2, p3 * p4);
  s = vnl_rational(p4 * p3, p1 * p5);
  p = r / s;
  TEST_NEAR("large division with overflow", p, double(r) / double(s), 1e-7);
}
#endif // VXL_INT_64_IS_LONG

static void test_approx()
{
  vnl_rational d ( 1.0/3.0 ); // explicit constructor from double
  TEST("construct from double", d, vnl_rational(1,3));
  d = vnl_rational(-5.0/7);
  TEST("construct from double", d, vnl_rational(-5,7));
  d = vnl_rational(0.42857142857);
  TEST("construct from double", d, vnl_rational(3,7));
  d = vnl_rational(-1.23456);
  TEST("construct from double", d, vnl_rational(-123456,100000));
  vnl_rational pi = vnl_rational(vnl_math::pi);
  double pi_a = double(pi);
  TEST("pi", pi_a-vnl_math::pi < 1e-18 && vnl_math::pi-pi_a < 1e-18, true);
  std::cout << "Best rational approximation of pi: " << pi << " = "
           << pi_a << '\n'
           << "Compare this with pi in 20 decimals:                     "
           << vnl_math::pi << std::endl;
}

static void test_determinant()
{
  vnl_matrix_fixed<vnl_rational,3,3> m;
  m[0][0] = vnl_rational(1,3);
  m[0][1] = vnl_rational(2,7);
  m[0][2] = vnl_rational(2,5);
  m[1][0] = vnl_rational(-1,2);
  m[1][1] = vnl_rational(1,4);
  m[1][2] = vnl_rational(6,7);
  m[2][0] = vnl_rational(2,3);
  m[2][1] = vnl_rational(1,5);
  m[2][2] = vnl_rational(5,2);
  std::cout << "rational matrix:\n" << m
           << "determinant = " << vnl_det(m[0], m[1], m[2]) << std::endl;
  TEST("determinant", vnl_det(m[0], m[1], m[2]), vnl_rational(16609,29400));
}

static void test_sqrt()
{
  vnl_rational d(16,9);
  TEST("sqrt", vnl_sqrt(d), vnl_rational(4,3));
  d = vnl_sqrt(vnl_rational(2L));
  double sqrt2 = std::sqrt(2.0), sqrt_2 = double(d);
  std::cout << "Best rational approximation of sqrt(2): " << d << " = "
           << sqrt_2 << '\n'
           << "Compare this with sqrt(2) in 20 decimals:                     "
           << sqrt2 << std::endl;
  TEST("sqrt(2)", sqrt2-sqrt_2 < 1e-18 && sqrt_2-sqrt2 < 1e-18, true);
}

static void test_zero_one()
{
  vnl_rational n = vnl_numeric_traits<vnl_rational>::zero;
  std::cout << "zero = " << n << '\n';
  TEST("zero", n, 0L);
  vnl_rational u = vnl_numeric_traits<vnl_rational>::one;
  std::cout << "one  = " << u << '\n';
  TEST("one", u, 1L);
}

#ifdef NEED_COMPLEX_RATIONAL // see vnl_complex.h
static void test_complex()
{
  std::complex<vnl_rational> c(0L,1L);
  vnl_rational cc(-1L);
  TEST("complex square", c*c, cc);
  TEST("complex abs", vnl_math::abs(c), 1);
  TEST("complex sqr mag", vnl_math::squared_magnitude(c), 1);
  TEST("complex vnl_math::isfinite", vnl_math::isfinite(c), true);
  TEST("complex vnl_math::isnan", vnl_math::isnan(c), false);
}

static void test_complex_zero_one()
{
  std::complex<vnl_rational> n = vnl_numeric_traits<std::complex<vnl_rational> >::zero;
  std::cout << "zero = " << n << '\n';
  TEST("zero", n, std::complex<vnl_rational>(0L,0L));
  std::complex<vnl_rational> u = vnl_numeric_traits<std::complex<vnl_rational> >::one;
  std::cout << "one  = " << u << '\n';
  TEST("one", u, std::complex<vnl_rational>(1L,0L));
}
#endif // NEED_COMPLEX_RATIONAL

void test_rational()
{
  test_operators();
  test_infinite();
  test_frac();
  test_approx();
  test_determinant();
  test_sqrt();
  test_zero_one();
#ifdef NEED_COMPLEX_RATIONAL // see vnl_complex.h
  test_complex();
  test_complex_zero_one();
#endif
#if VXL_INT_64_IS_LONG
  test_long_64();
#endif
}

TESTMAIN(test_rational);
