#include <vcl_iostream.h>
#include <vcl_iomanip.h>
#include <vnl/vnl_rational.h>
#include <vnl/vnl_rational_traits.h>
#include <vcl_complex.h>
#ifdef NEED_COMPLEX_RATIONAL
# include <vnl/vnl_complex.h>
#endif
#include <testlib/testlib_test.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_det.h>

inline vnl_rational vnl_sqrt(vnl_rational x) { return vnl_rational(vcl_sqrt(double(x))); }

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
  TEST("abs", vnl_math_abs(d), d);
  TEST("abs", vnl_math_abs(b), -b);
  TEST("sqr mag", vnl_math_squared_magnitude(d), vnl_rational(9,49));
  a += b;
  a -= b;
  a *= b;
  a /= b;
  a %= b;
  vcl_cout << vcl_setprecision(20)
           << "a=" << a << "=" << (double)a << vcl_endl
           << "b=" << b << "=" << (double)b << vcl_endl
           << "c=" << c << "=" << (double)c << vcl_endl
           << "d=" << d << "=" << (double)d << vcl_endl
           << "e=" << e << vcl_endl; // (double)d ==> floating exception
  d = -7;
  d = -7L;
  vcl_cout << vcl_endl;
}

static void test_infinite()
{
  vnl_rational Inf(1,0); ++Inf;
  TEST("Inf+1", Inf.numerator() == 1 && Inf.denominator() == 0, true);
  Inf = -Inf;
  TEST("-Inf", Inf.numerator() == -1 && Inf.denominator() == 0, true);
  TEST("vnl_math_isfinite", vnl_math_isfinite(Inf), false);
  TEST("vnl_math_isnan", vnl_math_isnan(Inf), false);
}

static void test_frac()
{
  vnl_rational r(-15,-20);
  TEST("vnl_math_isfinite", vnl_math_isfinite(r), true);
  TEST("vnl_math_isnan", vnl_math_isnan(r), false);
  TEST("simplify", r.numerator() == 3 && r.denominator() == 4, true);
}

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
  vcl_cout << "Best rational approximation of pi: " << pi << " = "
           << pi_a << vcl_endl
           << "Compare this with pi in 20 decimals:                     "
           << vnl_math::pi << vcl_endl;
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
  vcl_cout << "rational matrix:\n" << m
           << "determinant = " << vnl_det(m[0], m[1], m[2]) << vcl_endl;
  TEST("determinant", vnl_det(m[0], m[1], m[2]), vnl_rational(16609,29400));
}

static void test_sqrt()
{
  vnl_rational d(16,9);
  TEST("sqrt", vnl_sqrt(d), vnl_rational(4,3));
  d = vnl_sqrt(vnl_rational(2L));
  double sqrt2 = vcl_sqrt(2.0), sqrt_2 = double(d);
  vcl_cout << "Best rational approximation of sqrt(2): " << d << " = "
           << sqrt_2 << vcl_endl
           << "Compare this with sqrt(2) in 20 decimals:                     "
           << sqrt2 << vcl_endl;
  TEST("sqrt(2)", sqrt2-sqrt_2 < 1e-18 && sqrt_2-sqrt2 < 1e-18, true);
}

static void test_zero_one()
{
  vnl_rational n = vnl_numeric_traits<vnl_rational>::zero;
  vcl_cout << "zero = " << n << '\n';
  TEST("zero", n, 0L);
  vnl_rational u = vnl_numeric_traits<vnl_rational>::one;
  vcl_cout << "one  = " << u << '\n';
  TEST("one", u, 1L);
}

#ifdef NEED_COMPLEX_RATIONAL // see vnl_complex.h
static void test_complex()
{
  vcl_complex<vnl_rational> c(0L,1L);
  vnl_rational cc(-1L);
  TEST("complex square", c*c, cc);
  TEST("complex abs", vnl_math_abs(c), 1);
  TEST("complex sqr mag", vnl_math_squared_magnitude(c), 1);
  TEST("complex vnl_math_isfinite", vnl_math_isfinite(c), true);
  TEST("complex vnl_math_isnan", vnl_math_isnan(c), false);
}

static void test_complex_zero_one()
{
  vcl_complex<vnl_rational> n = vnl_numeric_traits<vcl_complex<vnl_rational> >::zero;
  vcl_cout << "zero = " << n << '\n';
  TEST("zero", n, vcl_complex<vnl_rational>(0L,0L));
  vcl_complex<vnl_rational> u = vnl_numeric_traits<vcl_complex<vnl_rational> >::one;
  vcl_cout << "one  = " << u << '\n';
  TEST("one", u, vcl_complex<vnl_rational>(1L,0L));
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
}

TESTMAIN(test_rational);
