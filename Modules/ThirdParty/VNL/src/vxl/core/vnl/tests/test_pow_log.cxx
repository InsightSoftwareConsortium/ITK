#include <testlib/testlib_test.h>
// And explicitly does *not* #include <cmath>,
// since the standard std::pow() functions are wittingly not used.

// A recursive implementation for a^b with a and b both integer;
// this is a more accurate alternative for std::pow(double a,double b),
// certainly in those cases where b is relatively small.
// Negative exponents make of course no sense since the result must be int.
// Beware of overflow!
inline static int int_pow(int a, unsigned int b)
{
  if (b==0) return 1;
  else if (b==1) return a;
  else
    {
    long asquare = static_cast<long>(a)*static_cast<long>(a);
    long r1      = static_cast<long>(int_pow(static_cast<int>(asquare),b/2));
    long r2      = static_cast<long>(int_pow(a, b%2));
    return static_cast<int>(r1*r2);
    }
}

// A recursive implementation for a^b with a double and b integer;
// this is a more accurate alternative for std::pow(double a,double b),
// certainly in those cases where b is relatively small.
inline static double int_pow(double a, int b)
{
  if (b==0) return 1;
  else if (b==1) return a;
  else if (b<0) return int_pow(1.0/a, -b);
  else return int_pow(a*a,b/2) * int_pow(a, b%2);
}

// An implementation for floor(log(a)/log(8)) with integer argument a;
// this is a more straightforward (and not too inefficient) alternative for
//  std::floor(std::log(double a)/std::log(8.0)).
// Negative arguments make of course no sense; strictly speaking, also a=0
// makes no sense, but in that case a "very negative" value is returned.
inline static int log8(unsigned int a)
{
  if (a==0) return -0x7fffffffL-1L; // stands for minus infinity
  int r = 0;
  while (a >= 8) ++r, a>>=3; // divide by 8
  return r;
}

static void test_int_pow()
{
  TEST("Exponent 0", int_pow(-33, 0U), 1);
  TEST("Exponent 1", int_pow(-33, 1U), -33);
  TEST("Exponent of 1", int_pow(1, 12345U), 1);
  TEST("Even exponent of -1", int_pow(-1, 12346U), 1);
  TEST("Odd exponent of -1", int_pow(-1, 12345U), -1);
  TEST("Large power of 2", int_pow(2, 30U), 0x40000000);
#ifdef TEST_SIGNED_OVERFLOW // "signed overflow" might give compiler warnings or even worse ...
  TEST("Power of 2 with overflow", int_pow(2, 31U), (int)(-0x80000000L));
  TEST("Power of 2 with even more overflow", int_pow(2, 32U), 0);
#endif // TEST_SIGNED_OVERFLOW
  TEST("Just a \"random\" case...", int_pow(-19, 7U), -893871739);
}

static void test_dbl_pow()
{
  TEST("Exponent 0", int_pow(-33.3, 0), 1.0);
  TEST("Exponent 1", int_pow(-33.3, 1), -33.3);
  TEST("Exponent -1", int_pow(0.4, -1), 2.5);
  TEST("Even exponent of -1", int_pow(-1.0, 12346), 1.0);
  TEST("Odd exponent of -1", int_pow(-1.0, 12345), -1.0);
  TEST("Just a small \"random\" case...", int_pow(-23.0, 7), -3404825447.0);
  // for small x: (1+x)^a = 1 + ax + a(a-1)/2 x^2 + ...
  TEST_NEAR("And a larger example...", int_pow(1.00000001, 900), 1.000009000040455, 1e-13);
  TEST_NEAR("And a large example...", int_pow(-10.0, 300), 1e300, 1e285);
  TEST_NEAR("Negative exponent", int_pow(-10.0, -300), 1e-300, 1e-313);
}

static void test_log()
{
  TEST("log(1)", log8(1), 0);
  TEST("log(2)", log8(2), 0);
  TEST("log(7)", log8(7), 0);
  TEST("log(8)", log8(8), 1);
  TEST("log(9)", log8(9), 1);
  TEST("log(60)", log8(60), 1);
  TEST("log(64)", log8(64), 2);
  TEST("log(69)", log8(69), 2);
  TEST("log(500)", log8(500), 2);
  TEST("log(511)", log8(511), 2);
  TEST("log(512)", log8(512), 3);
  TEST("log(0x7fffffff)", log8(0x7fffffff), 10);
  TEST("log(0)", log8(0) < -0x7fffffffL, true);
}

static void test_pow_log()
{
  test_int_pow();
  test_dbl_pow();
  test_log();
}

TESTMAIN( test_pow_log );
