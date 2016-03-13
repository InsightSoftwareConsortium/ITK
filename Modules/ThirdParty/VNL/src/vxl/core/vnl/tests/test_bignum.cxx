//:
// \file
// converted from COOL/test/test_BigNum.C by Peter Vanroose, 25 April 2002.
//
// \verbatim
//  Modifications:
//   20 jan 2008 - Peter Vanroose - added tests on "large" divisions
// \endverbatim

#include <iostream>
#include <limits>
#include <sstream>
#include <iomanip>
#include <vcl_compiler.h>
#include <vnl/vnl_bignum.h>
#include <vnl/vnl_bignum_traits.h>
#include <vnl/vnl_numeric_traits.h> // for vnl_numeric_traits<double>::maxval

#include <testlib/testlib_test.h>

// Two auxiliary functions, used in multiplication and division tests

// Factorial
static vnl_bignum factorial(int n)
{
  if (n <= 1) return vnl_bignum(1);
  else        return n * factorial(n-1);
}

// Combinations (n choose k)
static vnl_bignum Combinations(int n, int k)
{
  vnl_bignum CC(1);
  if (k >= n || k <= 0) return CC;
  else
  {
    for (int i = n; i > n - k; --i)
      CC *= i;
    CC /= factorial(k);
    return CC;
  }
}

static void run_constructor_tests()
{
  std::cout << "\nbignum constructor tests:\n";

  std::cout << "long constructor:\n";
  {vnl_bignum b(0L); TEST("vnl_bignum b(0L);", b, 0L);}
  {vnl_bignum b(1L); TEST("vnl_bignum b(1L);", b, 1L);}
  {vnl_bignum b(-1L); TEST("vnl_bignum b(-1L);", b, -1L);}
  {vnl_bignum b(0x7fffL); TEST("vnl_bignum b(0x7fffL);", b, 0x7fffL);}
  {vnl_bignum b(-0x7fffL); TEST("vnl_bignum b(-0x7fffL);", b, -0x7fffL);}
  {vnl_bignum b(0x7fffffffL); TEST("vnl_bignum b(0x7fffffffL);", b, 0x7fffffffL);}
  {vnl_bignum b(-0x7fffffffL); TEST("vnl_bignum b(-0x7fffffffL);", b, -0x7fffffffL);}
  {vnl_bignum b(0xf00000L); TEST("vnl_bignum b(0xf00000L);", b, 0xf00000);}

  std::cout << "double constructor:\n";
  {vnl_bignum b(0.0); TEST("vnl_bignum b(0.0);", (double)b, 0.0);}
  {vnl_bignum b(1.0); TEST("vnl_bignum b(1.0);", (double)b, 1.0);}
  {vnl_bignum b(-1.0); TEST("vnl_bignum b(-1.0);", (double)b, -1.0);}
  {vnl_bignum b(vnl_numeric_traits<double>::maxval);
   TEST("vnl_bignum b(vnl_numeric_traits<double>::maxval);", (double)b, vnl_numeric_traits<double>::maxval);}
  {vnl_bignum b(-vnl_numeric_traits<double>::maxval);
   TEST("vnl_bignum b(-vnl_numeric_traits<double>::maxval);", (double)b, -vnl_numeric_traits<double>::maxval);}
  {vnl_bignum b(1234567.0); TEST("vnl_bignum b(1234567.0);", (double)b, 1234567.0);}
  {vnl_bignum b(-1234567.0); TEST("vnl_bignum b(-1234567.0);", (double)b, -1234567.0);}
  {vnl_bignum b(1234567e3); TEST("vnl_bignum b(1234567e3);", (double)b, 1234567e3);}
  {vnl_bignum b(-1234567e3); TEST("vnl_bignum b(-1234567e3);", (double)b, -1234567e3);}
  {vnl_bignum b(double(0xf00000)); TEST("vnl_bignum b(double(0xf00000));", b, 0xf00000);}

#ifdef INCLUDE_LONG_DOUBLE_TESTS
  std::cout << "long double constructor:\n";
  {vnl_bignum b(0.0L); TEST("vnl_bignum b(0.0L);", (long double)b, 0.0L);}
  {vnl_bignum b(1.0L); TEST("vnl_bignum b(1.0L);", (long double)b, 1.0L);}
  {vnl_bignum b(-1.0L); TEST("vnl_bignum b(-1.0L);", (long double)b, -1.0L);}
  {vnl_bignum b(1234567.0L); TEST("vnl_bignum b(1234567.0L);", (long double)b, 1234567.0L);}
  {vnl_bignum b(-1234567.0L); TEST("vnl_bignum b(-1234567.0L);", (long double)b, -1234567.0L);}
  {vnl_bignum b(1234567e3L); TEST("vnl_bignum b(1234567e3L);", (long double)b, 1234567e3L);}
  {vnl_bignum b(-1234567e3L); TEST("vnl_bignum b(-1234567e3L);", (long double)b, -1234567e3L);}
  {vnl_bignum b((long double)(0xf00000)); TEST("vnl_bignum b((long double)(0xf00000));", (long double)b, (long double)0xf00000);}
#endif

  std::cout << "char* constructor:\n";
  {vnl_bignum b("-1"); TEST("vnl_bignum b(\"-1\");", b, -1L);}
  {vnl_bignum b("+1"); TEST("vnl_bignum b(\"+1\");", b, 1L);}
  {vnl_bignum b("1"); TEST("vnl_bignum b(\"1\");", b, 1L);}
  {vnl_bignum b("123"); TEST("vnl_bignum b(\"123\");", b, 123L);}
  {vnl_bignum b("123e5"); TEST("vnl_bignum b(\"123e5\");", b, 12300000L);}
  {vnl_bignum b("-123e+4"); TEST("vnl_bignum b(\"-123e+4\");", b, -1230000L);}
  {vnl_bignum b("123e12"); TEST("vnl_bignum b(\"123e12\");", (double)b, 123e12);}
  {vnl_bignum b("-1e120"); std::stringstream s; s << b; std::cout << b << '\n';
   // verify that b outputs as  "-1000...00" (120 zeros)
   bool t = s.str()[0] == '-' && s.str()[1] == '1';
   for (int i=0; i<120; ++i) t = t && s.str()[i+2] == '0';
   TEST("vnl_bignum b(\"-1e120\") outputs first 122 digits as \"-10000...00\"", t, true);
   // This isolates a problem that used to be part of the previous test.
   // I don't think this test failure is a bug in vnl.
   // vnl_bignum seems to populate the stringstream with 122 chars,
   // but the stringstream then reports its size as something else
   // on some systems. - FWW
   TEST("vnl_bignum b(\"-1e120\") outputs a length 122 string", s.str().length(), 122);
   std::cout << "length of string: " << s.str().length() << std::endl;
  }
  {vnl_bignum b("0x0"); TEST("vnl_bignum b(\"0x0\");", b, 0x0);}
  {vnl_bignum b("0x9"); TEST("vnl_bignum b(\"0x9\");", b, 0x9);}
  {vnl_bignum b("0xa"); TEST("vnl_bignum b(\"0xa\");", b, 0xa);}
  {vnl_bignum b("0xf"); TEST("vnl_bignum b(\"0xf\");", b, 0xf);}
  {vnl_bignum b("0xA"); TEST("vnl_bignum b(\"0xA\");", b, 0xa);}
  {vnl_bignum b("0xF"); TEST("vnl_bignum b(\"0xF\");", b, 0xf);}
  {vnl_bignum b("0x1aF"); TEST("vnl_bignum b(\"0x1aF\");", b, 0x1af);}
  {vnl_bignum b("0"); TEST("vnl_bignum b(\"0\");", b, 0);}
  {vnl_bignum b("00"); TEST("vnl_bignum b(\"00\");", b, 0);}
  {vnl_bignum b("012334567"); TEST("vnl_bignum b(\"012334567\");", b, 012334567);}
  {vnl_bignum b("9"); TEST("vnl_bignum b(\"9\");", b, 9);}
  {vnl_bignum b(" 9"); TEST("vnl_bignum b(\" 9\");", b, 9);}
  // infinity:
  {vnl_bignum b("+Inf"); TEST("vnl_bignum b(\"+Inf\");", b.is_plus_infinity(), true);}
  {vnl_bignum b("Infinity"); TEST("vnl_bignum b(\"Infinity\");", b.is_plus_infinity(), true);}
  {vnl_bignum b("-Infin"); TEST("vnl_bignum b(\"-Infin\");", b.is_minus_infinity(), true);}

  std::cout << "reading from istream:\n";
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "+1"; is >> b; TEST("\"+1\" >> b;", b, 1L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "-1"; is >> b; TEST("\"-1\" >> b;", b, -1L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "123"; is >> b; TEST("\"123\" >> b;", b, 123L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "123e5"; is >> b; TEST("\"123e5\" >> b;", b, 12300000L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "123e+4"; is >> b; TEST("\"123e+4\" >> b;", b, 1230000L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "0x0"; is >> b; TEST("\"0x0\" >> b;", b, 0x0);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "0x9"; is >> b; TEST("\"0x9\" >> b;", b, 0x9);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "0xa"; is >> b; TEST("\"0xa\" >> b;", b, 0xa);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "0xf"; is >> b; TEST("\"0xf\" >> b;", b, 0xf);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "0xA"; is >> b; TEST("\"0xA\" >> b;", b, 0xa);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "0xF"; is >> b; TEST("\"0xF\" >> b;", b, 0xf);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "0x1aF"; is >> b; TEST("\"0x1aF\" >> b;", b, 0x1af);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << '0'; is >> b; TEST("\"0\" >> b;", b, 0L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "00"; is >> b; TEST("\"00\" >> b;", b, 0L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << "012334567"; is >> b; TEST("\"012334567\" >> b;", b, 012334567);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << '9'; is >> b; TEST("\"9\" >> b;", b, 9L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_bignum b;
   is << " 9"; is >> b; TEST("\" 9\" >> b;", b, 9L);}

  std::cout << "vnl_bignum& constructor:\n";
  {vnl_bignum b50(vnl_bignum(0L));
  TEST("vnl_bignum b50(vnl_bignum(0L));", (long)b50, 0L);}

  {vnl_bignum b51(vnl_bignum(100L));
  TEST("vnl_bignum b51(vnl_bignum(100L));", (long)b51, 100L);}
}

static void run_conversion_operator_tests()
{
  std::cout << "\nConversion operator tests:\n";

  std::cout << "short conversion operator:\n";
  TEST("short(vnl_bignum(0L)) == 0", short(vnl_bignum(0L)), 0);
  TEST("short(vnl_bignum(0x7fffL)) == 0x7fff", short(vnl_bignum(0x7fffL)), 0x7fff);
  TEST("short(vnl_bignum(-0x7fffL)) == -0x7fff", short(vnl_bignum(-0x7fffL)), -0x7fff);
  TEST("short(vnl_bignum(-0x8000L)) == short(-0x8000)", short(vnl_bignum(-0x8000L)), short(-0x8000));

  std::cout << "int conversion operator:\n";
  TEST("int(vnl_bignum(0L)) == 0", int(vnl_bignum(0L)), 0);
  TEST("int(vnl_bignum(0x7fffffffL)) == 0x7fffffff", int(vnl_bignum(0x7fffffffL)), 0x7fffffff);
  TEST("int(vnl_bignum(-0x7fffffffL)) == -0x7fffffff", int(vnl_bignum(-0x7fffffffL)), -0x7fffffff);

  std::cout << "long conversion operator:\n";
  vnl_bignum b(0x7fffffffL);
  ++b;
  // Two casts are used here instead of a direct cast to unsigned long
  // because vnl_bignum does not implement an overload of "cast to
  // unsigned long".
  TEST("vnl_bignum b(0x7fffffffL); ++b; (unsigned long)long(b) == 0x80000000UL", (unsigned long)long(b), 0x80000000UL);
  --b;
  TEST("vnl_bignum b(0x80000000UL); --b; long(b) == 0x7fffffffL", long(b), 0x7fffffffL);

  // Use -0x7fffffffL-0x1L below instead of -0x80000000L because the
  // latter is parsed like -(0x80000000L) and 0x80000000L is promoted
  // to unsigned long because it is too big to be a signed long.
  ++b; b = -b;
  TEST("vnl_bignum b(0x7fffffffL); ++b; b=-b; long(b) == -0x7fffffffL-0x1L", long(b), -0x7fffffffL-0x1L);

  std::cout << "float conversion operator:\n";
  TEST("float(vnl_bignum(0.0)) == 0.0", (float) vnl_bignum(0.0), 0.0);
  TEST("float(vnl_bignum(99999.0)) == 99999.0",
       ((float) vnl_bignum(99999.0)), 99999.0);
  TEST("float(vnl_bignum(vnl_numeric_traits<float>::maxval)) == vnl_numeric_traits<float>::maxval",
       (vnl_numeric_traits<float>::maxval), (float) vnl_bignum(vnl_numeric_traits<float>::maxval));
  TEST("float(vnl_bignum(-vnl_numeric_traits<float>::maxval)) == -vnl_numeric_traits<float>::maxval",
       (-vnl_numeric_traits<float>::maxval), float(vnl_bignum(-vnl_numeric_traits<float>::maxval)));
  TEST("float(vnl_bignum(\"+Inf\")) == +Inf", (float) vnl_bignum("+Inf"), std::numeric_limits<float>::infinity());

  b = vnl_numeric_traits<double>::maxval;
  ++b;
  TEST("vnl_numeric_traits<double>::maxval + 1 is valid", b.is_infinity(), false);

  std::cout << "double conversion operator:\n";
  TEST("double(vnl_bignum(0.0)) == 0.0", (double) vnl_bignum(0.0), 0.0);
  TEST("double(vnl_bignum(99999.0)) == 99999.0",
       (double) vnl_bignum(99999.0), 99999.0);
  TEST("double(vnl_bignum(1e300)) == 1e300",
       double(vnl_bignum(1e300)), 1e300);
  TEST("double(vnl_bignum(-1e300)) == -1e300",
       double(vnl_bignum(-1e300)), -1e300);
  TEST("double(vnl_bignum(vnl_numeric_traits<float>::maxval)) == vnl_numeric_traits<float>::maxval",
       (vnl_numeric_traits<float>::maxval), (double) vnl_bignum(vnl_numeric_traits<float>::maxval));
  TEST("double(vnl_bignum(-vnl_numeric_traits<float>::maxval)) == -vnl_numeric_traits<float>::maxval",
       (-vnl_numeric_traits<float>::maxval), double(vnl_bignum(-vnl_numeric_traits<float>::maxval)));
  TEST("double(vnl_bignum(vnl_numeric_traits<double>::maxval)) == vnl_numeric_traits<double>::maxval",
       (double) vnl_bignum(vnl_numeric_traits<double>::maxval), vnl_numeric_traits<double>::maxval);
  TEST("double(vnl_bignum(-vnl_numeric_traits<double>::maxval)) == -vnl_numeric_traits<double>::maxval",
       (double) vnl_bignum(-vnl_numeric_traits<double>::maxval), -vnl_numeric_traits<double>::maxval);
  TEST("double(vnl_bignum(\"+Inf\")) == +Inf", (double) vnl_bignum("+Inf"), std::numeric_limits<double>::infinity());

  // Test for bug in bignum::dtobignum()
  // it wasn't resetting the value at the start.
  const vnl_bignum e(1000);
  vnl_bignum d(20);
  vnl_bignum_from_string(d, "1000");
  TEST("vnl_bignum_from_string", e, d);
}

static void run_assignment_tests()
{
  std::cout << "\nStarting assignment tests:\n";
  vnl_bignum b1;

  TEST_RUN ("vnl_bignum b1; b1 = 0xffff;", b1 = 0xffffL, long(b1), 0xffffL);

  // double assignment operator
  TEST_RUN ("double(b1) == -1.23e6", b1 = -1.23e6, double(b1), -1.23e6);

  // long assignment operator
  TEST_RUN ("long(b1) = -0x7fffffff", b1 = -0x7fffffffL, long(b1), -0x7fffffff);

  // char * assignment operator
  TEST_RUN ("long(b1) = 0x1fffffL", b1 = "0x1fffff", long(b1), 0x1fffffL);


  // vnl_bignum& assignment operator
  b1 = "0";
  vnl_bignum b5(0x1ffffL);
  TEST_RUN ("b1 = b5", b1 = b5, b1, b5);
}

static void run_logical_comparison_tests()
{
  std::cout << "\nStarting logical comparison tests:\n";
  vnl_bignum b0(0L);
  vnl_bignum b1(1L);
  vnl_bignum b2(0x7fffL);
  vnl_bignum b3(-0x7fffL);
  vnl_bignum p_inf("+Inf");
  vnl_bignum m_inf("-Inf");

  TEST("b0 == b0", b0 == b0, true);
  TEST("b0 == b1", b0 == b1, false);
  TEST("b0 == b2", b0 == b2, false);
  TEST("b0 == b3", b0 == b3, false);
  TEST("b1 == b1", b1 == b1, true);
  TEST("b1 == b2", b1 == b2, false);
  TEST("b1 == b3", b1 == b3, false);
  TEST("b2 == b2", b2 == b2, true);
  TEST("b2 == b3", b2 == b3, false);
  TEST("b3 == b3", b3 == b3, true);
  TEST("p_inf == p_inf", p_inf == p_inf, true);
  TEST("p_inf == m_inf", p_inf == m_inf, false);
  TEST("m_inf == m_inf", m_inf == m_inf, true);
  TEST("b0 == p_inf", b0 == p_inf, false);
  TEST("b1 == p_inf", b1 == p_inf, false);
  TEST("b2 == p_inf", b2 == p_inf, false);
  TEST("b3 == p_inf", b3 == p_inf, false);
  TEST("b0 == m_inf", b0 == m_inf, false);
  TEST("b1 == m_inf", b1 == m_inf, false);
  TEST("b2 == m_inf", b2 == m_inf, false);
  TEST("b3 == m_inf", b3 == m_inf, false);

  TEST("b0 != b0", b0 != b0, false);
  TEST("b0 != b1", b0 != b1, true);
  TEST("b0 != b2", b0 != b2, true);
  TEST("b0 != b3", b0 != b3, true);
  TEST("b1 != b1", b1 != b1, false);
  TEST("b1 != b2", b1 != b2, true);
  TEST("b1 != b3", b1 != b3, true);
  TEST("b2 != b2", b2 != b2, false);
  TEST("b2 != b3", b2 != b3, true);
  TEST("b3 != b3", b3 != b3, false);

  TEST("b0 < b0", b0 < b0, false);
  TEST("b0 < b1", b0 < b1, true);
  TEST("b0 < b2", b0 < b2, true);
  TEST("b0 < b3", b0 < b3, false);
  TEST("b1 < b1", b1 < b1, false);
  TEST("b1 < b2", b1 < b2, true);
  TEST("b1 < b3", b1 < b3, false);
  TEST("b2 < b2", b2 < b2, false);
  TEST("b2 < b3", b2 < b3, false);
  TEST("b3 < b3", b3 < b3, false);
  TEST("p_inf < p_inf", p_inf < p_inf, false);
  TEST("p_inf < m_inf", p_inf < m_inf, false);
  TEST("m_inf < p_inf", m_inf < p_inf, true);
  TEST("m_inf < m_inf", m_inf < m_inf, false);
  TEST("b0 < p_inf", b0 < p_inf, true);
  TEST("b1 < p_inf", b1 < p_inf, true);
  TEST("b2 < p_inf", b2 < p_inf, true);
  TEST("b3 < p_inf", b3 < p_inf, true);
  TEST("b0 < m_inf", b0 < m_inf, false);
  TEST("b1 < m_inf", b1 < m_inf, false);
  TEST("b2 < m_inf", b2 < m_inf, false);
  TEST("b3 < m_inf", b3 < m_inf, false);

  TEST("b0 > b0", b0 > b0, false);
  TEST("b0 > b1", b0 > b1, false);
  TEST("b0 > b2", b0 > b2, false);
  TEST("b0 > b3", b0 > b3, true);
  TEST("b1 > b1", b1 > b1, false);
  TEST("b1 > b2", b1 > b2, false);
  TEST("b1 > b3", b1 > b3, true);
  TEST("b2 > b2", b2 > b2, false);
  TEST("b2 > b3", b2 > b3, true);
  TEST("b3 > b3", b3 > b3, false);
  TEST("p_inf > p_inf", p_inf > p_inf, false);
  TEST("p_inf > m_inf", p_inf > m_inf, true);
  TEST("m_inf > p_inf", m_inf > p_inf, false);
  TEST("m_inf > m_inf", m_inf > m_inf, false);
  TEST("b0 > p_inf", b0 > p_inf, false);
  TEST("b1 > p_inf", b1 > p_inf, false);
  TEST("b2 > p_inf", b2 > p_inf, false);
  TEST("b3 > p_inf", b3 > p_inf, false);
  TEST("b0 > m_inf", b0 > m_inf, true);
  TEST("b1 > m_inf", b1 > m_inf, true);
  TEST("b2 > m_inf", b2 > m_inf, true);
  TEST("b3 > m_inf", b3 > m_inf, true);

  TEST("b3 != b2", b3 != b2, true);
  TEST("b3 != b3", b3 != b3, false);
  TEST("b3 < b2", b3 < b2, true);
  TEST("b3 <= b2", b3 <= b2, true);
  TEST("b3 <= b3", b3 <= b3, true);
  TEST("b3 > b3", b3 > b3, false);
  TEST("b3 > b2", b3 > b2, false);
  TEST("b3 >= b2", b3 >= b2, false);
  TEST("b3 >= b3", b3 >= b3, true);
  TEST("b2 >= b2", b2 >= b2, true);
  std::cout << b2 << " == " << &b2 << std::endl;
  TEST("<<", 1, 1);
}

static void run_division_tests()
{
  std::cout << "\nStarting division tests:\n";

  TEST("long(vnl_bignum(0L)/vnl_bignum(1L))", long(vnl_bignum(0L)/vnl_bignum(1L)), 0L);
  TEST("long(vnl_bignum(-1L)/vnl_bignum(1L))", long(vnl_bignum(-1L)/vnl_bignum(1L)), -1L);
  TEST("long(vnl_bignum(-1L)/vnl_bignum(\"+Inf\"))", long(vnl_bignum(-1L)/vnl_bignum("+Inf")), 0L);
  TEST("vnl_bignum(\"+Inf\")/(-1L)", vnl_bignum("+Inf")/(-1L), vnl_bignum("-Inf"));
  TEST("vnl_bignum(-1L)/0L", vnl_bignum(-1L)/0L, vnl_bignum("-Inf"));

  long i,j,k,l;
  long div_errors = 0;
  long mod_errors = 0;

  std::cout << " for (i = 0xffffff; i > 0; i /= 0x10)\n"
           << "   for (j = 0x7ffffff; j > 0; j /= 0x10)\n"
           << "     for (k = 1; k < 17; ++k)\n"
           << "       for (l = 1; l < 17; ++l)\n         ";
  for (i = 0xffffff; i > 0; i /= 0x10) {
    std::cout.put('.');
    std::cout.flush();
    for (j = 0x7ffffff; j > 0; j /= 0x10) {
      for (k = 1; k < 17; ++k) {
        for (l = 1; l < 17; ++l) {
          vnl_bignum b1(i+k);
          vnl_bignum b2(j+l);
          vnl_bignum b3(long((i+k)/(j+l)));
          if (b1/b2 != b3) {
            TEST("(vnl_bignum(i+k)/vnl_bignum(j+l)) == vnl_bignum(long((i+k)/(j+l)))", false, true);
            std::cout<<std::hex<< "i=0x"<<i<<", j=0x"<<j<<", k=0x"<<k<<", l="<<l
                    <<std::dec<<", b1="<<b1<<", b2="<<b2<<", b3="<<b3<<'\n';
            ++div_errors;
          }
          b3 = vnl_bignum(long((i+k)%(j+l)));
          if (b1%b2 != b3) {
            TEST("(vnl_bignum(i+k)%vnl_bignum(j+l)) == vnl_bignum(long((i+k)%(j+l)))", false, true);
            std::cout<<std::hex<< "i=0x"<<i<<", j=0x"<<j<<", k=0x"<<k<<", l="<<l
                    <<std::dec<<", b1="<<b1<<", b2="<<b2<<", b3="<<b3<<'\n';
            ++mod_errors;
          }
        }
      }
    }
  }

  std::cout << '\n';
  TEST("(vnl_bignum(i+k)/vnl_bignum(j+l)) == vnl_bignum(long((i+k)/(j+l)))",
       div_errors, 0);
  TEST("(vnl_bignum(i+k)%vnl_bignum(j+l)) == vnl_bignum(long((i+k)%(j+l)))",
       mod_errors, 0);

#ifdef INTERACTIVE
  char num[130], den[130];
  vnl_bignum b,r;

  while (true) {
    std::cout << "Enter next numerator:  ";
    std::cin >> num;
    std::cout << "Enter next denominator:  ";
    std::cin >> den;

    b = vnl_bignum(num)/vnl_bignum(den);
    r = vnl_bignum(num) % vnl_bignum(den);
    std::cout <<   "\nquotient  = " << b
             <<   "\n            "; b.dump();
    std::cout << "\n\nremainder = " << r
             <<   "\n            "; r.dump();
    std::cout << '\n';
  }
#endif
}

static void run_large_division_tests()
{
  std::cout << "\nStarting large division tests:\n";

  vnl_bignum a("10000000"), b("10000001"); b *= a; vnl_bignum c = b/10000001;
  std::cout << b << " / 10000001 = " << c << ", must be 10000000\n";
  TEST("100000010000000 / 10000001", c, a);

  // these are the same numbers, now written in hexadecimal:
  a = "0X989680"; b = "0X989681"; b *= a; c = b/vnl_bignum("0X989681");
  std::cout << b << " / 0X989681 = " << c << ", must be " << a << std::endl;
  TEST("0x5AF31112D680 / 0x989681", c, a);

  // an other decimal example:
  a = "111111"; b = "111111"; b *= a; c = b/111111;
  std::cout << b << " / 111111 = " << c << ", must be 111111\n";
  TEST("12345654321 / 111111", c, a);

  // these are the same numbers, now written in hexadecimal:
  a = "0X2B67"; b = "0X2B67"; b *= a; c = b/vnl_bignum("0X2B67");
  std::cout << b << " / 0X2B67 = " << c << ", must be " << a << std::endl;
  TEST("12345654321 / 0X2B67", c, a);

  a = "98789"; b = "98789"; b *= a; c = b/98789;
  std::cout << b << " / 98789 = " << c << ", must be 98789\n";
  TEST("9759266521 / 98789", c, a);

  a = "1e100"; b = "1e200"; c = b/a;
  std::cout << "1e200 / 1e100 = " << c << ", must be 1e100\n";
  TEST("1e200 / 1e100", c, a);

  a = "-1e100"; b = "1e200"; c = b/a;
  std::cout << "1e200 / -1e100 = " << c << ", must be -1e100\n";
  TEST("1e200 / -1e100", c, a);

  a = "1e100"; b = "-1e200"; c = b/a;
  std::cout << "-1e200 / 1e100 = " << c << ", must be -1e100\n";
  TEST("-1e200 / 1e100", c, -a);

  a = "-1e100"; b = "-1e200"; c = b/a;
  std::cout << "-1e200 / -1e100 = " << c << ", must be 1e100\n";
  TEST("-1e200 / -1e100", c, -a);

  a = "1e100"; b = "1e200"; c = b%a;
  std::cout << "1e200 % 1e100 = " << c << ", must be 0\n";
  TEST("1e100^2 % 1e100", c, 0);

  std::cout << "C(16,8) = " << Combinations(16,8) << std::endl;
  TEST("16 choose 8 = 12870", Combinations(16,8), 12870);
  std::cout << "C(18,9) = " << Combinations(18,9) << std::endl;
  TEST("18 choose 9 = 48620", Combinations(18,9), 48620);
  std::cout << "C(20,10) = " << Combinations(20,10) << std::endl;
  TEST("20 choose 10 = 184756", Combinations(20,10), 184756);
  std::cout << "C(100,44) = " << Combinations(100,44) << std::endl;
  TEST("100 choose 44 = 49378235797073715747364762200",
       Combinations(100,44), "49378235797073715747364762200");
}

static void run_multiplication_division_tests()
{
  std::cout << "\nCheck example in book:\n";

  vnl_bignum b2 = "0xffffffff";                 // Create vnl_bignum object
  vnl_bignum b3 = "12345e30";                   // Create vnl_bignum object

  std::cout << "b2 = " << b2 << std::endl
           << "b3 = " << b3 << std::endl;

  TEST("(b2*b3) / b3 = b2", ((b2*b3) / b3 == b2), 1);
  TEST("(b2*b3) / b2 = b3", ((b2*b3) / b2 == b3), 1);
  TEST("((b3/b2) * b2) + (b3%b2) = b3", (((b3/b2) * b2) + (b3%b2) == b3), 1);
}

static void run_addition_subtraction_tests()
{
  std::cout << "\nStarting addition, subtraction tests:\n";

  long i,j;
  long add_errors = 0;
  long sub_errors = 0;
  vnl_bignum bi,bj,bij;

  std::cout << " for (i = 1; i < 0xfffffff;  i *= 3)\n"
           << "   for (j = 1; j < 0xfffffff; j *= 3)\n      ";

  {for (i = 1; i < 0xfffffff;  i *= 3) {
    std::cout.put('.');
    std::cout.flush();
    for (j = 1; j < 0xfffffff; j *= 3) {
      bi = i;
      bj = j;
      bij = vnl_bignum(i+j);
      if (bi + bj != bij) {
        TEST("bi + bj == vnl_bignum(i + j)", false, true);
        std::cout << "i = "<<i<<", j = "<<j<<std::endl;
        ++add_errors;
      }
      bij = vnl_bignum(i-j);
      if (bi - bj != bij) {
        TEST("bi - bj == vnl_bignum(i - j)", false, true);
        std::cout << "i = "<<i<<", j = "<<j<<std::endl;
        ++sub_errors;
      }
    }
  }}
  std::cout << std::endl;
  TEST("bi + bj == vnl_bignum(i + j)", add_errors, 0);
  TEST("bi - bj == vnl_bignum(i - j)", sub_errors, 0);

  vnl_bignum b0(0L);
  vnl_bignum zillion("1000000000000000000");
  vnl_bignum b1000(1000L), b1000000(1000000L);
  vnl_bignum p_inf("+Inf"), m_inf("-Inf");

  TEST("-b0 == b0", -b0, b0);
  TEST("-p_inf == m_inf", -p_inf, m_inf);
  TEST("-m_inf == p_inf", -m_inf, p_inf);
  TEST("-b1000 == vnl_bignum(-1L)*b1000", -b1000, vnl_bignum(-1L)*b1000);
  TEST("-(-b1000000) == b1000000", -(-b1000000), b1000000);
  TEST("b0 + b1000 == b1000", b0 + b1000, b1000);
  TEST("b0 + b1000000 == b1000000", b0 + b1000000, b1000000);
  TEST("b1000 + b0 == b1000", b1000 + b0, b1000);
  TEST("b1000000 + b0 == b1000000", b1000000 + b0, b1000000);
  TEST("b0 + (-b1000) == -b1000", b0 + (-b1000), -b1000);
  TEST("-b1000 + b0 == -b1000", -b1000 + b0, -b1000);
  TEST("-b1000 + (-b1000) == (vnl_bignum(2L)*(-b1000))",
        -b1000 + (-b1000),   (vnl_bignum(2L)*(-b1000)));
  TEST("-b1000000 + (-b1000000) == (vnl_bignum(2L)*(-b1000000))",
        -b1000000 + (-b1000000), (vnl_bignum(2L)*(-b1000000)));
  TEST("b1000000 + (-b1000000) == b0", b1000000 + (-b1000000), b0);
  TEST("zillion + (-zillion) == b0", zillion + (-zillion), b0);
  TEST("zillion + b1000 == b1000000*b1000000*b1000000 + b1000",
        zillion + b1000,   b1000000*b1000000*b1000000 + b1000);
  TEST("zillion + p_inf == p_inf", zillion + p_inf, p_inf);
  TEST("zillion + m_inf == m_inf", zillion + m_inf, m_inf);
  TEST("p_inf + zillion == p_inf", p_inf + zillion, p_inf);
  TEST("m_inf + zillion == m_inf", m_inf + zillion, m_inf);

  TEST("b0 - b1000 == -b1000", b0 - b1000, -b1000);
  TEST("b0 - b1000000 == -b1000000", b0 -b1000000, -b1000000);
  TEST("b1000 - b0 == b1000", b1000 - b0, b1000);
  TEST("b1000000 - b0 == b1000000", b1000000 - b0, b1000000);
  TEST("b0 - (-b1000) == b1000", b0 - (-b1000), b1000);
  TEST("-b1000 - b0 == -b1000", -b1000 - b0, -b1000);
  TEST("-b1000 - (-b1000) == b0", -b1000 - (-b1000), b0);
  TEST("-b1000 - (-zillion) == zillion - b1000",
        -b1000 - (-zillion),   zillion - b1000);
  TEST("-b1000000 - (-b1000000) == b0", -b1000000 - (-b1000000), b0);
  TEST("-b1000000 - (b1000000) == -vnl_bignum(2L)*b1000000",
        -b1000000 - (b1000000),   -vnl_bignum(2L)*b1000000);
  TEST("b1000000 - (-b1000000) == vnl_bignum(2L)*b1000000",
        b1000000 - (-b1000000),   vnl_bignum(2L)*b1000000);
  TEST("zillion - (-zillion) == vnl_bignum(2L)*zillion",
        zillion - (-zillion),   vnl_bignum(2L)*zillion);
  TEST("zillion - b1000 == b1000000*b1000000*b1000000 - b1000",
        zillion - b1000,   b1000000*b1000000*b1000000 - b1000);
  TEST("-zillion - b1000 == -b1000000*b1000000*b1000000 - b1000",
        -zillion - b1000,   -b1000000*b1000000*b1000000 - b1000);
  TEST("zillion - p_inf == m_inf", zillion - p_inf, m_inf);
  TEST("zillion - m_inf == p_inf", zillion - m_inf, p_inf);
  TEST("p_inf - zillion == p_inf", p_inf - zillion, p_inf);
  TEST("m_inf - zillion == m_inf", m_inf - zillion, m_inf);

  // example in book
  vnl_bignum b2 = "0xffffffff";                 // Create vnl_bignum object
  vnl_bignum b3 = "12345e30";                   // Create vnl_bignum object
  TEST("(b2+b3) - b2 = b3", (b2+b3) - b2 == b3, 1);
  TEST("(b2+b3) - b3 = b2", (b2+b3) - b3 == b2, 1);
  b3.dump();
  TEST("b3.dump()", 1, 1);
}


static void run_increment_tests()
{
  std::cout << "increment special cases:\n";
  vnl_bignum b1;
  TEST("b1     ==  0", b1, 0);
  ++b1;
  TEST("++b1   ==  1", b1, 1);
  ++b1;
  TEST("++b1   ==  2", b1, 2);
  --b1;
  TEST("--b1   ==  1", b1, 1);
  --b1;
  TEST("--b1   ==  0", b1, 0);
  --b1;
  TEST("--b1   == -1", b1, -1);
  --b1;
  TEST("--b1   == -2", b1, -2);
  ++b1;
  TEST("++b1   == -1", b1, -1);
  ++b1;
  TEST("++b1   ==  0", b1, 0);

  vnl_bignum b2("Infinity");
  TEST("b2     ==  infinity", b2.is_plus_infinity(), true);
  ++b2;
  TEST("++b2   ==  infinity", b2.is_plus_infinity(), true);
  --b2;
  TEST("--b2   ==  infinity", b2.is_plus_infinity(), true);

  vnl_bignum b3("-Infinity");
  TEST("b3     ==  -infinity", b3.is_minus_infinity(), true);
  ++b3;
  TEST("++b3   ==  -infinity", b3.is_minus_infinity(), true);
  --b3;
  TEST("--b3   ==  -infinity", b3.is_minus_infinity(), true);

  vnl_bignum b4("65534");
  TEST("b4     ==  65534", b4, 65534);
  ++b4;
  TEST("++b4   ==  65535", b4, 65535);
  ++b4;
  TEST("++b4   ==  65536", b4, 65536);
  ++b4;
  TEST("++b4   ==  65537", b4, 65537);
  --b4;
  TEST("--b4   ==  65536", b4, 65536);
  --b4;
  TEST("--b4   ==  65535", b4, 65535);
  --b4;
  TEST("--b4   ==  65534", b4, 65534);


  vnl_bignum b5("-65534");
  TEST("b5     ==  -65534", b5, -65534);
  --b5;
  TEST("--b5   ==  -65535", b5, -65535);
  --b5;
  TEST("--b5   ==  -65536", b5, -65536);
  --b5;
  TEST("--b5   ==  -65537", b5, -65537);
  ++b5;
  TEST("++b5   ==  -65536", b5, -65536);
  ++b5;
  TEST("++b5   ==  -65535", b5, -65535);
  ++b5;
  TEST("++b5   ==  -65534", b5, -65534);
}


static void run_multiplication_tests()
{
  std::cout << "\nStarting multiplication tests:\n";

  vnl_bignum b0(0L), b1000(1000L), b1000000(1000000L),
  zillion("1000000000000000000");
  vnl_bignum p_inf("+Inf"), m_inf("-Inf");

  TEST("b0*b0 == b0", b0*b0, b0);
  TEST("b0*b1000 == b0", b0*b1000, b0);
  TEST("b1000*b0 == b0", b1000*b0, b0);
  TEST("b1000*b1000 == b1000000", b1000*b1000, b1000000);
  TEST("b1000*b1000000 == b1000000*b1000", b1000*b1000000, b1000000*b1000);
  TEST("-b1000000*b1000000*b1000000 == -zillion", -b1000000*b1000000*b1000000, -zillion);
  TEST("zillion*-b1000 == b1000*-zillion", zillion*-b1000, b1000*-zillion);
  TEST("p_inf*b1000 == p_inf", p_inf*b1000, p_inf);
  TEST("m_inf*b1000 == m_inf", m_inf*b1000, m_inf);

  TEST("10! = 3628800", factorial(10), 3628800);
  TEST("15! = 1307674368000", factorial(15), "1307674368000");
  TEST("44! = 2.65827157478844e54", factorial(44),
       "2658271574788448768043625811014615890319638528000000000");
}

static void run_left_shift_tests()
{
  vnl_bignum b1(1L);
  vnl_bignum p_inf("+Inf"), m_inf("-Inf");

  // left shift
  TEST("int(b1) == 1",int(b1), 1);
  TEST("long(b1 << 1) == 2",long(b1 << 1), 2);
  TEST("long(b1 << 2) == 4",long(b1 << 2), 4);
  TEST("long(b1 << 3) == 8",long(b1 << 3), 8);
  TEST("long(b1 << 4) == 16",long(b1 << 4), 16);
  TEST("long(b1 << 5) == 32",long(b1 << 5), 32);
  TEST("long(b1 << 6) == 64",long(b1 << 6), 64);
  TEST("long(b1 << 7) == 128",long(b1 << 7), 128);
  TEST("long(b1 << 8) == 256",long(b1 << 8), 256);
  TEST("long(b1 << 9) == 512",long(b1 << 9), 512);
  TEST("long(b1 << 10) == 1024",long(b1 << 10), 1024);
  TEST("long(b1 << 11) == 2048",long(b1 << 11), 2048);
  TEST("long(b1 << 12) == 4096",long(b1 << 12), 4096);
  TEST("long(b1 << 13) == 8192",long(b1 << 13), 8192);
  TEST("long(b1 << 14) == 16384",long(b1 << 14), 16384);
  TEST("long(b1 << 15) == 32768",long(b1 << 15), 32768);
  TEST("long(b1 << 16) == 65536",long(b1 << 16), 65536);
  TEST("(b1 << 32) == vnl_bignum(\"4294967296\")",
       (b1 << 32), vnl_bignum("4294967296"));
  TEST("p_inf << 16 == p_inf",p_inf << 16, p_inf);

  TEST("long(-b1 << 1) == -2",long(-b1 << 1), -2);
  TEST("long(-b1 << 2) == -4",long(-b1 << 2), -4);
  TEST("long(-b1 << 3) == -8",long(-b1 << 3), -8);
  TEST("long(-b1 << 4) == -16",long(-b1 << 4), -16);
  TEST("long(-b1 << 5) == -32",long(-b1 << 5), -32);
  TEST("long(-b1 << 6) == -64",long(-b1 << 6), -64);
  TEST("long(-b1 << 7) == -128",long(-b1 << 7), -128);
  TEST("long(-b1 << 8) == -256",long(-b1 << 8), -256);
  TEST("long(-b1 << 9) == -512",long(-b1 << 9), -512);
  TEST("long(-b1 << 10) == -1024",long(-b1 << 10), -1024);
  TEST("long(-b1 << 11) == -2048",long(-b1 << 11), -2048);
  TEST("long(-b1 << 12) == -4096",long(-b1 << 12), -4096);
  TEST("long(-b1 << 13) == -8192",long(-b1 << 13), -8192);
  TEST("long(-b1 << 14) == -16384",long(-b1 << 14), -16384);
  TEST("long(-b1 << 15) == -32768",long(-b1 << 15), -32768);
  TEST("long(-b1 << 16) == -65536",long(-b1 << 16), -65536);
  TEST("(-b1 << 32) == vnl_bignum(\"-4294967296\")",
       (-b1 << 32), vnl_bignum("-4294967296"));
  TEST("m_inf << 16 == m_inf",m_inf << 16, m_inf);

  TEST("long(b1 << -16) == 0",long(b1 << -16), 0);
  TEST("long(-b1 << -16) == 0",long(-b1 << -16), 0);
}

static void run_right_shift_tests()
{
  vnl_bignum b2("4294967296");
  vnl_bignum p_inf("+Inf"), m_inf("-Inf");

  // right shift
  TEST("b2 == vnl_bignum(\"4294967296\")",b2, vnl_bignum("4294967296"));
  TEST("(b2 >> 1) == vnl_bignum(\"2147483648\")", (b2 >> 1), vnl_bignum("2147483648"));
  TEST("long(b2 >> 2) == 1073741824",long(b2 >> 2), 1073741824L);
  TEST("long(b2 >> 3) == 536870912",long(b2 >> 3), 536870912L);
  TEST("long(b2 >> 4) == 268435456",long(b2 >> 4), 268435456L);
  TEST("long(b2 >> 5) == 134217728",long(b2 >> 5), 134217728L);
  TEST("long(b2 >> 6) == 67108864",long(b2 >> 6), 67108864L);
  TEST("long(b2 >> 7) == 33554432",long(b2 >> 7), 33554432L);
  TEST("long(b2 >> 8) == 16777216",long(b2 >> 8), 16777216L);
  TEST("long(b2 >> 9) == 8388608",long(b2 >> 9), 8388608L);
  TEST("long(b2 >> 10) == 4194304",long(b2 >> 10), 4194304L);
  TEST("long(b2 >> 11) == 2097152",long(b2 >> 11), 2097152L);
  TEST("long(b2 >> 12) == 1048576",long(b2 >> 12), 1048576L);
  TEST("long(b2 >> 13) == 524288",long(b2 >> 13), 524288L);
  TEST("long(b2 >> 14) == 262144",long(b2 >> 14), 262144L);
  TEST("long(b2 >> 15) == 131072",long(b2 >> 15), 131072L);
  TEST("long(b2 >> 16) == 65536",long(b2 >> 16), 65536L);
  TEST("long(b2 >> 32) == 1",long(b2 >> 32), 1L);
  TEST("long(b2 >> 33) == 0",long(b2 >> 33), 0L);
  TEST("long(b2 >> 67) == 0",long(b2 >> 67), 0L);
  TEST("p_inf >> 16 == p_inf",p_inf >> 16, p_inf);

  TEST("(-b2 >> 1) == vnl_bignum(\"-2147483648\")", (-b2 >> 1), vnl_bignum("-2147483648"));
  TEST("long(-b2 >> 2) == -1073741824",long(-b2 >> 2), -1073741824L);
  TEST("long(-b2 >> 3) == -536870912",long(-b2 >> 3), -536870912L);
  TEST("long(-b2 >> 4) == -268435456",long(-b2 >> 4), -268435456L);
  TEST("long(-b2 >> 5) == -134217728",long(-b2 >> 5), -134217728L);
  TEST("long(-b2 >> 6) == -67108864",long(-b2 >> 6), -67108864L);
  TEST("long(-b2 >> 7) == -33554432",long(-b2 >> 7), -33554432L);
  TEST("long(-b2 >> 8) == -16777216",long(-b2 >> 8), -16777216L);
  TEST("long(-b2 >> 9) == -8388608",long(-b2 >> 9), -8388608L);
  TEST("long(-b2 >> 10) == -4194304",long(-b2 >> 10), -4194304L);
  TEST("long(-b2 >> 11) == -2097152",long(-b2 >> 11), -2097152L);
  TEST("long(-b2 >> 12) == -1048576",long(-b2 >> 12), -1048576L);
  TEST("long(-b2 >> 13) == -524288",long(-b2 >> 13), -524288L);
  TEST("long(-b2 >> 14) == -262144",long(-b2 >> 14), -262144L);
  TEST("long(-b2 >> 15) == -131072",long(-b2 >> 15), -131072L);
  TEST("long(-b2 >> 16) == -65536",long(-b2 >> 16), -65536L);
  TEST("long(-b2 >> 32) == -1",long(-b2 >> 32), -1);
  TEST("long(-b2 >> 33) == -0",long(-b2 >> 33), 0);
  TEST("long(-b2 >> 67) == -0",long(-b2 >> 67), 0);
  TEST("m_inf >> 16 == m_inf",m_inf >> 16, m_inf);
}

static void run_shift_tests()
{
  std::cout << "\nStarting shift tests:\n";

  run_left_shift_tests();
  run_right_shift_tests();
#ifdef INTERACTIVE
  vnl_bignum b;
  char s[100];
  int sh;

  while (true) {
    std::cout << "Enter next vnl_bignum:  ";
    std::cin >> s;
    b = s;
    std::cout << "Enter shift amount: ";
    std::cin >> sh;
    std::cout << "Shift == " << sh << std::endl;

    b = b << sh;
  }
#endif
}

void test_bignum()
{
  run_constructor_tests();
  run_conversion_operator_tests();
  run_assignment_tests();
  run_addition_subtraction_tests();
  run_increment_tests();
  run_multiplication_tests();
  run_division_tests();
  run_multiplication_division_tests();
  run_large_division_tests();
  run_shift_tests();
  run_logical_comparison_tests();
}

TESTMAIN(test_bignum);
