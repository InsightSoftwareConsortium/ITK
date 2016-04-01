#include <iostream>
#include <sstream>
#include <iomanip>
#include <vcl_compiler.h>
#include <vnl/vnl_decnum.h>
#include <testlib/testlib_test.h>

// Two auxiliary functions, used in multiplication and division tests

// Factorial
static vnl_decnum factorial(unsigned long n)
{
  if (n <= 1) return vnl_decnum(1L);
  else        return factorial(n-1) * vnl_decnum(n);
}

// Combinations (n choose k)
static vnl_decnum binom(unsigned long n, unsigned long k)
{
  vnl_decnum r(1L);
  if (k >= n || k == 0) return r;
  else
  {
    for (unsigned long i=n; i>n-k; --i)
      r *= i;
    r /= factorial(k);
    return r;
  }
}

static void run_constructor_tests()
{
  std::cout << "\ndecnum constructor tests:\n";

  std::cout << "long constructor:\n";
  {vnl_decnum b(0L); TEST("vnl_decnum b(0L);", b, 0L);}
  {vnl_decnum b(1L); TEST("vnl_decnum b(1L);", b, 1L);}
  {vnl_decnum b(-1L); TEST("vnl_decnum b(-1L);", b, -1L);}
  {vnl_decnum b(0x7fffL); TEST("vnl_decnum b(0x7fffL);", b, 0x7fffL);}
  {vnl_decnum b(-0x7fffL); TEST("vnl_decnum b(-0x7fffL);", b, -0x7fffL);}
  {vnl_decnum b(0x7fffffffL); TEST("vnl_decnum b(0x7fffffffL);", b, 0x7fffffffL);}
  {vnl_decnum b(-0x7fffffffL); TEST("vnl_decnum b(-0x7fffffffL);", b, -0x7fffffffL);}
  {vnl_decnum b(0xf00000L); TEST("vnl_decnum b(0xf00000L);", b, 0xf00000);}

  std::cout << "double constructor:\n";
  {vnl_decnum b(0.0); TEST("vnl_decnum b(0.0);", b, 0L);}
  {vnl_decnum b(1.0); TEST("vnl_decnum b(1.0);", b, 1L);}
  {vnl_decnum b(-1.0); TEST("vnl_decnum b(-1.0);", b, -1L);}
  {vnl_decnum b(1234.0); TEST("vnl_decnum(1234.0)", b, 1234L);}
  {vnl_decnum b(-1234.); TEST("vnl_decnum(-1234.)", b, -1234L);}
  {vnl_decnum b(1234e3); TEST("vnl_decnum(1234e3)", b, 1234000L);}
  {vnl_decnum b(-1234e3); TEST("vnl_decnum(-1234e3)", b, -1234000L);}
  {vnl_decnum b(1234e-3); TEST("vnl_decnum(1234e-3)", long(b*"1000"), 1234L);}
  {vnl_decnum b(1.234); TEST("vnl_decnum(1.234)", long(b*"1000"), 1234L);}

  std::cout << "char* constructor:\n";
  {vnl_decnum b("-1"); TEST("vnl_decnum b(\"-1\");", b, -1L);}
  {vnl_decnum b("+1"); TEST("vnl_decnum b(\"+1\");", b, 1L);}
  {vnl_decnum b("1"); TEST("vnl_decnum b(\"1\");", b, 1L);}
  {vnl_decnum b("123"); TEST("vnl_decnum b(\"123\");", b, 123L);}
  {vnl_decnum b("123e5"); TEST("vnl_decnum b(\"123e5\");", b, 12300000L);}
  {vnl_decnum b("-123e+4"); TEST("vnl_decnum b(\"-123e+4\");", b, -1230000L);}
  {vnl_decnum b("-1e120"); std::stringstream s; s << b << ' ';
   // verify that b outputs as  "-1e120"
   bool t = s.str()[0] == '-' && s.str()[1] == '1' && s.str()[2] == 'e'
         && s.str()[3] == '1' && s.str()[4] == '2' && s.str()[5] == '0' && s.str()[6] == ' ';
   TEST("vnl_decnum b(\"-1e120\") outputs as \"-1e120\"", t, true);
  }
  {vnl_decnum b("0"); TEST("vnl_decnum b(\"0\");", b, 0L);}
  {vnl_decnum b("00"); TEST("vnl_decnum b(\"00\");", b, 0L);}
  {vnl_decnum b("012334567"); TEST("vnl_decnum b(\"012334567\");", b, 12334567L);}
  {vnl_decnum b("9"); TEST("vnl_decnum b(\"9\");", b, 9L);}
  {vnl_decnum b(" 9"); TEST("vnl_decnum b(\" 9\");", b, 9L);}

  // infinity and NaN:
  {vnl_decnum b("-123"); TEST("isfinite(-123)", vnl_math::isfinite(b), true);}
  {vnl_decnum b("0"); TEST("isfinite(0)", vnl_math::isfinite(b), true);}
  {vnl_decnum b("1e999"); TEST("isfinite(1e999)", vnl_math::isfinite(b), true);}
  {vnl_decnum b("NaN"); TEST("isfinite(NaN)", vnl_math::isfinite(b), false);}
  {vnl_decnum b("NaN"); TEST("isnan(NaN)", vnl_math::isnan(b), true);}
  {vnl_decnum b("1e999"); TEST("isnan(1e999)", vnl_math::isnan(b), false);}
  {vnl_decnum b("+Inf"); TEST("vnl_decnum(\"+Inf\")", vnl_math::isfinite(b), false);}
  {vnl_decnum b("Infinity"); TEST("vnl_decnum(\"Infinity\")", vnl_math::isfinite(b), false);}
  {vnl_decnum b("-Infin"); TEST("vnl_decnum(\"-Infin\")", vnl_math::isfinite(b), false);}
  {vnl_decnum b("Inf"); TEST("isnan(Inf)", vnl_math::isnan(b), false);}

  std::cout << "reading from istream:\n";
  {std::stringstream is(std::ios::in | std::ios::out); vnl_decnum b;
   is << "+1"; is >> b; TEST("\"+1\" >> b;", b, 1L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_decnum b;
   is << "-1"; is >> b; TEST("\"-1\" >> b;", b, -1L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_decnum b;
   is << "123"; is >> b; TEST("\"123\" >> b;", b, 123L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_decnum b;
   is << "123e5"; is >> b; TEST("\"123e5\" >> b;", b, 12300000L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_decnum b;
   is << "-123e+4"; is >> b; TEST("\"-123e+4\" >> b;", b, -1230000L);}
  {std::stringstream is(std::ios::in | std::ios::out); vnl_decnum b;
   is << " 9"; is >> b; TEST("\" 9\" >> b;", b, 9L);}

  std::cout << "vnl_decnum& constructor:\n";
  {vnl_decnum b50(vnl_decnum(0L));
   TEST("vnl_decnum b50(vnl_decnum(0L));", b50, 0L);}

  {vnl_decnum b51(vnl_decnum(100L));
   TEST("vnl_decnum b51(vnl_decnum(100L));", b51, 100L);}
}

static void run_conversion_operator_tests()
{
  std::cout << "\nConversion operator tests:\n";

  std::cout << "int conversion operator:\n";
  TEST("int(vnl_decnum(0L)) == 0", int(vnl_decnum(0L)), 0);
  TEST("int(vnl_decnum(0x7fffffffL)) == 0x7fffffff", int(vnl_decnum(0x7fffffffL)), 0x7fffffff);
  TEST("int(vnl_decnum(-0x7fffffffL)) == -0x7fffffff", int(vnl_decnum(-0x7fffffffL)), -0x7fffffff);

  std::cout << "long conversion operator:\n";
  vnl_decnum b(0x7fffffffL); ++b;
  TEST("vnl_decnum b(0x7fffffffL); ++b; (unsigned long)b == 0x80000000UL", (unsigned long)b, 0x80000000UL);
  --b;
  TEST("vnl_decnum b(0x80000000UL); --b; long(b) == 0x7fffffffL", long(b), 0x7fffffffL);
}

static void run_assignment_tests()
{
  std::cout << "\nStarting assignment tests:\n";
  vnl_decnum b1;

  TEST_RUN ("vnl_decnum b1; b1 = 0xffff;", b1 = 0xffffL, long(b1), 0xffffL);

  // long assignment operator
  TEST_RUN ("long(b1) = -0x7fffffff", b1 = -0x7fffffffL, long(b1), -0x7fffffff);

  // char * assignment operator
  TEST_RUN ("long(b1) = 0x1fffffL", b1 = "123456789", long(b1), 123456789L);

  // vnl_decnum& assignment operator
  b1 = "0";
  vnl_decnum b5(0x1ffffL);
  TEST_RUN ("b1 = b5", b1 = b5, b1, b5);
}

static void run_logical_comparison_tests()
{
  std::cout << "\nStarting logical comparison tests:\n";
  vnl_decnum b0(0L);
  vnl_decnum b1(1L);
  vnl_decnum b2(0x7fffL);
  vnl_decnum b3(-0x7fffL);
  vnl_decnum p_inf("+Inf");
  vnl_decnum m_inf("-Inf");

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
}

static void run_division_tests()
{
  std::cout << "\nStarting division tests:\n";

  TEST("long(vnl_decnum(0L)/vnl_decnum(1L))", long(vnl_decnum(0L)/vnl_decnum(1L)), 0L);
  TEST("long(vnl_decnum(-1L)/vnl_decnum(1L))", long(vnl_decnum(-1L)/vnl_decnum(1L)), -1L);
  TEST("long(vnl_decnum(-1L)/vnl_decnum(\"+Inf\"))", long(vnl_decnum(-1L)/vnl_decnum("+Inf")), 0L);
  TEST("vnl_decnum(\"+Inf\")/(-1L)", vnl_decnum("+Inf")/(-1L), vnl_decnum("-Inf"));
  TEST("vnl_decnum(-1L)/0L", vnl_decnum(-1L)/0L, vnl_decnum("-Inf"));
  TEST("vnl_decnum(-0L)/0L", vnl_math::isnan(vnl_decnum(-0L)/0L), true);

  for (long i = 0xffff; i!=0; i /= -16) {
   for (long k = -6; k < 9; k+=3) {
     vnl_decnum bi(i+k);
     for (long j = 0xffff; j!=0; j /= -16) {
      for (long l = -4; l < 9; l+=3) { // avoid division by zero...
       vnl_decnum bj(j+l);
       vnl_decnum bij(long((i+k)/(j+l)));
       {std::stringstream s; s << bi << " / " << bj << " == " << bi/bj << " == " << bij << " ?";
        TEST(s.str().c_str(), bi/bj, bij);}
       bij = long((i+k)%(j+l));
       {std::stringstream s; s << bi << " % " << bj << " == " << bi%bj << " == " << bij << " ?";
        TEST(s.str().c_str(), bi%bj, bij);}
     }}
  }}
}

static void run_large_division_tests()
{
  std::cout << "\nStarting large division tests:\n";

  vnl_decnum a("10000000"), b("10000001"); b *= a; vnl_decnum c = b/10000001;
  std::cout << b << " / 10000001 = " << c << ", must be 10000000\n";
  TEST("100000010000000 / 10000001", c, a);

  // an other example:
  a = "111111"; b = "111111"; b *= a; c = b/111111;
  std::cout << b << " / 111111 = " << c << ", must be 111111\n";
  TEST("12345654321 / 111111", c, a);

  a = "98789"; b = "98789"; b *= a; c = b/98789;
  std::cout << b << " / 98789 = " << c << ", must be 98789\n";
  TEST("9759266521 / 98789", c, a);

  a = "1e100"; b = "1e200"; c = b/a;
  std::cout << "1e200 / 1e100 = "
           << c << ", must be 1e100\n";
  TEST("1e200 / 1e100", c, a);

  a = "-1e100"; b = "1e200"; c = b/a;
  std::cout << "1e200 / -1e100 = "
           << c << ", must be -1e100\n";
  TEST("1e200 / -1e100", c, a);

  a = "1e100"; b = "-1e200"; c = b/a;
  std::cout << "-1e200 / 1e100 = "
           << c << ", must be -1e100\n";
  TEST("-1e200 / 1e100", c, -a);

  a = "-1e100"; b = "-1e200"; c = b/a;
  std::cout << "-1e200 / -1e100 = "
           << c << ", must be 1e100\n";
  TEST("-1e200 / -1e100", c, -a);

  a = "1e100"; b = "1e200"; c = b%a;
  std::cout << "1e200 % 1e100 = "
           << c << ", must be 0\n";
  TEST("1e200 % 1e100", c, 0);

  std::cout << "C(16,8) = " << binom(16,8) << std::endl;
  TEST("16 choose 8 = 12870", binom(16,8), 12870);
  std::cout << "C(18,9) = " << binom(18,9) << std::endl;
  TEST("18 choose 9 = 48620", binom(18,9), 48620);
  std::cout << "C(20,10) = " << binom(20,10) << std::endl;
  TEST("20 choose 10 = 184756", binom(20,10), 184756);
  std::cout << "C(100,44) = " << binom(100,44) << std::endl;
  TEST("100 choose 44 = 49378235797073715747364762200",
       binom(100,44), "49378235797073715747364762200");
}

static void run_multiplication_division_tests()
{
  std::cout << "\nCheck example in book:\n";

  vnl_decnum b2 = "4294967295"; // == 0xffffffff         // Create vnl_decnum object
  vnl_decnum b3 = "12345e30";   // Create vnl_decnum object

  std::cout << "b2 = " << b2 << '\n'
           << "b3 = " << b3 << std::endl;

  TEST("(b2*b3) / b3 = b2", (b2*b3) / b3, b2);
  std::cout << "(b2*b3) / b3 = " << (b2*b3) / b3 << std::endl;
  TEST("(b2*b3) / b2 = b3", (b2*b3) / b2, b3);
  std::cout << "(b2*b3) / b2 = " << (b2*b3) / b2 << std::endl;
  TEST("((b3/b2) * b2) + (b3%b2) = b3", ((b3/b2) * b2) + (b3%b2), b3);
  std::cout << "((b3/b2) * b2) + (b3%b2) = " << ((b3/b2) * b2) + (b3%b2) << std::endl;
}

static void run_addition_subtraction_tests()
{
  std::cout << "\nStarting addition, subtraction tests:\n";

  long i,j;
  vnl_decnum bi,bj,bij;

  {for (i = 1; i < 0xffff;  i *= -3) {
    bi = i;
    for (j = 1; j < 0xffff; j *= -3) {
      bj = j;
      bij = vnl_decnum(i+j);
      {std::stringstream s; s << bi << " + " << bj << " == " << bi+bj << " == " << i+j << " ?";
       TEST(s.str().c_str(), bi+bj, bij);}
      bij = vnl_decnum(i-j);
      {std::stringstream s; s << bi << " - " << bj << " == " << bi-bj << " == " << i-j << " ?";
       TEST(s.str().c_str(), bi-bj, bij);}
    }
  }}

  vnl_decnum b0(0L);
  vnl_decnum zillion("1000000000000000000000000000000000000");
  vnl_decnum b1000000(1000000L), b1000000000000("1000000000000");
  vnl_decnum p_inf("+Inf"), m_inf("-Inf");

  TEST("-p_inf == m_inf", -p_inf, m_inf);
  TEST("-m_inf == p_inf", -m_inf, p_inf);
  TEST("-b0 == b0", -b0, b0);
  TEST("-b1000000 == vnl_decnum(-1L)*b1000000", -b1000000, vnl_decnum(-1L)*b1000000);
  TEST("-(-b1000000000000) == b1000000000000", -(-b1000000000000), b1000000000000);
  TEST("b0 + b1000000 == b1000000", b0 + b1000000, b1000000);
  TEST("b0 + b1000000000000 == b1000000000000", b0 + b1000000000000, b1000000000000);
  TEST("b1000000 + b0 == b1000000", b1000000 + b0, b1000000);
  TEST("b1000000000000 + b0 == b1000000000000", b1000000000000 + b0, b1000000000000);
  TEST("b0 + (-b1000000) == -b1000000", b0 + (-b1000000), -b1000000);
  TEST("-b1000000 + b0 == -b1000000", -b1000000 + b0, -b1000000);
  TEST("-b1000000 + (-b1000000) == (vnl_decnum(2L)*(-b1000000))",
        -b1000000 + (-b1000000),   (vnl_decnum(2L)*(-b1000000)));
  TEST("-b1000000000000 + (-b1000000000000) == (vnl_decnum(2L)*(-b1000000000000))",
        -b1000000000000 + (-b1000000000000), (vnl_decnum(2L)*(-b1000000000000)));
  TEST("b1000000000000 + (-b1000000000000) == b0", b1000000000000 + (-b1000000000000), b0);
  TEST("zillion + (-zillion) == b0", zillion + (-zillion), b0);
  TEST("zillion + b1000000 == b1000000000000*b1000000000000*b1000000000000 + b1000000",
        zillion + b1000000,   b1000000000000*b1000000000000*b1000000000000 + b1000000);
  TEST("zillion + p_inf == p_inf", zillion + p_inf, p_inf);
  TEST("zillion + m_inf == m_inf", zillion + m_inf, m_inf);
  TEST("p_inf + zillion == p_inf", p_inf + zillion, p_inf);
  TEST("m_inf + zillion == m_inf", m_inf + zillion, m_inf);

  TEST("b0 - b1000000 == -b1000000", b0 - b1000000, -b1000000);
  TEST("b0 - b1000000000000 == -b1000000000000", b0 -b1000000000000, -b1000000000000);
  TEST("b1000000 - b0 == b1000000", b1000000 - b0, b1000000);
  TEST("b1000000000000 - b0 == b1000000000000", b1000000000000 - b0, b1000000000000);
  TEST("b0 - (-b1000000) == b1000000", b0 - (-b1000000), b1000000);
  TEST("-b1000000 - b0 == -b1000000", -b1000000 - b0, -b1000000);
  TEST("-b1000000 - (-b1000000) == b0", -b1000000 - (-b1000000), b0);
  TEST("-b1000000 - (-zillion) == zillion - b1000000",
        -b1000000 - (-zillion),   zillion - b1000000);
  TEST("-b1000000000000 - (-b1000000000000) == b0", -b1000000000000 - (-b1000000000000), b0);
  TEST("-b1000000000000 - (b1000000000000) == -vnl_decnum(2L)*b1000000000000",
        -b1000000000000 - (b1000000000000),   -vnl_decnum(2L)*b1000000000000);
  TEST("b1000000000000 - (-b1000000000000) == vnl_decnum(2L)*b1000000000000",
        b1000000000000 - (-b1000000000000),   vnl_decnum(2L)*b1000000000000);
  TEST("zillion - (-zillion) == vnl_decnum(2L)*zillion",
        zillion - (-zillion),   vnl_decnum(2L)*zillion);
  TEST("zillion - b1000000 == b1000000000000*b1000000000000*b1000000000000 - b1000000",
        zillion - b1000000,   b1000000000000*b1000000000000*b1000000000000 - b1000000);
  TEST("-zillion - b1000000 == -b1000000000000*b1000000000000*b1000000000000 - b1000000",
        -zillion - b1000000,   -b1000000000000*b1000000000000*b1000000000000 - b1000000);
  TEST("zillion - p_inf == m_inf", zillion - p_inf, m_inf);
  TEST("zillion - m_inf == p_inf", zillion - m_inf, p_inf);
  TEST("p_inf - zillion == p_inf", p_inf - zillion, p_inf);
  TEST("m_inf - zillion == m_inf", m_inf - zillion, m_inf);

  // example in book
  vnl_decnum b2 = "4294967295"; // == 0xffffffff         // Create vnl_decnum object
  vnl_decnum b3 = "12345e30";   // Create vnl_decnum object
  TEST("(b2+b3) - b2 = b3", (b2+b3) - b2 == b3, 1);
  TEST("(b2+b3) - b3 = b2", (b2+b3) - b3 == b2, 1);
  std::cout << b3 << '\n';
  TEST("cout << b3", 1, 1);
}


static void run_increment_tests()
{
  std::cout << "increment special cases:\n";
  vnl_decnum b1;
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

  vnl_decnum b4("65534");
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


  vnl_decnum b5("-65534");
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

  vnl_decnum b0(0L), b1000(1000L), b1000000(1000000L),
  zillion("1000000000000000000");
  vnl_decnum p_inf("+Inf"), m_inf("-Inf");

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
  vnl_decnum b1(1L);
  vnl_decnum p_inf("+Inf"), m_inf("-Inf");

  // left shift
  TEST("int(b1) == 1",int(b1), 1);
  TEST("long(b1 << 1) == 10",long(b1 << 1), 10);
  TEST("long(b1 << 2) == 100",long(b1 << 2), 100);
  TEST("long(b1 << 3) == 1000",long(b1 << 3), 1000);
  TEST("long(b1 << 4) == 10000",long(b1 << 4), 10000);
  TEST("long(b1 << 5) == 100000",long(b1 << 5), 100000);
  TEST("long(b1 << 6) == 1000000",long(b1 << 6), 1000000);
  TEST("(b1 << 78) == vnl_decnum(\"1e78\")", (b1 << 78), vnl_decnum("1e78"));
  TEST("p_inf << 16 == p_inf",p_inf << 16, p_inf);

  TEST("long(-b1 << 1) == -10",long(-b1 << 1), -10);
  TEST("long(-b1 << 2) == -100",long(-b1 << 2), -100);
  TEST("long(-b1 << 3) == -1000",long(-b1 << 3), -1000);
  TEST("long(-b1 << 4) == -10000",long(-b1 << 4), -10000);
  TEST("long(-b1 << 5) == -100000",long(-b1 << 5), -100000);
  TEST("long(-b1 << 6) == -1000000",long(-b1 << 6), -1000000);
  TEST("(-b1 << 78) == vnl_decnum(\"-1e78\")", (-b1 << 78), vnl_decnum("-1e78"));
  TEST("m_inf << 16 == m_inf",m_inf << 16, m_inf);

  TEST("long(b1 << -16) == 0",long(b1 << -16), 0);
  TEST("long(-b1 << -16) == 0",long(-b1 << -16), 0);
}

static void run_right_shift_tests()
{
  vnl_decnum b2("1e78");
  vnl_decnum p_inf("+Inf"), m_inf("-Inf");

  // right shift
  TEST("b2 == vnl_decnum(\"1e78\")",b2, vnl_decnum("1e78"));
  TEST("(b2 >> 1) == vnl_decnum(\"1e77\")", (b2 >> 1), vnl_decnum("1e77"));
  TEST("long(b2 >> 70) == 100000000",long(b2 >> 70), 100000000);
  TEST("long(b2 >> 78) == 1",long(b2 >> 78), 1L);
  TEST("long(b2 >> 79) == 0",long(b2 >> 79), 0L);
  TEST("long(b2 >> 99) == 0",long(b2 >> 99), 0L);
  TEST("p_inf >> 16 == p_inf",p_inf >> 16, p_inf);

  TEST("-b2 == vnl_decnum(\"-1e78\")",-b2, vnl_decnum("-1e78"));
  TEST("(-b2 >> 1) == vnl_decnum(\"-1e77\")", (-b2 >> 1), vnl_decnum("-1e77"));
  TEST("long(-b2 >> 70) == -100000000",long(-b2 >> 70), -100000000);
  TEST("long(-b2 >> 78) == -1",long(-b2 >> 78), -1L);
  TEST("long(-b2 >> 79) == 0",long(-b2 >> 79), 0L);
  TEST("long(-b2 >> 99) == 0",long(-b2 >> 99), 0L);
  TEST("m_inf >> 16 == m_inf",m_inf >> 16, m_inf);
}

static void run_shift_tests()
{
  std::cout << "\nStarting shift tests:\n";

  run_left_shift_tests();
  run_right_shift_tests();
}

static void run_normalisation_tests()
{
  std::cout << "\nStarting normalisation tests:\n";

  vnl_decnum a("-1e10"), b=a;
  TEST("a stored as -1e10", a.data(), "1");
  TEST("b stored as -1e10", b.data(), "1");
  a += 1L; a -= 1L;
  TEST("a stored as -10000000000", a.data(), "10000000000");
  TEST("a equals b", a, b);
  b = a;
  TEST("b stored as -10000000000", b.data(), "10000000000");
  a.compactify();
  TEST("a stored as -1e10", a.data(), "1");
  TEST("a equals b", a, b);
  b = a;
  TEST("b stored as -1e10", b.data(), "1");
  a.expand();
  TEST("a stored as -10000000000", a.data(), "10000000000");
  TEST("a equals b", a, b);
}

void test_decnum()
{
  run_constructor_tests();
  run_conversion_operator_tests();
  run_assignment_tests();
  run_logical_comparison_tests();
  run_increment_tests();
  run_addition_subtraction_tests();
  run_multiplication_tests();
  run_division_tests();
  run_multiplication_division_tests();
  run_large_division_tests();
  run_shift_tests();
  run_normalisation_tests();
}

TESTMAIN(test_decnum);
