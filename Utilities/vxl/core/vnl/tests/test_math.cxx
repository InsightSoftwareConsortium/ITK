#include <vcl_iostream.h>
#include <vcl_limits.h> // for infinity()
#include <vnl/vnl_math.h>
#include <vnl/vnl_complex.h>
#include <testlib/testlib_test.h>

static
void check_pointer( const void * )
{
}

static
void test_static_const_definition()
{
  check_pointer( &vnl_math::e );
  check_pointer( &vnl_math::log2e );
  check_pointer( &vnl_math::log10e );
  check_pointer( &vnl_math::ln2 );
  check_pointer( &vnl_math::ln10 );
  check_pointer( &vnl_math::pi );
  check_pointer( &vnl_math::pi_over_2 );
  check_pointer( &vnl_math::pi_over_4 );
  check_pointer( &vnl_math::one_over_pi );
  check_pointer( &vnl_math::two_over_pi );
  check_pointer( &vnl_math::two_over_sqrtpi );
  check_pointer( &vnl_math::sqrt2 );
  check_pointer( &vnl_math::sqrt1_2 );
  check_pointer( &vnl_math::eps );
  check_pointer( &vnl_math::sqrteps );
}

void test_math()
{
  // Call it to avoid compiler warnings
  test_static_const_definition();

  int n = -11;
  float f = -7.5;
  double d = -vnl_math::pi;
  vcl_complex<double> i(0,1);
  vcl_complex<double> z(-1,2);
  vcl_complex<double> e_ipi = vcl_exp(d*i);

  vcl_cout << "n = " << n << vcl_endl
           << "f = " << f << vcl_endl
           << "d = " << d << vcl_endl
           << "i = " << i << vcl_endl
           << "z = " << z << vcl_endl
           << "exp(d*i) = " << e_ipi << vcl_endl
           << vcl_endl

           << "abs(n) = " << vnl_math_abs(n) << vcl_endl
           << "abs(f) = " << vnl_math_abs(f) << vcl_endl
           << "abs(d) = " << vnl_math_abs(d) << vcl_endl
           << "abs(i) = " << vnl_math_abs(i) << vcl_endl
           << "abs(z) = " << vnl_math_abs(z) << vcl_endl
           <<"norm(z) = " << vnl_math_squared_magnitude(z) << vcl_endl
           << vcl_endl;

  testlib_test_assert("abs(n) == 11", vnl_math_abs(n) == 11);
  testlib_test_assert("abs(f) == 7.5", vnl_math_abs(f) == 7.5);
  testlib_test_assert("abs(d) == pi", vnl_math_abs(d) == vnl_math::pi);
  testlib_test_assert("abs(i) == 1", vnl_math_abs(i) == 1.0);
  testlib_test_assert_near("abs(-1+2i)~=sqrt(5)",vnl_math_abs(z),vcl_sqrt(5.0));
  testlib_test_assert_near("norm(-1+2i) ~= 5", vnl_math_squared_magnitude(z),5);
  testlib_test_assert_near("exp(d*i) ~= -1", vnl_math_abs(e_ipi+1.0), 0);
  vcl_cout << vcl_endl;

  testlib_test_assert(" isfinite(f)    ",  vnl_math_isfinite(f));
  testlib_test_assert(" isfinite(d)    ",  vnl_math_isfinite(d));
  testlib_test_assert(" isfinite(i)    ",  vnl_math_isfinite(i));
  testlib_test_assert(" isfinite(z)    ",  vnl_math_isfinite(z));


  // There is an assumption in this code that vcl_numeric_limits<float/double>::has_infinity==true

  TEST("vcl_numeric_limits<float>::has_infinity==true assumption",vcl_numeric_limits<float>::has_infinity,true);
  TEST("vcl_numeric_limits<double>::has_infinity==true assumption",vcl_numeric_limits<double>::has_infinity,true);
  if ((! vcl_numeric_limits<float>::has_infinity) || (! vcl_numeric_limits<double>::has_infinity))
  {
    vcl_cout << "Your platform doesn't appear to have an infinity. VXL is in places relatively\n"
             << "dependent on the existence of an infinity. There are two solutions.\n"
             << "A. If your platform really doesn have an infinity, VXL's configuration code\n"
             << "   can be modified to correctly detect and use the infinity.\n"
             << "B. Fix VXL so that it can cope with the lack of an infinity.\n" << vcl_endl;
  }
  // Create Inf and -Inf:
  float pinf_f =   vcl_numeric_limits<float>::infinity();
  float ninf_f = - vcl_numeric_limits<float>::infinity();
  double pinf_d =   vcl_numeric_limits<double>::infinity();
  double ninf_d = - vcl_numeric_limits<double>::infinity();
  long double pinf_q =   vcl_numeric_limits<long double>::infinity();
  long double ninf_q = - vcl_numeric_limits<long double>::infinity();

  // Create NaN
  float qnan_f = vcl_numeric_limits<float>::quiet_NaN();
  double qnan_d = vcl_numeric_limits<double>::quiet_NaN();
  long double qnan_q = vcl_numeric_limits<long double>::quiet_NaN();

  vcl_cout << "pinf_f = " << pinf_f << vcl_endl
           << "ninf_f = " << ninf_f << vcl_endl
           << "pinf_d = " << pinf_d << vcl_endl
           << "ninf_d = " << ninf_d << vcl_endl
           << "pinf_q = " << pinf_q << vcl_endl
           << "ninf_q = " << ninf_q << vcl_endl
           << "qnan_f = " << qnan_f << vcl_endl
           << "qnan_d = " << qnan_d << vcl_endl
           << "qnan_q = " << qnan_q << vcl_endl
           << vcl_endl;

#ifndef __alpha__ // on alpha, infinity() == max()
  testlib_test_assert("!isfinite(pinf_f)", !vnl_math_isfinite(pinf_f));
  testlib_test_assert(" isinf(pinf_f)   ",  vnl_math_isinf(pinf_f));
  testlib_test_assert("!isfinite(ninf_f)", !vnl_math_isfinite(ninf_f));
  testlib_test_assert(" isinf(ninf_f)   ",  vnl_math_isinf(ninf_f));
#endif
  testlib_test_assert("!isnan(pinf_f)   ", !vnl_math_isnan(pinf_f));
  testlib_test_assert("!isnan(ninf_f)   ", !vnl_math_isnan(ninf_f));
  testlib_test_assert("!isfinite(qnan_f)", !vnl_math_isfinite(qnan_f));
  testlib_test_assert("!isinf(qnan_f)   ", !vnl_math_isinf(qnan_f));
  testlib_test_assert(" isnan(qnan_f)   ",  vnl_math_isnan(qnan_f));

#ifndef __alpha__ // on alpha, infinity() == max()
  testlib_test_assert("!isfinite(pinf_d)", !vnl_math_isfinite(pinf_d));
  testlib_test_assert(" isinf(pinf_d)   ",  vnl_math_isinf(pinf_d));
  testlib_test_assert("!isfinite(ninf_d)", !vnl_math_isfinite(ninf_d));
  testlib_test_assert(" isinf(ninf_d)   ",  vnl_math_isinf(ninf_d));
#endif
  testlib_test_assert("!isnan(pinf_d)   ", !vnl_math_isnan(pinf_d));
  testlib_test_assert("!isnan(ninf_d)   ", !vnl_math_isnan(ninf_d));
  testlib_test_assert("!isfinite(qnan_d)", !vnl_math_isfinite(qnan_d));
  testlib_test_assert("!isinf(qnan_d)   ", !vnl_math_isinf(qnan_d));
  testlib_test_assert(" isnan(qnan_d)   ",  vnl_math_isnan(qnan_d));

#ifndef __alpha__ // on alpha, infinity() == max()
  testlib_test_assert("!isfinite(pinf_q)", !vnl_math_isfinite(pinf_q));
  testlib_test_assert(" isinf(pinf_q)   ",  vnl_math_isinf(pinf_q));
  testlib_test_assert("!isfinite(ninf_q)", !vnl_math_isfinite(ninf_q));
  testlib_test_assert(" isinf(ninf_q)   ",  vnl_math_isinf(ninf_q));
#endif
  testlib_test_assert("!isnan(pinf_q)   ", !vnl_math_isnan(pinf_q));
  testlib_test_assert("!isnan(ninf_q)   ", !vnl_math_isnan(ninf_q));
  testlib_test_assert("!isfinite(qnan_q)", !vnl_math_isfinite(qnan_q));
  testlib_test_assert("!isinf(qnan_q)   ", !vnl_math_isinf(qnan_q));
  testlib_test_assert(" isnan(qnan_q)   ",  vnl_math_isnan(qnan_q));
}

TESTMAIN(test_math);
