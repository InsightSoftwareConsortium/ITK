// This is core/vnl/tests/test_gamma.cxx
#include <vnl/vnl_gamma.h>
#include <vnl/vnl_erf.h>
#include <vnl/vnl_math.h>
#include <testlib/testlib_test.h>

static void test_gamma()
{
  TEST_NEAR("vnl_log_gamma(1)",      vnl_log_gamma(1),            0,  1e-8);
  TEST_NEAR("vnl_gamma(1))",         vnl_gamma(1),                1,  1e-8);

  // `true' values obtained from http://www.efunda.com/math/gamma/findgamma.cfm
  TEST_NEAR("vnl_gamma(0.5))",       vnl_gamma(0.5),  1.77245385091,  1e-10);
  TEST_NEAR("vnl_gamma(1.5))",       vnl_gamma(1.5),  0.88622692545,  1e-10);
  TEST_NEAR("vnl_gamma(10.7))",      vnl_gamma(10.7), 1799844.07893,  1e-5);

  TEST_NEAR("vnl_gamma_p(2,0)",      vnl_gamma_p(2,0),            0,  1e-8);
  TEST_NEAR("vnl_gamma_p(2,inf)",    vnl_gamma_p(2,1e9),        1.0,  1e-8);

  // Values from MATLAB
  TEST_NEAR("vnl_gamma_p(2.5,1.5)",  vnl_gamma_p(2.5,1.5), 0.300014164,  1e-8);
  TEST_NEAR("vnl_gamma_p(2.5,0.5)",  vnl_gamma_p(2.5,0.5), 0.037434227,  1e-8);
  // Next one not so accurate?
  TEST_NEAR("vnl_gamma_p(10,10)",    vnl_gamma_p(10,10),   0.542070286,  1e-6);

  TEST_NEAR("vnl_gamma_q(2,0)",      vnl_gamma_q(2,0),          1.0,  1e-8);
  TEST_NEAR("vnl_gamma_q(2,inf)",    vnl_gamma_q(2,1e9),          0,  1e-8);
  TEST_NEAR("vnl_erf(0)",            vnl_erf(0),                  0,  1e-8);
  TEST_NEAR("vnl_erf(inf)",          vnl_erf(1e9),              1.0,  1e-8);
  TEST_NEAR("vnl_erf(-inf)",         vnl_erf(-1e9),            -1.0,  1e-8);
  TEST("vnl_erfc(0)",           vnl_erfc(0.0),                 1.0);
  TEST_NEAR("vnl_erfc(0.2) = 1-vnl_erf(0.2)", vnl_erfc(0.2), 1.0-vnl_erf(0.2), 1e-8);
  TEST_NEAR("vnl_erfc(1.2) = 1-vnl_erf(1.2)", vnl_erfc(1.2), 1.0-vnl_erf(1.2), 1e-6);
  TEST_NEAR("vnl_erfc(2)",           vnl_erfc(2),   0.00467773498105, 1e-8);
  TEST_NEAR_REL("vnl_erfc(6)",       vnl_erfc(6),   2.15197367125e-17,1e-8);
  TEST_NEAR("vnl_erfc(inf)",         vnl_erfc(1e9),             0.0,  1e-8);
  TEST_NEAR("vnl_erfc(-inf)",        vnl_erfc(-1e9),            2.0,  1e-8);

  TEST_NEAR("vnl_scaled_erfc(6)",    vnl_scaled_erfc(6.), vcl_exp(36.)*vnl_erfc(6.), 0.01);
  TEST_NEAR("vnl_scaled_erfc(100)",  vnl_scaled_erfc(100.),   0.0056, 0.01);
  TEST_NEAR("vnl_scaled_erfc(-inf)", vnl_scaled_erfc(-1e9),  0.0,  1e-8);
  TEST_NEAR("vnl_digamma(1)",        vnl_digamma(1), -vnl_math::euler, 1e-10);
  TEST_NEAR("vnl_digamma(20)",       vnl_digamma(20), 2.970523992242149, 1e-10);
}

TESTMAIN(test_gamma);
