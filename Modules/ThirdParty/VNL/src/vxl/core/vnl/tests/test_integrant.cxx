// not used? #include <vcl_compiler.h>
#include <iostream>
#include <vnl/vnl_integrant_fnct.h>
#include <testlib/testlib_test.h>

class my_test_integrant : public vnl_integrant_fnct
{
 public:
  double f_(double x) override { return x/(1+x*x); }
};

void test_integrant()
{
  my_test_integrant f;

  TEST_NEAR("test integrand f = x/(1+x^2) when x=1, f is ", f.f_(1), 0.5, 1e-13);
}

TESTMAIN(test_integrant);
