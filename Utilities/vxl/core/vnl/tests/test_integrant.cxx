#include <vcl_iostream.h>
#include <vnl/vnl_analytic_integrant.h>
#include <testlib/testlib_test.h>

class my_test_integrant : public vnl_integrant_fnct
{
  public:
    double f_(double x) { return x/(1+x*x); }
};

void test_integrant()
{
  my_test_integrant f;
  
  TEST_NEAR("test integrant f = x/(1+x^2) when x=1, f is ", f.f_(1), 0.5, 1e-13);

}

TESTMAIN(test_integrant);
