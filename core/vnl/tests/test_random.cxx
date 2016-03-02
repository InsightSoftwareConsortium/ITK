// This is core/vnl/tests/test_random.cxx
#include <vcl_iostream.h>
#include <vcl_cmath.h> // for vcl_sqrt()
#include <vnl/vnl_random.h>
#include <testlib/testlib_test.h>

void test_random()
{
  vcl_cout << "********************\n"
           << " Testing vnl_random\n"
           << "********************\n";

  vnl_random mz_random;
  mz_random.reseed(123456);

  TEST("lrand32",mz_random.lrand32(),3501493769ul);
  TEST("lrand32(0,10)",mz_random.lrand32(0,10),9);
  double d1 = mz_random.drand32(0,1);
  TEST_NEAR("drand32(0,1)", d1, 0.6158541, 1e-7);
  double d2 = mz_random.drand64(0,1);
  TEST_NEAR("drand64(0,1)", d2, 0.2257411, 1e-7);

  double sum = 0.0;
  double sum_sq = 0.0;
  int n = 10000;
  for (int i=0;i<n;++i)
  {
    double r = mz_random.normal();
    sum += r;
    sum_sq += r*r;
  }

  double mean = sum/n;
  double var  = vcl_sqrt(sum_sq/n-mean*mean);
  TEST_NEAR("normal() mean near zero",mean, 0.0, 0.01);
  TEST_NEAR("normal() var near one",var, 1.0, 0.02);

  sum = 0.0;
  sum_sq = 0.0;
  for (int i=0;i<n;++i)
  {
    double r = mz_random.normal64();
    sum += r;
    sum_sq += r*r;
  }

  mean = sum/n;
  var  = vcl_sqrt(sum_sq/n-mean*mean);
  TEST_NEAR("normal64() mean near zero",mean, 0.0, 0.01);
  TEST_NEAR("normal64() var near one",var, 1.0, 0.01);
}

TESTMAIN(test_random);
