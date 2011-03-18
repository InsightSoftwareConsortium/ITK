// This is core/vnl/tests/test_bessel.cxx
#include <vnl/vnl_bessel.h>
// not used? #include <vcl_fstream.h>
#include <testlib/testlib_test.h>


static void test_bessel()
{
  TEST_NEAR("J_0(0)=1.0",vnl_bessel(0,0.0),1.0,1e-8);

  // First zero of Bessel function of order 0
  TEST_NEAR("J_0(2.4048)=0.0",vnl_bessel(0,2.4048),0.0,1e-4);
  // Second zero of Bessel function of order 0
  TEST_NEAR("J_0(5.5201)=0.0",vnl_bessel(0,5.5201),0.0,1e-4);

  TEST_NEAR("J_1(0)=0.0",vnl_bessel(1,0.0),0.0,1e-5);

  // First zero of Bessel function of order 1
  TEST_NEAR("J_1(3.8315)=0.0",vnl_bessel(1,3.8315),0.0,1e-4);

  TEST_NEAR("bessel0(1.23)",vnl_bessel0(1.23),vnl_bessel(0,1.23),1e-8);
  TEST_NEAR("bessel0(0.23)",vnl_bessel0(0.23),vnl_bessel(0,0.23),1e-8);
  TEST_NEAR("bessel0(0.001)",vnl_bessel0(0.001),1.0,1e-5);

  // Test consistency
  vnl_vector<double> J(3);
  vnl_bessel(2,1.234,J);
  TEST_NEAR("bessel(0,1.234)",vnl_bessel(0,1.234),J[0],1e-8);
  TEST_NEAR("bessel(1,1.234)",vnl_bessel(1,1.234),J[1],1e-8);
  TEST_NEAR("bessel(2,1.234)",vnl_bessel(2,1.234),J[2],1e-8);
}

TESTMAIN(test_bessel);
