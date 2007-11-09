#include <vcl_iostream.h>
#include <vnl/algo/vnl_convolve.h>
#include <vnl/vnl_double_2.h>
#include <vnl/vnl_random.h>
#include <vnl/vnl_int_2.h>
#include <vul/vul_timer.h>

#include <testlib/testlib_test.h>

#include "test_util.h"

void test_convolve()
{
  int b_data[] = { -2, 0, 4, 6, 2, 0 };
  vnl_vector<int> b(6, 6, b_data);
  vnl_vector<double> k1 = vnl_double_2(0.5,-0.5).as_vector();

  vnl_vector<double> r1 = vnl_convolve(b, k1, (double*)0);
  TEST("vnl_convolve() simple length", r1.size(), 7);
  vcl_cout << r1 << vcl_endl;
  TEST("vnl_convolve() simple values", true,
       r1[0]==-1 && r1[1]==1 && r1[2]==2 && r1[3]==1 && r1[4]==-2 && r1[5]==-1 && r1[6]==0);

  vnl_vector<int> k2 = vnl_int_2(1,-1).as_vector();
  vnl_vector<int> r2 = vnl_convolve(b, k2);
  TEST("vnl_convolve() simple length", r2.size(), 7);
  vcl_cout << r2 << vcl_endl;
  TEST("vnl_convolve() simple values", true,
       r2[0]==-2 && r2[1]==2 && r2[2]==4 && r2[3]==2 && r2[4]==-4 && r2[5]==-2 && r2[6]==0);
  vnl_vector<int> r3 = vnl_convolve(b, k2, (int*)0);
  TEST("vnl_convolve() 2nd form", r3, r2);
  vnl_vector<int> r4 = vnl_convolve(k2, b);
  TEST("vnl_convolve() commutativity", r4, r2);
  vnl_vector<double> r7 = vnl_convolve(b, k1, (double*)0, 1);
  vcl_cout << r7 << vcl_endl;
  TEST_NEAR("vnl_convolve() with_fft(7)", (r7-r1).two_norm(), 0.0, 1e-6);
  vnl_vector<double> r8 = vnl_convolve(b, k1, (double*)0, 8);
  vcl_cout << r8 << vcl_endl;
  TEST_NEAR("vnl_convolve() with_fft(8)", (r8-r1).two_norm(), 0.0, 1e-6);

  // TIMING TEST on a very long convolution:
  vnl_vector<double> l(10000), k3(2000);
  vnl_random rng;
  test_util_fill_random(l.begin(), l.end(), rng);
  test_util_fill_random(k3.begin(), k3.end(), rng);
  const unsigned ntimes = 10; // repeat some expts to get more accurate timings.
  vnl_vector<double> r9;
  vul_timer timer;
  for (unsigned i=0; i < ntimes; ++i)
    r9 = vnl_convolve(l, k3);
  int ms1 = timer.user();
  vcl_cout << "Done straightforward 10000x2000 convolution in " << ms1/double(ntimes) << " milliseconds\n";

  vnl_vector<double> r10;
  timer.mark();
  for (unsigned i=0; i < ntimes; ++i)
    r10 = vnl_convolve(l, k3, 16384);
  int ms2 = timer.user();
  TEST_NEAR("vnl_convolve() with_fft(16384)", (r9-r10).two_norm(), 0.0, 1e-6);
  vcl_cout << "Done FFT-2-based 10000x2000 convolution in " << ms2/double(ntimes) << " milliseconds\n";
  TEST("vnl_convolve() timing: should be at least 2.5x faster", 5*ms2 < 2*ms1, true);

  vnl_vector<double> r11;
  timer.mark();
  for (unsigned i=0; i < ntimes; ++i)
    r11 = vnl_convolve(l, k3, 12800);
  int ms3 = timer.user();
  TEST_NEAR("vnl_convolve() with_fft(12800)", (r9-r11).two_norm(), 0.0, 1e-6);
  vcl_cout << "Done FFT-2,5-based 10000x2000 convolution in " << ms3/double(ntimes) << " milliseconds\n";
  TEST("vnl_convolve() timing: should even be faster", ms3 < ms2, true);

  vnl_vector<double> r12;
  timer.mark();
  for (unsigned i=0; i < ntimes; ++i)
    r12 = vnl_convolve(l, k3, 27648);
  int ms4 = timer.user();
  TEST_NEAR("vnl_convolve() with_fft(27648)", (r9-r12).two_norm(), 0.0, 1e-6);
  vcl_cout << "Done FFT-2,3-based 10000x2000 convolution in " << ms4/double(ntimes) << " milliseconds\n";
  TEST("vnl_convolve() timing: should be slower", 5*ms4 > 3*ms2, true);

  double c1_data[] = { -1, 0, 1, 2, 3, 4 };
  vnl_vector<double> c1(6, 6, c1_data);
  double c2_data[] = { 5, 3, 1, -1, -3, -5 };
  vnl_vector<double> c2(6, 6, c2_data);
  vnl_vector<double> r5 = vnl_convolve_cyclic(c1, c2, (double*)0);
  TEST("vnl_convolve_cyclic() length", r5.size(), 6);
  vcl_cout << r5 << vcl_endl;
  TEST("vnl_convolve_cyclic() values", true,
       r5[0]==5 && r5[1]==-13 && r5[2]==-19 && r5[3]==-13 && r5[4]==5 && r5[5]==35);
  vnl_vector<double> r6 = vnl_convolve_cyclic(c1, c2, (double*)0, true);
  vcl_cout << r6 << vcl_endl;
  TEST_NEAR("vnl_convolve_cyclic() with_fft", (r6-r5).two_norm(), 0.0, 1e-6);
}

TESTMAIN(test_convolve);
