#include <vcl_iostream.h>

#include <testlib/testlib_test.h>
#include <vnl/vnl_real_polynomial.h>
#include <vcl_cmath.h>

void test_real_polynomial()
{
  vnl_real_polynomial f1(3),f2(4);

  for (int i=0;i<=f1.degree();++i) f1[i]=i+1; // f1 = X^2 + 2X + 3
  f1.print(vcl_cout);
  for (int i=0;i<=f2.degree();++i) f2[i]=2*i+1; // f2 = X^3 + 3X^2 + 5X + 7
  f2.print(vcl_cout);

  vnl_real_polynomial f3 = f1+f2;
  f3.print(vcl_cout);
  TEST("f1+f2=f3",f1.evaluate(2.5)+f2.evaluate(2.5), f3.evaluate(2.5));
  // Evaluating in 2.5 is exact, since 2.5 is exactly representable (binary 10.1)

  vnl_real_polynomial f4 = f1-f2;
  f4.print(vcl_cout);
  TEST("f1-f2=f4",f1.evaluate(2.5)-f2.evaluate(2.5), f4.evaluate(2.5));

  vnl_real_polynomial f5 = f1*f2;
  f5.print(vcl_cout);

  TEST("f1*f2 has correct degree",f5.degree()==(f1.degree()+f2.degree()),true);

  TEST("f1*f2=f5",f1.evaluate(2.5)*f2.evaluate(2.5), f5.evaluate(2.5));

  vnl_real_polynomial f1d = f1.derivative();
  f1d.print(vcl_cout);
  vnl_real_polynomial f2d = f2.derivative();
  f2d.print(vcl_cout);

  TEST("Derivative", f5.derivative(), f1d*f2+f2d*f1);

  vnl_real_polynomial f5p = (f1d*f2+f2d*f1).primitive();
  f5p.print(vcl_cout);

  TEST("Primitive", f5p, f5-f5.evaluate(0.0));

  TEST("Integral",vcl_fabs(3*f1.evaluate_integral(2.0)-70)<1e-6, true);

  TEST("RMS difference(f1,f2) is zero",vnl_rms_difference(f1,f1,0,1)<1e-8,true);
}

TESTMAIN(test_real_polynomial);
