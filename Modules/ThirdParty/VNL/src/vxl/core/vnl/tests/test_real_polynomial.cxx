#include <iostream>
#include <sstream>

#include <testlib/testlib_test.h>
#include <vnl/vnl_real_polynomial.h>
#include <vnl/vnl_vector.h>
#include <vcl_compiler.h>

void test_real_polynomial()
{
  vnl_real_polynomial f1(3u);
  vnl_real_polynomial f2(4); //Test initialization with signed value.

  for (int i=0;i<=f1.degree();++i) f1[i]=i+1; // f1 = X^3 +2 X^2 +3 X + 4
  f1.print(std::cout); std::cout << std::endl;
  { std::stringstream testStream;
    f1.print(testStream);
    std::string expected = " X^3 +2 X^2 +3 X +4";
    TEST("f1 prints as X^3 +2 X^2 +3 X +4", testStream.str(), expected);
    std::cout << "Actual:\t\t\"" << testStream.str() << '"' << std::endl
             << "Expected:\t\"" << expected << '"' << std::endl;
  }

  for (int i=0;i<=f2.degree();++i) f2[i]=2*i+1; // f2 = X^4 +3 X^3 +5 X^2 +7 X +9
  f2.print(std::cout); std::cout << std::endl;

  vnl_real_polynomial f3 = f1+f2;
  f3.print(std::cout); std::cout << std::endl;
  TEST("f1+f2=f3",f1.evaluate(2.5)+f2.evaluate(2.5), f3.evaluate(2.5));
  // Evaluating in 2.5 is exact, since 2.5 is exactly representable (binary 10.1)

  vnl_real_polynomial f4 = f1-f2;
  f4.print(std::cout); std::cout << std::endl;
  TEST("f1-f2=f4",f1.evaluate(2.5)-f2.evaluate(2.5), f4.evaluate(2.5));

  vnl_real_polynomial f5 = f1*f2;
  f5.print(std::cout); std::cout << std::endl;

  TEST("f1*f2 has correct degree",f5.degree()==(f1.degree()+f2.degree()),true);

  TEST("f1*f2=f5",f1.evaluate(2.5)*f2.evaluate(2.5), f5.evaluate(2.5));

  vnl_real_polynomial f1d = f1.derivative();
  f1d.print(std::cout); std::cout << std::endl;
  vnl_real_polynomial f2d = f2.derivative();
  f2d.print(std::cout); std::cout << std::endl;

  TEST("Derivative", f5.derivative(), f1d*f2+f2d*f1);

  vnl_real_polynomial f5p = (f1d*f2+f2d*f1).primitive();
  f5p.print(std::cout); std::cout << std::endl;

  TEST("Primitive", f5p, f5-f5.evaluate(0.0));

  TEST_NEAR("Integral", f1.evaluate_integral(2.0), 70.0/3, 1e-9);

  TEST("Polynomial of degree 0", vnl_real_polynomial(1.0).evaluate(0.5),1.0);
  double v_data[] = {3.0,2.0,1.0}; vnl_vector<double> v(v_data, 3);
  TEST("Vector initialisation", vnl_real_polynomial(v).evaluate(2.0),17.0);

  TEST_NEAR("RMS difference(f1,f2)",vnl_rms_difference(f1,f1,0,1),0.0,1e-9);

  vnl_real_polynomial f6(1u); //f6 = X + 1
  f6[0] = f6[1] = 1;
  { std::stringstream testStream;
    f6.print(testStream);
    std::string expected = " X +1";
    TEST("f6 prints as X +1", testStream.str(), expected);
    std::cout << "Actual:\t\t\"" << testStream.str() << '"' << std::endl
             << "Expected:\t\"" << expected << '"' << std::endl;
  }

  vnl_real_polynomial f7(1u); //f7 = X - 1
  f7[0] = 1;
  f7[1] = -1;
  { std::stringstream testStream;
    f7.print(testStream);
    std::string expected = " X -1";
    TEST("f7 prints as X -1", testStream.str(), expected);
    std::cout << "Actual:\t\t\"" << testStream.str() << '"' << std::endl
             << "Expected:\t\"" << expected << '"' << std::endl;
  }

  vnl_real_polynomial f8(1u); //f8 = 0
  f8[0] = 0;
  f8[1] = 0;
  { std::stringstream testStream;
    f8.print(testStream);
    std::string expected = " 0";
    TEST("f8 prints as 0", testStream.str(), expected);
    std::cout << "Actual:\t\t\"" << testStream.str() << '"' << std::endl
             << "Expected:\t\"" << expected << '"' << std::endl;
  }
}

TESTMAIN(test_real_polynomial);
