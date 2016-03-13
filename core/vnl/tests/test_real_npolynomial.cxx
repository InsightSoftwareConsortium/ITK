#include <iostream>
#include <vcl_compiler.h>

#include <testlib/testlib_test.h>
#include <vnl/vnl_real_npolynomial.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

void test_real_npolynomial()
{
  vnl_vector<double> coef_0(3), coef_1(3), coef_2(4);
  for (unsigned int i=0;i<3;++i) coef_0(i)=i+1.0;   // f0 = X + 2Y + 3Z
  for (unsigned int i=0;i<3;++i) coef_1(i)=i+1.0;   // f1 = X^2 + 2XY^3 + 3
  for (unsigned int i=0;i<4;++i) coef_2(i)=2*i+1.0; // f2 = X^3 + 3X^2Y + 5XY^2 + 7Y^3

  vnl_matrix<unsigned int> expo_0(3,3, 0U), expo_1(3,2, 0U), expo_2(4,2, 0U);
  for (unsigned int i=0;i<3;++i) expo_0(i,i)=1;
  expo_1(0,0)=2; expo_1(1,0)=1; expo_1(1,1)=3;
  for (unsigned int i=0;i<4;++i) expo_2(i,1)=expo_2(3-i,0)=i;

  vnl_real_npolynomial f0(coef_0,expo_0), f1(coef_1,expo_1), f2(coef_2,expo_2);

  std::cout << "f0 = " << f0 << "f1 = " << f1 << "f2 = " << f2;
  TEST("f0 has total degree 1", f0.degree(), 1);
  TEST("f0 has maximal degree 1", f0.maxdegree(), 1);
  TEST("f0 has degree 1 in X", f0.degrees()[0], 1);
  TEST("f0 has degree 1 in Y", f0.degrees()[1], 1);
  TEST("f0 has degree 1 in Z", f0.degrees()[2], 1);

  TEST("f1 has total degree 4", f1.degree(), 4);
  TEST("f1 has maximal degree 3", f1.maxdegree(), 3);
  TEST("f1 has degree 2 in X", f1.degrees()[0], 2);
  TEST("f1 has degree 3 in Y", f1.degrees()[1], 3);

  TEST("f2 has total degree 3", f2.degree(), 3);
  TEST("f2 has maximal degree 3", f2.maxdegree(), 3);
  TEST("f2 has degree 3 in X", f2.degrees()[0], 3);
  TEST("f2 has degree 3 in Y", f2.degrees()[1], 3);

  vnl_vector<double> vec3(2); vec3(0)=vec3(1)=2.5;

  vnl_real_npolynomial f3 = f1+f2;
  std::cout << "f1+f2 = " << f3;
  TEST("f1+f2=f3",f1.eval(vec3)+f2.eval(vec3), f3.eval(vec3));

  TEST("f3 has total degree 4", f3.degree(), 4);
  TEST("f3 has maximal degree 3", f3.maxdegree(), 3);
  TEST("f3 has degree 3 in X", f3.degrees()[0], 3);
  TEST("f3 has degree 3 in Y", f3.degrees()[1], 3);

  vnl_real_npolynomial f4 = f1-f2;
  std::cout << "f1-f2 = " << f4;
  TEST("f1-f2=f4",f1.eval(vec3)-f2.eval(vec3), f4.eval(vec3));

  TEST("f4 has total degree 4", f4.degree(), 4);
  TEST("f4 has maximal degree 3", f4.maxdegree(), 3);
  TEST("f4 has degree 3 in X", f4.degrees()[0], 3);
  TEST("f4 has degree 3 in Y", f4.degrees()[1], 3);

  vnl_real_npolynomial f5 = f1*f2;
  std::cout << "f1*f2 = " << f5;
  TEST("f1*f2=f5",f1.eval(vec3)*f2.eval(vec3), f5.eval(vec3));

  TEST("f5 has total degree 7", f5.degree(), 7);
  TEST("f5 has maximal degree 6", f5.maxdegree(), 6);
  TEST("f5 has degree 5 in X", f5.degrees()[0], 5);
  TEST("f5 has degree 6 in Y", f5.degrees()[1], 6);

  TEST("f1*f2 has correct total degree",f5.degree(), f1.degree()+f2.degree());
  TEST("f1*f2 has correct maximal degree",f5.maxdegree(), f1.maxdegree()+f2.maxdegree());
  TEST("f1*f2 has correct degree in X",f5.degrees()[0], f1.degrees()[0]+f2.degrees()[0]);
  TEST("f1*f2 has correct degree in Y",f5.degrees()[1], f1.degrees()[1]+f2.degrees()[1]);
}

TESTMAIN(test_real_npolynomial);
