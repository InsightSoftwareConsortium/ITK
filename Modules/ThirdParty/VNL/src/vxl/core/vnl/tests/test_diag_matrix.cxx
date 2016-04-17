// This is core/vnl/tests/test_diag_matrix.cxx
#include <iostream>
#include <exception>
#include <testlib/testlib_test.h>
//:
// \file
// \author Peter Vanroose, KULeuven
// \date 20 Sept. 2002

#include <vnl/vnl_diag_matrix.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_vector_fixed.h>
#include <vcl_compiler.h>

void test_diag_matrix()
{
  const unsigned int N = 3;

  vnl_diag_matrix<double> m1(N);
  for (unsigned i=0; i<N; i++)
    m1(i,i) = i*0.25-0.25;

  vnl_diag_matrix<double> const m1c=m1; // must be const in order to use m(i,j)
  std::cout << "m1 = " << m1 << " =\n";
  for (unsigned i=0; i<N; i++) {
    std::cout << '\t';
    for (unsigned j=0; j<N; j++)
      std::cout << m1c(i,j) << ' ';
    std::cout << '\n';
  }

  vnl_diag_matrix<double> m2(N);
  for (unsigned i=0; i<N; i++)
    m2(i,i) = 2.0-1.25*i;

  vnl_diag_matrix<double> const m2c=m2;
  std::cout << "m2 = " << m2 << " =\n";
  for (unsigned i=0; i<N; i++) {
    std::cout << '\t';
    for (unsigned j=0; j<N; j++)
      std::cout << m2c(i,j) << ' ';
    std::cout << '\n';
  }

  vnl_diag_matrix<double> const sum = m1 + m2;
  std::cout << "sum = " << sum << '\n';
  TEST("sum", sum(0,0) == 1.75 && sum(1,1) == 0.75 && sum(2,2) == -0.25, true);

  vnl_diag_matrix<double> const diff = m1 - m2;
  std::cout << "difference = " << diff << " =\n";
  TEST("difference", diff(0,0) == -2.25 && diff(1,1) == -0.75 && diff(2,2) == 0.75, true);

  vnl_diag_matrix<double> const prod = m1 * m2;
  std::cout << "product = " << prod << '\n';
  TEST("product", prod(0,0) == -0.5 && prod(1,1) == 0.0 && prod(2,2) == -0.125, true);

  vnl_vector_fixed<double,N> th; th(0)=2.0; th(1)=-3.0; th(2)=6.0;
  std::cout << "vector = " << th << '\n';

  vnl_vector<double> vec = m1 * th.as_ref();
  std::cout << "vector product (post-multiplied) = " << vec << '\n';
  TEST("vector product", vec(0) == -0.5 && vec(1) == 0.0 && vec(2) == 1.5, true);

  vec = th.as_ref() * m1;
  std::cout << "vector product (pre-multiplied) = " << vec << '\n';
  TEST("vector product", vec(0) == -0.5 && vec(1) == 0.0 && vec(2) == 1.5, true);

  vnl_matrix_fixed<double,N,N> mat;
  mat(0,0)=0.0;    mat(0,1)=-th(2); mat(0,2)= th(1);
  mat(1,0)= th(2); mat(1,1)=0.0;    mat(1,2)=-th(0);
  mat(2,0)=-th(1); mat(2,1)= th(0); mat(2,2)=0.0;
  std::cout << "matrix =\n" << mat;

  vnl_matrix_fixed<double,N,N> s1=m1+mat.as_ref();
  std::cout << "m1+matrix =\n" << s1;
  TEST("matrix sum", s1(0,0) == -0.25 && s1(0,1) == -6.0 && s1(0,2) == -3.0
                  && s1(1,0) ==  6.0  && s1(1,1) ==  0.0 && s1(1,2) == -2.0
                  && s1(2,0) ==  3.0  && s1(2,1) ==  2.0 && s1(2,2) == 0.25, true);
  vnl_matrix_fixed<double,N,N> s2=mat.as_ref()+m1;
  std::cout << "matrix+m1 =\n" << s2;
  TEST("matrix sum", s2(0,0) == -0.25 && s2(0,1) == -6.0 && s2(0,2) == -3.0
                  && s2(1,0) ==  6.0  && s2(1,1) ==  0.0 && s2(1,2) == -2.0
                  && s2(2,0) ==  3.0  && s2(2,1) ==  2.0 && s2(2,2) == 0.25, true);
  vnl_matrix_fixed<double,N,N> d1=m1-mat.as_ref();
  std::cout << "m1-matrix =\n" << d1;
  TEST("matrix difference", d1(0,0) == -0.25 && d1(0,1) ==  6.0 && d1(0,2) == 3.0
                         && d1(1,0) == -6.0  && d1(1,1) ==  0.0 && d1(1,2) == 2.0
                         && d1(2,0) == -3.0  && d1(2,1) == -2.0 && d1(2,2) == 0.25, true);
  vnl_matrix_fixed<double,N,N> d2=mat.as_ref()-m1;
  std::cout << "matrix-m1 =\n" << d2;
  TEST("matrix difference", d2(0,0) == 0.25 && d2(0,1) == -6.0 && d2(0,2) == -3.0
                         && d2(1,0) == 6.0  && d2(1,1) == -0.0 && d2(1,2) == -2.0
                         && d2(2,0) == 3.0  && d2(2,1) ==  2.0 && d2(2,2) == -0.25, true);
  vnl_matrix_fixed<double,N,N> p1=m1*mat.as_ref();
  std::cout << "m1*matrix =\n" << p1;
  TEST("matrix product", p1(0,0) == 0.0  && p1(0,1) == 1.5 && p1(0,2) == 0.75
                      && p1(1,0) == 0.0  && p1(1,1) == 0.0 && p1(1,2) == 0.0
                      && p1(2,0) == 0.75 && p1(2,1) == 0.5 && p1(2,2) == 0.0, true);
  vnl_matrix_fixed<double,N,N> p2=mat.as_ref()*m1;
  std::cout << "matrix*m1 =\n" << p2;
  TEST("matrix product", p2(0,0) ==  0.0  && p2(0,1) == 0.0 && p2(0,2) == -0.75
                      && p2(1,0) == -1.5  && p2(1,1) == 0.0 && p2(1,2) == -0.5
                      && p2(2,0) == -0.75 && p2(2,1) == 0.0 && p2(2,2) ==  0.0, true);

  ///////////////
  // ACCESSORS //
  ///////////////

#if VNL_CONFIG_CHECK_BOUNDS

  {
  // Get
  bool exceptionThrownAndCaught = false;
  try { m1.get(25,25); }  // Raise out of bounds exception.
  catch(...) { exceptionThrownAndCaught = true; }
  TEST("Out of bounds get(25,25)", exceptionThrownAndCaught, true);

  // Put
  exceptionThrownAndCaught = false;
  try { m1.put(25,25,0); }  // Raise out of bounds exception.
  catch(...) { exceptionThrownAndCaught = true; }
  TEST("Out of bounds put(25,25,0)", exceptionThrownAndCaught, true);
  }

#endif

}

TESTMAIN(test_diag_matrix);
