// This is core/vnl/tests/test_sparse_matrix.cxx
#include <iostream>
#include <vcl_compiler.h>
#include <vnl/vnl_sparse_matrix.h>
#include <vnl/vnl_matrix.h>
#include <testlib/testlib_test.h>

static
void test_sparse_int()
{
  std::cout << "********************************\n"
           << " Testing vnl_sparse_matrix<int>\n"
           << "********************************\n";
  vnl_sparse_matrix<int> m0(2000,3000);
  TEST("vnl_sparse_matrix<int> m0(2000,3000)", (m0.rows()==2000 && m0.columns()==3000), true);
  vnl_sparse_matrix<int> m1(3000,4000);
  TEST("vnl_sparse_matrix<int> m1(3000,4000)", (m1.rows()==3000 && m1.columns()==4000), true);
  m0(1111,2222) = m1(1111,2222) = 3333;
  TEST("(vnl_sparse_matrix)(i,j)", m0(1111,2222), 3333);
  const vnl_sparse_matrix<int> ma = m0;
  TEST("copy constructor", ma, m0);
  vnl_sparse_matrix<int> m2(m1);
  TEST("vnl_sparse_matrix<int> m2(m1)", m1, m2);
  TEST("(const vnl_sparse_matrix)(i,j)", ma(1111,2222), 3333);
  vnl_sparse_matrix<int> mb; mb = m0;
  TEST("assignment operator", mb(1111,2222), 3333);
  TEST("m0 == ma", (m0 == ma), true);
  TEST("m1.put(1,1,3)", (m1.put(1,1,3),m1(1,1)==3), true);
  TEST("m1.get(1,1)", m1.get(1,1), 3);
  TEST("!(m0 == m1)", (m0 == m1), false);
  TEST("m0 != m1", (m0 != m1), true);
  TEST("!(m1 == m2)", (m1 == m2), false);

  // test transpose operator
  mb = m0.transpose();
  TEST("transpose size", (mb.rows()==3000 && mb.cols()==2000), true);
  TEST("transpose content", (mb.get(0,0)==0 && mb.get(2222,1111)==3333), true);

  // test additions and subtractions
  TEST("mb = -m0", (mb = -m0, (mb.get(0,0)==0 && mb.get(1111,2222)==-3333)), true);
  TEST("mb=m1+m2", (mb=m1+m2, (mb.get(0,0)==0 && mb.get(1111,2222)==6666 && mb.get(1,1)==3)), true);
  TEST("mb=m1-m2", (mb=m1-m2, (mb.get(0,0)==0 && mb.get(1111,2222)==0 && mb.get(1,1)==3)), true);
  TEST("m1 -= m2", (m1 -= m2, m1), mb);
  TEST("m1 += m2", (m1 += m2, m1), mb+m2);

  /// test multiplications and divisions
  TEST("mb=m1*5", (mb=m1*5, (mb.get(0,0)==0 && mb.get(1,1)==15)), true);
  TEST("mb=5*m1", (mb=5*m1, (mb.get(0,0)==0 && mb.get(1,1)==15)), true);
  TEST("m1*=5",   (m1*=5,   m1), mb);
  TEST("mb=m1/5", (mb=m1/5, (mb.get(0,0)==0 && mb.get(1,1)==3)), true);
  TEST("m1/=5",   (m1/=5,   m1), mb);

  TEST("mb=m0*m1", (mb=m0*m1, mb), vnl_sparse_matrix<int>(2000,4000)); // all-zero matrix
  TEST("m0*=m1", (m0*=m1, m0), mb);
  vnl_sparse_matrix<int> c0(4000,1); c0(2222,0) = 1;
  vnl_sparse_matrix<int> c1;
  TEST("c1=m0*c0", (c1=m0*c0, c1.rows()==m0.rows() && c1.columns()==c0.columns() && c1.get(1111,0)==m0(1111,2222)), true);
  vnl_sparse_matrix<int> r0(1,2000); r0(0,1111) = 1;
  vnl_sparse_matrix<int> r1;
  TEST("r1=r0*m0", (r1=r0*m0, r1.rows()==r0.rows() && r1.columns()==m0.columns() && r1.get(0,2222)==m0(1111,2222)), true);
  TEST("r0*=m0", (r0*=m0, r0==r1), true);

  /// test .mult() with a dense matrix
  vnl_sparse_matrix<int> s0(5,100); s0(0,0) = 1; s0(0,4) = 1;
  vnl_matrix<int> s1(100,4,0); s1(0,0) = 1; s1(4,0) = 1; s1(0,1) = 1; s1(4,1) = 1;
  s1 = s1.transpose(); // s0.mult() expects s1 in col-major order, but s1.data_block() provides row-major order.
  vnl_matrix<int> s2(5,4,0);
  s2 = s2.transpose(); // s0.mult() expects s2 in col-major order, but s2.data_block() provides row-major order.
  TEST("r0.mult(100,4,s1.data_block(),s2.data_block())", \
    (s0.mult(100,4, s1.data_block(), s2.data_block()), \
    s2(0,0) == 2 && s2(1,0) == 2 && s2(3,0) == 0 && s2(0,4) == 0 && s2(3,4) == 0), \
    true);
  // note: s2 was built in row-major order, so the result is actually s2.transpose().

  // Zero-size
#ifdef ZERO_SIZE_TESTS // these tests seem to cause segfaults, on Linux only, for yet unknown reason...
  {
    vnl_sparse_matrix<int> m1(0,3);
    vnl_sparse_matrix<int> m2(3,4);
    vnl_sparse_matrix<int> m3(4,0);
    m2(0,0) = m2(0,1) = m2(0,2) = m2(0,3) = 777;
    m2(1,0) = m2(1,1) = m2(1,2) = m2(1,3) = 888;
    m2(2,0) = m2(2,1) = m2(2,2) = m2(2,3) = 999;
    vnl_sparse_matrix<int> m = m1 * (m2 * m3);
    TEST("zero-size mult rows", m.rows(), 0);
    TEST("zero-size mult cols", m.columns(), 0);

    m = (m1 * m2) * m3;
    TEST("zero-size mult rows", m.rows(), 0);
    TEST("zero-size mult cols", m.columns(), 0);

    m2.clear();
    TEST("empty after clear()", m2, vnl_sparse_matrix<int>(3,4)); // all-zero matrix
  }
#endif // ZERO_SIZE_TESTS
}

void test_sparse_float()
{
  std::cout << "**********************************\n"
           << " Testing vnl_sparse_matrix<float>\n"
           << "**********************************\n";
  vnl_sparse_matrix<float> d0(2,2);
  TEST("vnl_sparse_matrix<float> d0(2,2)", (d0.rows()==2 && d0.columns()==2), true);
  vnl_sparse_matrix<float> d1(3,4);
  TEST("vnl_sparse_matrix<float> d1(3,4)", (d1.rows()==3 && d1.columns()==4), true);
  vnl_sparse_matrix<float> d2(d1);
  TEST("copy constructor", d2, d1);
  TEST("d2.put(1,1,3.f)", (d2.put(1,1,3.f),d2.get(1,1)), 3.f);
  TEST("d2.get(1,1)", d2.get(1,1), 3.f);
  TEST("!(d0 == d2)", (d0 == d2), false);
  TEST("d0 != d2", (d0 != d2), true);
  TEST("d0=d2", (d0=d2,  (d0==d2)), true);
  vnl_sparse_matrix<float> d5(2,2);
  TEST("d5=d0+d2", (d5=d0+d2, (d5.get(0,0)==0.f && d5.get(0,1)==0.f && d5.get(1,0)==0.f && d5.get(1,1)==6.f)), true);
  TEST("d0+=d2",   (d0+=d2,   d0), d5);
  TEST("d0=d2*5.f",(d0=d2*5.f,(d0.get(0,0)==0.f && d0.get(0,1)==0.f && d0.get(1,0)==0.f && d0.get(1,1)==15.f)), true);
  TEST("d2*=5.f",  (d2*=5.f,  d2), d0);
  TEST("transpose", (d1 = d2, d2 = d1.transpose(), (d2.rows()==4 && d2.cols()==3 && d2.get(0,0)==0.f && d2.get(1,1)==15.f)), true);
}

void test_sparse_double()
{
  std::cout << "***********************************\n"
           << " Testing vnl_sparse_matrix<double>\n"
           << "***********************************\n";
  vnl_sparse_matrix<double> d0(2,2);
  TEST("vnl_sparse_matrix<double> d0(2,2)", (d0.rows()==2 && d0.columns()==2), true);
  vnl_sparse_matrix<double> d1(3,4);
  TEST("vnl_sparse_matrix<double> d1(3,4)", (d1.rows()==3 && d1.columns()==4), true);
  vnl_sparse_matrix<double> d2=d0;
  TEST("copy constructor", d0, d2);
  TEST("d2.put(1,1,3.0)", (d2.put(1,1,3.0),d2.get(1,1)), 3.0);
  TEST("d2.get(1,1)", d2.get(1,1), 3.0);
  d2(1,0) = 4.0;
  TEST("d2(1,0) = 4.0", d2(1,0), 4.0);
  TEST("!(d0 == d2)", (d0 == d2), false);
  TEST("d0 != d2", (d0 != d2), true);
  vnl_sparse_matrix<double> d3(d2);
  TEST("vnl_sparse_matrix<double> d3(d2)", d2, d3);
  TEST("d0=d2", (d0=d2,  (d0==d2)), true);

  // normalizations
  d2(0,1) = 7.0;
  d2.normalize_rows();
  TEST("normalize_rows()", d2(0,0)==0.0 && d2(0,1)==1.0, true);
  TEST_NEAR("normalize_rows()", d2(1,0), 0.8, 1e-12);
  TEST_NEAR("normalize_rows()", d2(1,1), 0.6, 1e-12);
}

void test_sparse_complex()
{
  std::cout << "*******************************************\n"
           << " Testing vnl_sparse_matrix<complex_double>\n"
           << "*******************************************\n";
  vnl_sparse_matrix<std::complex<double> > d0(2,2);
  TEST("vnl_sparse_matrix<complex_double> d0(2,2)", (d0.rows()==2 && d0.columns()==2), true);
  vnl_sparse_matrix<std::complex<double> > d1(3,4);
  TEST("vnl_sparse_matrix<complex_double> d1(3,4)", (d1.rows()==3 && d1.columns()==4), true);
  vnl_sparse_matrix<std::complex<double> > d2=d0;
  TEST("copy constructor", d0, d2);
  TEST("d2.put(1,1,3.0)", (d2.put(1,1,3.0),d2.get(1,1)), 3.0);
  TEST("d2.get(1,1)", d2.get(1,1), 3.0);
  std::complex<double> r4i12(4.0, 12.0);
  d2(1,0) = r4i12;
  TEST("d2(1,0) = 4+12i", d2(1,0), r4i12);
  TEST("!(d0 == d2)", (d0 == d2), false);
  TEST("d0 != d2", (d0 != d2), true);
  vnl_sparse_matrix<std::complex<double> > d3(d2);
  TEST("vnl_sparse_matrix<complex_double> d3(d2)", d2, d3);
  TEST("assignment operator", (d0=d2,  (d0==d2)), true);

  d1.put(1,1, 3.0);
  d1.put(1,0, r4i12);
  d1 = d1.conjugate_transpose();
//TEST("Hermitian", (d1.rows()==4 && d1.cols()==3 && d1.get(1,1)==3.0 && d1.get(0,1)==8.0-r4i12), true);
  d1 = d1.transpose();
//TEST("transpose", (d1.rows()==3 && d1.cols()==4 && d1.get(1,1)==3.0 && d1.get(0,1)==0.0), true);

  // normalizations
  d2(0,1) = 7.0;
  d2.normalize_rows();
  TEST("normalize_rows()", d2(0,0)==0.0 && d2(0,1)==1.0, true);
  TEST_NEAR("normalize_rows()", d2(1,0), r4i12/13.0, 1e-12);
  TEST_NEAR("normalize_rows()", d2(1,1), 3.0/13.0, 1e-12);
}

static
void test_sparse_matrix()
{
  test_sparse_int();
  test_sparse_float();
  test_sparse_double();
  test_sparse_complex();
}

TESTMAIN(test_sparse_matrix);
