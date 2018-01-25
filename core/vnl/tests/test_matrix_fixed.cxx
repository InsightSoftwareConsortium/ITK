// This is core/vnl/tests/test_matrix_fixed.cxx
#include <cstdlib>
#include <cstddef>
#include <cmath>
#include <iostream>
#include <exception>
#include <cstdio>
#ifdef TEST_MALLOC // see note below, at the other #ifdef TEST_MALLOC
# include <vcl_new.h>
#endif
#include <vcl_compiler.h>

#include <vnl/vnl_matrix_fixed.hxx>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_double_3x3.h>
#include <vnl/vnl_double_3.h>
#include <vnl/vnl_double_2x2.h>
#include <vnl/vnl_float_2x2.h>
#include <vnl/vnl_int_2x2.h>

#include <testlib/testlib_test.h>

#undef printf // to work around a bug in libintl.h

bool verbose_malloc = false;
int malloc_count = 0;

// FIXME: Win32 will have different operator new in vnl dll from
// the one generated here, so this test fails - RWMC.
# define reset_count malloc_count = 0
#if defined(VCL_WIN32)
# define check_count TEST("mallocs (no test)",true,true)
#else
# define check_count TEST("mallocs",malloc_count<=1,true)
#endif

// This function is used in testing later.
template< typename T, unsigned int n >
T sum_vector(const vnl_vector_fixed<T,n> &v) { return v.sum(); }

static
void
test_size()
{
  vnl_matrix_fixed<double,3,4> m;
  TEST( "memory footprint", sizeof(m), sizeof(double[12]) );
}

static
void
test_multiply()
{
  double data_m1[6] = {
    1, 2,
    3, 4,
    5, 6
  };
  double data_m2[8] = {
    2, 3, 4, 5,
    6, 7, 8, 9
  };
  double data_v1[2] = {
    7,
    8
  };

  vnl_matrix_fixed<double,3,2> m1( data_m1 );
  vnl_matrix_fixed<double,2,4> m2( data_m2 );
  vnl_vector_fixed<double,2> v1( data_v1 );

  vnl_matrix_fixed<double,3,4> mr = m1*m2;
  TEST("Matrix-matrix multiply",
       mr(0,0) == 14 && mr(0,1) == 17 && mr(0,2) == 20 && mr(0,3) == 23 &&
       mr(1,0) == 30 && mr(1,1) == 37 && mr(1,2) == 44 && mr(1,3) == 51 &&
       mr(2,0) == 46 && mr(2,1) == 57 && mr(2,2) == 68 && mr(2,3) == 79, true);

  vnl_vector_fixed<double,3> vr = m1*v1;
  TEST("Matrix-vector multiply", vr(0) == 23 && vr(1) == 53 && vr(2) == 83, true);
}

static
void test_int()
{
  std::cout << "*********************************\n"
           << "Testing vnl_matrix_fixed<int,x,x>\n"
           << "*********************************" << std::endl;

  //////////////////
  // CONSTRUCTORS //
  //////////////////

  vnl_matrix_fixed<int,2,2> m0;
  TEST("vnl_matrix_fixed<int,2,2> m0", (m0.rows()==2 && m0.columns()==2), true);
  vnl_matrix_fixed<int,3,4> m1;
  TEST("vnl_matrix_fixed<int,3,4> m1", (m1.rows()==3 && m1.columns()==4), true);
  vnl_int_2x2 m2(2);
  TEST("vnl_int_2x2 m2(2)",
       (m2.get(0,0)==2 && m2.get(0,1)==2 && m2.get(1,0)==2 && m2.get(1,1)==2), true);
  TEST("m2 = vnl_int_2x2(2)",
       (m2 = vnl_int_2x2(2),
        (m2.get(0,0)==2 && m2.get(0,1)==2 && m2.get(1,0)==2 && m2.get(1,1)==2)), true);
  const vnl_int_2x2 ma = m2;
  TEST("(const vnl_matrix_fixed)(i,j)",
       (ma(0,0)==2 && ma(0,1)==2 && ma(1,0)==2 && ma(1,1)==2), true);
  vnl_int_2x2 mb = m2;
  TEST("(vnl_matrix_fixed)(i,j)",
       (mb(0,0) = 0,
        mb(0,0)==0 && mb(0,1)==2 && mb(1,0)==2 && mb(1,1)==2), true);
  int mcvalues[4] = {1, 2, 3};
  vnl_int_2x2 mc(mcvalues);
  TEST("vnl_int_2x2 mc(int[])",
       (mc(0,0)==1 && mc(0,1)==2 && mc(1,0)==3 && mc(1,1)==0), true);
  TEST("m0=2",
       (m0=2,
        (m0.get(0,0)==2 && m0.get(0,1)==2 && m0.get(1,0)==2 && m0.get(1,1)==2)), true);
  TEST("m0 == m2", (m0 == m2), true);
  TEST("(m0 == m2)", (m0 == m2), true);

  ///////////////
  // ACCESSORS //
  ///////////////

#if VNL_CONFIG_CHECK_BOUNDS

  {
  // Get
  bool exceptionThrownAndCaught = false;
  try { m0.get(0,25); }  // Raise out of bounds exception.
  catch(...) { exceptionThrownAndCaught = true; }
  TEST("Out of bounds get(0,25)", exceptionThrownAndCaught, true);

  exceptionThrownAndCaught = false;
  try { m0.get(25,0); }  // Raise out of bounds exception.
  catch(...) { exceptionThrownAndCaught = true; }
  TEST("Out of bounds get(25,0)", exceptionThrownAndCaught, true);

  exceptionThrownAndCaught = false;
  try { m0.get(25,25); }  // Raise out of bounds exception.
  catch(...) { exceptionThrownAndCaught = true; }
  TEST("Out of bounds get(25,25)", exceptionThrownAndCaught, true);

  // Put
  exceptionThrownAndCaught = false;
  try { m0.put(0,25,0); }  // Raise out of bounds exception.
  catch(...) { exceptionThrownAndCaught = true; }
  TEST("Out of bounds put(0,25,0)", exceptionThrownAndCaught, true);

  exceptionThrownAndCaught = false;
  try { m0.put(25,0,0); }  // Raise out of bounds exception.
  catch(...) { exceptionThrownAndCaught = true; }
  TEST("Out of bounds put(25,0,0)", exceptionThrownAndCaught, true);

  exceptionThrownAndCaught = false;
  try { m0.put(25,25,0); }  // Raise out of bounds exception.
  catch(...) { exceptionThrownAndCaught = true; }
  TEST("Out of bounds put(25,25,0)", exceptionThrownAndCaught, true);

  }

#endif

  TEST("m2.put(1,1,3)", (m2.put(1,1,3),m2.get(1,1)), 3);
  TEST("m2.get(1,1)", m2.get(1,1), 3);
  int v2_data[] = {2,3};
  TEST("m2.get_diagonal()", m2.get_diagonal(), vnl_vector<int>(2,2,v2_data));
  TEST("m0 == m2", (m0 == m2), false);
  TEST("m0 != m2", (m0 != m2), true);
  TEST("m1.fill(3)",
       (m1.fill(3),
        (m1.get(0,0)==3 && m1.get(1,1)==3 && m1.get(2,2)==3 && m1.get(2,3)==3)), true);
  TEST("m2.fill(2)",
       (m2.fill(2),
        (m2.get(0,0)==2 && m2.get(0,1)==2 && m2.get(1,0)==2 && m2.get(1,1)==2)), true);
  TEST("m0=m2", (m0=m2, (m0==m2)), true);

  // test additions and subtractions
  TEST("m0=m2+3",
       ((m0=m2+3),
        (m0.get(0,0)==5 && m0.get(0,1)==5 && m0.get(1,0)==5 && m0.get(1,1)==5)), true);
  TEST("m0=3+m2",
       ((m0=3+m2),
        (m0.get(0,0)==5 && m0.get(0,1)==5 && m0.get(1,0)==5 && m0.get(1,1)==5)), true);
  TEST("m0+=(-3)",
       (m0+=(-3),
        (m0.get(0,0)==2 && m0.get(0,1)==2 && m0.get(1,0)==2 && m0.get(1,1)==2)), true);
  TEST("m0-=(-3)",
       (m0-=(-3),
        (m0.get(0,0)==5 && m0.get(0,1)==5 && m0.get(1,0)==5 && m0.get(1,1)==5)), true);
  TEST("m0=m2-3",
       ((m0=m2-3),
        (m0.get(0,0)==-1 && m0.get(0,1)==-1 && m0.get(1,0)==-1 && m0.get(1,1)==-1)), true);
  TEST("m0=3-m2",
       ((m0=3-m2),
        (m0.get(0,0)==1 && m0.get(0,1)==1 && m0.get(1,0)==1 && m0.get(1,1)==1)), true);
  TEST("m0= -m2",
       (m0= -m2,
        (m0.get(0,0)==-2 && m0.get(0,1)==-2 && m0.get(1,0)==-2 && m0.get(1,1)==-2)), true);

  vnl_int_2x2 m5;
  m0 = m2;
  TEST("m5=m0+m2",
       ((m5=m0+m2),
        (m5.get(0,0)==4 && m5.get(0,1)==4 && m5.get(1,0)==4 && m5.get(1,1)==4)), true);
  TEST("m5=m0-m2",
       ((m5=m0-m2),
        (m5.get(0,0)==0 && m5.get(0,1)==0 && m5.get(1,0)==0 && m5.get(1,1)==0)), true);
  TEST("m0+=m2",
       ((m0+=m2),
        (m0.get(0,0)==4 && m0.get(0,1)==4 && m0.get(1,0)==4 && m0.get(1,1)==4)), true);
  TEST("m0-=m2",
       ((m0-=m2),
        (m0.get(0,0)==2 && m0.get(0,1)==2 && m0.get(1,0)==2 && m0.get(1,1)==2)), true);

  // test multiplications and divisions
  m2(0,0) = 1; m2(0,1) = 2; m2(1,0) = 3;
  TEST("m0=m2*5",
       ((m0=m2*5),
        (m0.get(0,0)==5 && m0.get(0,1)==10 && m0.get(1,0)==15)), true);
  TEST("m0=5*m2",
       ((m0=5*m2),
        (m0.get(0,0)==5 && m0.get(0,1)==10 && m0.get(1,0)==15)), true);
  TEST("m2*=5",((m2*=5), (m2== m0)), true);
  TEST("m0=m2/5",
       ((m0=m2/5),
        (m0.get(0,0)==1 && m0.get(0,1)==2 && m0.get(1,0)==3)), true);
  TEST("m2/=5", ((m2/=5), (m2==m0)), true);

  int m6values [] = {1,2,3,4};
  vnl_int_2x2 m6(m6values);
  TEST("vnl_int_2x2 m6({1,2,3,4})", m6.get(1,1), 4);
  int m7values [] = {5,6,7,8};
  vnl_int_2x2 m7(m7values);
  TEST("vnl_int_2x2 m7({5,6,7,8})", m7.get(1,1), 8);
  TEST("m5=m6*m7",
       ((m5=m6*m7),
        (m5.get(0,0)==19 && m5.get(0,1)==22 && m5.get(1,0)==43 && m5.get(1,1)==50)), true);
  TEST("m6*=m7",
       ((m6*=m7),
        (m6.get(0,0)==19 && m6.get(0,1)==22 && m6.get(1,0)==43 && m6.get(1,1)==50)), true);

    /////////////////////////////////////////////////////////////////
    // Test `flatten_row_major` and `flatten_column_major` Methods //
    /////////////////////////////////////////////////////////////////

    {
    int data[16] = { 0,  1,  2,  3,
                     4,  5,  6,  7,
                     8,  9, 10, 11,
                    12, 13, 14, 15};

    vnl_vector<int> flat(data, 16);

    vnl_matrix_fixed<int, 4, 4> sq(data);
    vnl_matrix_fixed<int, 2, 8> lg(data);
    vnl_matrix_fixed<int, 8, 2> wd(data);

    TEST("sq.flatten_row_major", flat.is_equal(sq.flatten_row_major().as_vector(), 10e-6), true);
    TEST("lg.flatten_row_major", flat.is_equal(lg.flatten_row_major().as_vector(), 10e-6), true);
    TEST("wd.flatten_row_major", flat.is_equal(wd.flatten_row_major().as_vector(), 10e-6), true);

    TEST("sq.flatten_column_major", flat.is_equal(sq.transpose().flatten_column_major().as_vector(), 10e-6), true);
    TEST("lg.flatten_column_major", flat.is_equal(lg.transpose().flatten_column_major().as_vector(), 10e-6), true);
    TEST("wd.flatten_column_major", flat.is_equal(wd.transpose().flatten_column_major().as_vector(), 10e-6), true);
    }


  // additional tests
  int mvalues [] = {0,-2,2,0};
  vnl_int_2x2 m(mvalues); m0 = m;
  vnl_matrix<int> m3;
  TEST("m(i,j)",
       (m(0,0)==0 && m(0,1)==-2 && m(1,0)==2 && m(1,1)==0), true);
  TEST("m.max_value()", m.max_value(),  2);
  TEST("m.min_value()", m.min_value(), -2);
  TEST("m.arg_max()",   m.arg_max(),   2);
  TEST("m.arg_min()",   m.arg_min(),   1);
  TEST("m.transpose()",
       ((m0 = m.transpose()),
        (m0(0,0)==0 && m0(0,1)==2 && m0(1,0)==-2 && m0(1,1)==0)), true);
  TEST("element_product(m,m)",
       ((m0 = element_product(m,m)),
        (m0(0,0)==0 && m0(0,1)==4 && m0(1,0)==4 && m0(1,1)==0)), true);
  TEST("element_quotient(m,[2])",
       ((m2 = 2),
        (m0 = element_quotient(m,m2)),
        (m0(0,0)==0 && m0(0,1)==-1 && m0(1,0)==1 && m0(1,1)==0)), true);
  TEST("m.extract(1,1,1,1)",
       ((m3 = m.extract(1,1,1,1)),
        (m3.rows()==1 && m3.columns()==1 && m3(0,0)==m(1,1))), true);
  TEST("m.update([4],1,1)",
       ((m3=4),
        (m.update(m3,1,1)),
        (m(0,0)==0 && m(0,1)==-2 && m(1,0)==2 && m(1,1)==4)), true);
}

static
void test_float()
{
  std::cout << "***********************************\n"
           << "Testing vnl_matrix_fixed<float,x,x>\n"
           << "***********************************" << std::endl;
  vnl_matrix_fixed<float,2,2> d0;
  TEST("vnl_matrix_fixed<float,2,2> d0", (d0.rows()==2 && d0.columns()==2), true);
  vnl_matrix_fixed<float,3,4> d1;
  TEST("vnl_matrix_fixed<float,3,4> d1", (d1.rows()==3 && d1.columns()==4), true);
  vnl_float_2x2 d2(2.0);
  TEST("vnl_float_2x2 d2(2.0)",
       (d2.get(0,0)==2.0 && d2.get(0,1)==2.0 && d2.get(1,0)==2.0 && d2.get(1,1)==2.0), true);
  TEST("d0=2.0", (d0=2.0,
                  (d0.get(0,0)==2.0 && d0.get(0,1)==2.0 && d0.get(1,0)==2.0 && d0.get(1,1)==2.0)), true);
  TEST("d0 == d2", (d0 == d2), true);
  TEST("(d0 == d2)", (d0==d2), true);
  TEST("d2.put(1,1,3.0)", (d2.put(1,1,(float)3.0),d2.get(1,1)), (float)3.0);
  TEST("d2.get(1,1)", d2.get(1,1), (float)3.0);
  float v2_data[] = {2.f,3.f};
  TEST("d2.get_diagonal()", d2.get_diagonal(), vnl_vector<float>(2,2,v2_data));
  TEST("d0 == d2", (d0 == d2), false);
  TEST("d0 != d2", (d0 != d2), true);
  TEST("d1.fill(3.0)",
       (d1.fill(3.0),
        (d1.get(0,0)==3.0 && d1.get(1,1)==3.0 && d1.get(2,2)==3.0 && d1.get(2,3)==3.0)), true);
  TEST("d2.fill(2.0)",
       (d2.fill(2.0),
        (d2.get(0,0)==2.0 && d2.get(0,1)==2.0 && d2.get(1,0)==2.0 && d2.get(1,1)==2.0)), true);
  TEST("d0=d2", (d0=d2,  (d0==d2)), true);

  // test additions and subtractions
  TEST("d0=d2+3.0",
       ((d0=d2+(float)3.0),
        (d0.get(0,0)==5.0 && d0.get(0,1)==5.0 && d0.get(1,0)==5.0 && d0.get(1,1)==5.0)), true);
  TEST("d0+=(-3.0)",
       (d0+=(-3.0),
        (d0.get(0,0)==2.0 && d0.get(0,1)==2.0 && d0.get(1,0)==2.0 && d0.get(1,1)==2.0)), true);
  vnl_float_2x2 d5;
  TEST("d5=d0+d2",
       ((d5=d0+d2),
        (d5.get(0,0)==4.0 && d5.get(0,1)==4.0 && d5.get(1,0)==4.0 && d5.get(1,1)==4.0)), true);
  TEST("d0+=d2",
       ((d0+=d2),
        (d0.get(0,0)==4.0 && d0.get(0,1)==4.0 && d0.get(1,0)==4.0 && d0.get(1,1)==4.0)), true);

  // test multiplications and divisions
  d2(0,0) = 1; d2(0,1) = 2; d2(1,0) = 3;
  TEST("d0=d2*5.0f",
       ((d0=d2*5.0f),
        (d0.get(0,0)==5 && d0.get(0,1)==10 && d0.get(1,0)==15)), true);
  TEST("d0=5.0f*d2",
       ((d0=5.0f*d2),
        (d0.get(0,0)==5 && d0.get(0,1)==10 && d0.get(1,0)==15)), true);
  TEST("d2*=5.0f",((d2*=5.0f), (d2== d0)), true);
  TEST("d0=d2/5.0f",
       ((d0=d2/5.0f),
        (d0.get(0,0)==1 && d0.get(0,1)==2 && d0.get(1,0)==3)), true);
  TEST("d2/=5.0f", ((d2/=5.0f), (d2==d0)), true);
  float d6values [] = {1.0f,2.0f,
                       3.0f,4.0f};
  vnl_float_2x2 d6(d6values);
  TEST("vnl_float_2x2 d6({1.0,2.0,3.0,4.0})", d6.get(1,1), 4.0);
  float d7values [] = {5.0,6.0,
                       7.0,8.0};
  vnl_float_2x2 d7(d7values);
  TEST("vnl_float_2x2 d7({5.0,6.0,7.0,8.0})", d7.get(1,1), 8.0);
  TEST("d5=d6*d7", ((d5=d6*d7),
                    (d5.get(0,0)==19.0 && d5.get(0,1)==22.0 && d5.get(1,0)==43.0 && d5.get(1,1)==50.0)), true);
  TEST("d6*=d7", ((d6*=d7),
                  (d6.get(0,0)==19.0 && d6.get(0,1)==22.0 && d6.get(1,0)==43.0 && d6.get(1,1)==50.0)), true);

  // additional tests
  vnl_float_2x2 m1, m2;
  float mvalues [] = {0,-2,2,0};
  vnl_float_2x2 m(mvalues);
  m1 = m; m2 = m;
  vnl_matrix<float> m3;
  TEST("m(i,j)",
       (m(0,0)==0 && m(0,1)==-2 && m(1,0)==2 && m(1,1)==0), true);
  TEST("m.max_value()", m.max_value(),  2);
  TEST("m.min_value()", m.min_value(), -2);
  TEST("m.arg_max()",   m.arg_max(),   2);
  TEST("m.arg_min()",   m.arg_min(),   1);
  TEST("m.transpose()",
       ((m1 = m.transpose()),
        (m1(0,0)==0 && m1(0,1)==2 && m1(1,0)==-2 && m1(1,1)==0)), true);
  TEST("element_product(m,m)",
       ((m1 = element_product(m,m)),
        (m1(0,0)==0 && m1(0,1)==4 && m1(1,0)==4 && m1(1,1)==0)), true);
  TEST("element_quotient(m,[2])",
       ((m2 = 2),
        (m1 = element_quotient(m,m2)),
        (m1(0,0)==0 && m1(0,1)==-1 && m1(1,0)==1 && m1(1,1)==0)), true);
  TEST("m.extract(1,1,1,1)",
       ((m3 = m.extract(1,1,1,1)),
        (m3.rows()==1 && m3.columns()==1 && m3(0,0)==m(1,1))), true);
  TEST("m.update([4],1,1)",
       ((m3=4),
        (m.update(m3,1,1)),
        (m(0,0)==0 && m(0,1)==-2 && m(1,0)==2 && m(1,1)==4)), true);
}

static
void test_double()
{
  std::cout << "************************************\n"
           << "Testing vnl_matrix_fixed<double,x,x>\n"
           << "************************************" << std::endl;
  vnl_matrix_fixed<double,2,2> d0;
  TEST("vnl_matrix_fixed<double,2,2> d0", (d0.rows()==2 && d0.columns()==2), true);
  vnl_matrix_fixed<double,3,4> d1;
  TEST("vnl_matrix_fixed<double,3,4> d1", (d1.rows()==3 && d1.columns()==4), true);
  vnl_double_2x2 d2(2.0);
  TEST("vnl_double_2x2 d2(2.0)",
       (d2.get(0,0)==2.0 && d2.get(0,1)==2.0 && d2.get(1,0)==2.0 && d2.get(1,1)==2.0), true);
  TEST("d0=2.0", (d0=2.0,
                  (d0.get(0,0)==2.0 && d0.get(0,1)==2.0 && d0.get(1,0)==2.0 && d0.get(1,1)==2.0)), true);
  TEST("d0 == d2", (d0 == d2), true);
  TEST("(d0 == d2)", (d0==d2), true);
  TEST("d2.put(1,1,3.0)", (d2.put(1,1,3.0),d2.get(1,1)), 3.0);
  TEST("d2.get(1,1)", d2.get(1,1), 3.0);
  double v2_data[] = {2.0,3.0};
  TEST("d2.get_diagonal()", d2.get_diagonal(), vnl_vector<double>(2,2,v2_data));
  TEST("d0 == d2", (d0 == d2), false);
  TEST("d0 != d2", (d0 != d2), true);
  TEST("d1.fill(3.0)",
       (d1.fill(3.0),
        (d1.get(0,0)==3.0 && d1.get(1,1)==3.0 && d1.get(2,2)==3.0 && d1.get(2,3)==3.0)), true);
  TEST("d2.fill(3.0)",
       (d2.fill(2.0),
        (d2.get(0,0)==2.0 && d2.get(0,1)==2.0 && d2.get(1,0)==2.0 && d2.get(1,1)==2.0)), true);
  TEST("d0=d2", (d0=d2,  (d0==d2)), true);

  // test additions and subtractions
  TEST("d0=d2+3.0",
       ((d0=d2+3.0),
        (d0.get(0,0)==5.0 && d0.get(0,1)==5.0 && d0.get(1,0)==5.0 && d0.get(1,1)==5.0)), true);
  TEST("d0+=(-3.0)",
       (d0+=(-3.0),
        (d0.get(0,0)==2.0 && d0.get(0,1)==2.0 && d0.get(1,0)==2.0 && d0.get(1,1)==2.0)), true);
  vnl_double_2x2 d5;
  TEST("d5=d0+d2",
       ((d5=d0+d2),
        (d5.get(0,0)==4.0 && d5.get(0,1)==4.0 && d5.get(1,0)==4.0 && d5.get(1,1)==4.0)), true);
  TEST("d0+=d2",
       ((d0+=d2),
        (d0.get(0,0)==4.0 && d0.get(0,1)==4.0 && d0.get(1,0)==4.0 && d0.get(1,1)==4.0)), true);

  // test multiplications and divisions
  d2(0,0) = 1; d2(0,1) = 2; d2(1,0) = 3;
  TEST("d0=d2*5.0",
       ((d0=d2*5.0),
        (d0.get(0,0)==5 && d0.get(0,1)==10 && d0.get(1,0)==15)), true);
  TEST("d0=5.0*d2",
       ((d0=5.0*d2),
        (d0.get(0,0)==5 && d0.get(0,1)==10 && d0.get(1,0)==15)), true);
  TEST("d2*=5.0",((d2*=5.0), (d2== d0)), true);
  TEST("d0=d2/5.0",
       ((d0=d2/5.0),
        (d0.get(0,0)==1 && d0.get(0,1)==2 && d0.get(1,0)==3)), true);
  TEST("d2/=5.0", ((d2/=5.0), (d2==d0)), true);
  double d6values [] = {1.0,2.0,
                        3.0,4.0};
  vnl_double_2x2 d6(d6values);
  TEST("vnl_double_2x2 d6({1.0,2.0,3.0,4.0})", d6.get(1,1), 4.0);
  double d7values [] = {5.0,6.0,
                        7.0,8.0};
  vnl_double_2x2 d7(d7values);
  TEST("vnl_double_2x2 d7({5.0,6.0,7.0,8.0})", d7.get(1,1), 8.0);
  TEST("d5=d6*d7", ((d5=d6*d7),
                    (d5.get(0,0)==19.0 && d5.get(0,1)==22.0 && d5.get(1,0)==43.0 && d5.get(1,1)==50.0)), true);
  TEST("d6*=d7", ((d6*=d7),
                  (d6.get(0,0)==19.0 && d6.get(0,1)==22.0 && d6.get(1,0)==43.0 && d6.get(1,1)==50.0)), true);

  // apply sqrt to every element
  double d8values [] = {0.0, 1.0, 9.0, 16.0};
  vnl_double_2x2 d8(d8values);
  d8 = d8.apply(std::sqrt);
  TEST("apply(sqrt)", d8[0][0]==0 && d8[0][1]==1 && d8[1][0]==3 && d8[1][1]==4, true);

  {
  vnl_matrix_fixed<double,4,20> m(1.);
  vnl_vector_fixed<double,4> vr = m.apply_rowwise(sum_vector);
  for (unsigned int i = 0; i < vr.size(); ++i)
    TEST("vr.apply_rowwise(sum_vector)", vr.get(i), 20.);
  vnl_vector_fixed<double,20> vc = m.apply_columnwise(sum_vector);
  for (unsigned int i = 0; i < vc.size(); ++i)
    TEST("vc.apply_columnwise(sum_vector)", vc.get(i), 4.);
  }

  // normalizations
  d8.normalize_rows();
  TEST("normalize_rows()", d8[0][0]==0 && d8[0][1]==1, true);
  TEST_NEAR("normalize_rows()", d8[1][0], 0.6, 1e-12);
  TEST_NEAR("normalize_rows()", d8[1][1], 0.8, 1e-12);
  d8.normalize_columns();
  TEST("normalize_columns()", d8[0][0]==0 && d8[1][0]==1, true);
}

namespace {

template<class T>
void
test_extract( T* )
{
  vnl_matrix_fixed<T,2,6> m;
  m(0,0)=1; m(0,1)=2; m(0,2)=3; m(0,3)=4; m(0,4)=5; m(0,5) = 11;
  m(1,0)=6; m(1,1)=7; m(1,2)=8; m(1,3)=9; m(1,4)=0; m(1,5) = 12;
  std::cout << "m=\n" << m.as_ref() << '\n';

  vnl_matrix_fixed<T,1,3> r;
  m.extract( r.as_ref().non_const(), 1, 2 );
  std::cout << "r=\n" << r.as_ref() << '\n';
  TEST( "extract into existing matrix", r(0,0)==8 && r(0,1)==9 && r(0,2)==0, true );
}

} // end anonymous namespace

void test_matrix_fixed()
{
  verbose_malloc = true;
  double datablock[9] = {
    11, 12, 13,
    21, 22, 23,
    31, 32, 33,
  };

  std::printf("Calling ctor -- should be no mallocs\n");
  reset_count;
  vnl_double_3x3 X(datablock);
  check_count;
  std::printf("X = [ %g %g %g\n      %g %g %g\n      %g %g %g ]\n",
             X(0,0),X(0,1),X(0,2),X(1,0),X(1,1),X(1,2),X(2,0),X(2,1),X(2,2));

  reset_count;
  vnl_double_3 v(10,11,12);
  check_count;
  std::printf("v = [ %g %g %g ]\n", v(0), v(1), v(2));

  reset_count;
  vnl_double_3 splork = X * (v + v);
  check_count;
  std::printf("splork = [ %g %g %g ]\n", splork(0), splork(1), splork(2));

  std::printf("Now watch the mallocs\n");
  vnl_matrix_ref<double> CX = X.as_ref();
  vnl_vector_ref<double> cv = v.as_ref();
  vnl_vector<double> Xv = CX * (cv + cv);
  std::printf("X v = [ %g %g %g ]\n", Xv[0], Xv[1], Xv[2]);

  verbose_malloc = false;

  // test that vnl_double_3x3's can be multiplied
  vnl_double_3x3 A(datablock);
  vnl_double_3x3 B = A * A;

  // test that vnl_double_3x3's can be added and subtracted
  B = A + B;
  B -= A;

  B.fill(1.0);
  TEST("fill(1)", B(0,0)==1 && B(0,1)==1 && B(1,2)==1 && B(2,2)==1, true);
  B.fill_diagonal(4.0);
  TEST("fill_diagonal(4)", B(0,0)==4 && B(0,1)==1 && B(1,2)==1 && B(2,2)==4, true);
  B.set_diagonal(vnl_double_3(7,9,16));
  TEST("set_diagonal(7,9,16))",B(0,0)==7 && B(1,1)==9 && B(2,2)==16 && B(1,2)==1, true);

  // apply sqrt to every element
  B = B.apply(std::sqrt);
  TEST("apply(sqrt)", B(1,1)==3 && B(0,2)==1 && B(2,1)==1 && B(2,2)==4, true);

  test_multiply();
  test_size();

  test_int();
  test_float();
  test_double();

  test_extract( (double*)VXL_NULLPTR );
}

#ifdef TEST_MALLOC
      // BAD-BAD-BAD these operators new/delete are picked up by *all* tests!!!
      //  The problem is that they don't provide new[] and delete[].  - PVr

// with gcc 3.0, formatted stream output uses operator
// new so printing to cout here causes stack overflow.

void* operator new(std::size_t s)
  // [18.4.1] lib.new.delete
  throw(std::bad_alloc)
{
  void *r = std::malloc(s);

  ++malloc_count;

  if (verbose_malloc)
    std::printf("malloc: %08lX for %d\n", (unsigned long)r, int(s));

  return r;
}

void operator delete(void* s)
  throw()
{
  if (verbose_malloc)
    std::printf("delete: %08lX\n", (unsigned long)s);

  std::free(s);
}

#endif // TEST_MALLOC

TESTMAIN(test_matrix_fixed);
