// This is core/vnl/tests/test_matrix_fixed.cxx
#ifdef TEST_MALLOC // see note below, at the other #ifdef TEST_MALLOC
# include <vcl_new.h>
#endif
#include <vcl_cstdlib.h>
#include <vcl_cstddef.h> // for vcl_size_t
#include <vcl_cmath.h> // for sqrt

#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_double_3x3.h>
#include <vnl/vnl_double_3.h>
#include <vnl/vnl_double_2x2.h>
#include <vnl/vnl_float_2x2.h>
#include <vnl/vnl_int_2x2.h>

#include <testlib/testlib_test.h>

#undef printf // to work around a bug in libintl.h
#include <vcl_cstdio.h> // do not use iostream within operator new - it causes infinite recursion

bool verbose_malloc = false;
int malloc_count = 0;

// FIXME: Win32 will have different operator new in vnl dll from
// the one generated here, so this test fails - RWMC.
// The test also fails for gcc 3.0 - PVr
# define reset_count malloc_count = 0
#if !defined(VCL_WIN32) && !defined(GNU_LIBSTDCXX_V3)
# define check_count TEST("mallocs",malloc_count<=1,true)
#else
# define check_count TEST("mallocs (no test)",true,true)
#endif

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

  testlib_test_begin( "Matrix-matrix multiply" );
  vnl_matrix_fixed<double,3,4> mr = m1*m2;
  testlib_test_perform( mr(0,0) == 14 && mr(0,1) == 17 && mr(0,2) == 20 && mr(0,3) == 23 &&
                        mr(1,0) == 30 && mr(1,1) == 37 && mr(1,2) == 44 && mr(1,3) == 51 &&
                        mr(2,0) == 46 && mr(2,1) == 57 && mr(2,2) == 68 && mr(2,3) == 79 );

  testlib_test_begin( "Matrix-vector multiply" );
  vnl_vector_fixed<double,3> vr = m1*v1;
  testlib_test_perform( vr(0) == 23 && vr(1) == 53 && vr(2) == 83 );
}

static
void test_int()
{
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
  TEST("m2.put(1,1,3)", (m2.put(1,1,3),m2.get(1,1)), 3);
  TEST("m2.get(1,1)", m2.get(1,1), 3);
  TEST("m0 == m2", (m0 == m2), false);
  TEST("m0 != m2", (m0 != m2), true);
  TEST("(m0 == m2)", (m0 == m2), false);
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

  // additional tests
  int mvalues [] = {0,-2,2,0};
  vnl_int_2x2 m(mvalues); m0 = m;
  vnl_matrix<int> m3;
  TEST("m(i,j)",
       (m(0,0)==0 && m(0,1)==-2 && m(1,0)==2 && m(1,1)==0), true);
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
  TEST("d0 == d2", (d0 == d2), false);
  TEST("d0 != d2", (d0 != d2), true);
  TEST("(d0 == d2)", (d0==d2), false);
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
  TEST("d0 == d2", (d0 == d2), false);
  TEST("d0 != d2", (d0 != d2), true);
  TEST("(d0 == d2)", (d0==d2), false);
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
  d8 = d8.apply(vcl_sqrt);
  TEST("apply(sqrt)", d8[0][0]==0 && d8[0][1]==1 && d8[1][0]==3 && d8[1][1]==4, true);

  // normalizations
  d8.normalize_rows();
  TEST("normalize_rows()", d8[0][0]==0 && d8[0][1]==1, true);
  TEST_NEAR("normalize_rows()", d8[1][0], 0.6, 1e-12);
  TEST_NEAR("normalize_rows()", d8[1][1], 0.8, 1e-12);
  d8.normalize_columns();
  TEST("normalize_columns()", d8[0][0]==0 && d8[1][0]==1, true);
}

void test_matrix_fixed()
{
  verbose_malloc = true;
  double datablock[9] = {
    11, 12, 13,
    21, 22, 23,
    31, 32, 33,
  };

  vcl_printf("Calling ctor -- should be no mallocs\n");
  reset_count;
  vnl_double_3x3 X(datablock);
  check_count;
  vcl_printf("X = [ %g %g %g\n      %g %g %g\n      %g %g %g ]\n",
             X(0,0),X(0,1),X(0,2),X(1,0),X(1,1),X(1,2),X(2,0),X(2,1),X(2,2));

  reset_count;
  vnl_double_3 v(10,11,12);
  check_count;
  vcl_printf("v = [ %g %g %g ]\n", v(0), v(1), v(2));

  reset_count;
  vnl_double_3 splork = X * (v + v);
  check_count;
  vcl_printf("splork = [ %g %g %g ]\n", splork(0), splork(1), splork(2));

  // This shouldn't compile...
#if 0
  vnl_matrix<double>* base = new vnl_double_3x3(datablock);
#endif

  vcl_printf("Now watch the mallocs\n");
  vnl_matrix_ref<double> CX = X;
  vnl_vector_ref<double> cv = v;
  vnl_vector<double> Xv = CX * (cv + cv);
  vcl_printf("X v = [ %g %g %g ]\n", Xv[0], Xv[1], Xv[2]);

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

  // apply sqrt to every element
  B = B.apply(vcl_sqrt);
  TEST("apply(sqrt)", B(1,1)==2 && B(0,2)==1 && B(2,1)==1 && B(2,2)==2, true);

  test_multiply();
  test_size();

  test_int();
  test_float();
  test_double();
}

#ifdef TEST_MALLOC
      // BAD-BAD-BAD these operators new/delete are picked up by *all* tests!!!
      //  The problem is that they don't provide new[] and delete[].  - PVr

// with gcc 3.0, formatted stream output uses operator
// new so printing to cout here causes stack overflow.

void* operator new(vcl_size_t s)
  // [18.4.1] lib.new.delete
#if defined(VCL_SUNPRO_CC_5) || defined(GNU_LIBSTDCXX_V3) || defined(VCL_KAI)
  throw(std::bad_alloc)
#endif
{
  void *r = vcl_malloc(s);

  ++malloc_count;

  if (verbose_malloc)
    vcl_printf("malloc: %08lX for %d\n", (unsigned long)r, int(s));

  return r;
}

void operator delete(void* s)
#if defined(GNU_LIBSTDCXX_V3) || defined(VCL_SUNPRO_CC_5)
  throw()
#endif
{
  if (verbose_malloc)
    vcl_printf("delete: %08lX\n", (unsigned long)s);

  vcl_free(s);
}

#endif // TEST_MALLOC

TESTMAIN(test_matrix_fixed);
