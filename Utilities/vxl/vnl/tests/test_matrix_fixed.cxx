// This is core/vnl/tests/test_matrix_fixed.cxx
#if TEST_MALLOC // see note below, at the other #if TEST_MALLOC
#include <vcl_new.h>
#endif
#include <vcl_cstdio.h> // do not use iostream within operator new - it causes infinite recursion
#include <vcl_cstdlib.h>
#include <vcl_cstddef.h> // for vcl_size_t

#include <vnl/vnl_double_3x3.h>
#include <vnl/vnl_double_3.h>

#include <testlib/testlib_test.h>

bool verbose_malloc = false;
int malloc_count = 0;

// FIXME: Win32 will have different operator new in vnl dll from
// the one generated here, so this test fails - RWMC.
// The test also fails for gcc 3.0 - PVr
# define reset_count malloc_count = 0
#if !defined(VCL_WIN32) && !defined(GNU_LIBSTDCXX_V3)
# define check_count TEST("mallocs",malloc_count<=1,true)
#else
# define check_count /* */
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

void test_matrix_fixed()
{
  verbose_malloc = true;
  double datablock[9] = {
    11, 12, 13,
    21, 22, 23,
    31, 32, 33,
  };

  vcl_printf("Calling ctor -- should be no mallocs\n");
  //Refvnl_double_3x3 X(datablock);
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

  test_multiply();
  test_size();
}

#if TEST_MALLOC
      // BAD-BAD-BAD these operators new/delete are picked up by *all* tests!!!
      //  The problem is that they don't provide new[] and delete[].  - PVr

// with gcc 3.0, formatted stream output uses operator
// new so printing to cout here causes stack overflow.

void* operator new(vcl_size_t s)
  // [18.4.1] lib.new.delete
#if defined(VCL_SUNPRO_CC_50) || defined(GNU_LIBSTDCXX_V3) || defined(VCL_KAI)
  throw (std::bad_alloc)
#endif
{
  void *r = vcl_malloc(s);

  ++malloc_count;

  if (verbose_malloc)
    vcl_printf("malloc: %08lX for %d\n", (unsigned long)r, int(s));

  return r;
}

void operator delete(void* s)
#if defined(GNU_LIBSTDCXX_V3) || defined(VCL_SUNPRO_CC_50)
  throw ()
#endif
{
  if (verbose_malloc)
    vcl_printf("delete: %08lX\n", (unsigned long)s);

  vcl_free(s);
}

#endif // 0

TESTMAIN(test_matrix_fixed);
