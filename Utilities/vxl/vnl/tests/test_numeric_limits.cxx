#include <vcl_iostream.h>
#include <vnl/vnl_numeric_limits.h>
#include <testlib/testlib_test.h>

static
void test_if_bool_defined( const bool* )
{
}

static
void test_if_int_defined( const int* )
{
}

static
void test_if_vnl_float_round_style_defined( const vnl_float_round_style* )
{
}

// if this function compiles and links, then all the constants have
// definitions as they should.
static
void test_static_const_definition()
{
#define TEST_TYPE( Type ) \
  test_if_bool_defined( &vnl_numeric_limits< Type >::is_specialized );\
  test_if_int_defined( &vnl_numeric_limits< Type >::digits );\
  test_if_int_defined( &vnl_numeric_limits< Type >::digits10 );\
  test_if_bool_defined( &vnl_numeric_limits< Type >::is_signed );\
  test_if_bool_defined( &vnl_numeric_limits< Type >::is_integer );\
  test_if_bool_defined( &vnl_numeric_limits< Type >::is_exact );\
  test_if_int_defined( &vnl_numeric_limits< Type >::radix );\
  test_if_int_defined( &vnl_numeric_limits< Type >::min_exponent );\
  test_if_int_defined( &vnl_numeric_limits< Type >::min_exponent10 );\
  test_if_int_defined( &vnl_numeric_limits< Type >::max_exponent );\
  test_if_int_defined( &vnl_numeric_limits< Type >::max_exponent10 );\
  test_if_bool_defined( &vnl_numeric_limits< Type >::has_infinity );\
  test_if_bool_defined( &vnl_numeric_limits< Type >::has_quiet_NaN );\
  test_if_bool_defined( &vnl_numeric_limits< Type >::has_signaling_NaN );\
  test_if_bool_defined( &vnl_numeric_limits< Type >::has_denorm );\
  test_if_bool_defined( &vnl_numeric_limits< Type >::is_iec559 );\
  test_if_bool_defined( &vnl_numeric_limits< Type >::is_bounded );\
  test_if_bool_defined( &vnl_numeric_limits< Type >::is_modulo );\
  test_if_bool_defined( &vnl_numeric_limits< Type >::traps );\
  test_if_bool_defined( &vnl_numeric_limits< Type >::tinyness_before );\
  test_if_vnl_float_round_style_defined( &vnl_numeric_limits< Type >::round_style )

  TEST_TYPE(int);
  TEST_TYPE(long);
  TEST_TYPE(unsigned long);
  TEST_TYPE(short);
  TEST_TYPE(unsigned short);
  TEST_TYPE(float);
  TEST_TYPE(double);
#undef TEST_TYPE
}


void test_numeric_limits()
{
  // call it to avoid "unused function" compiler warnings,
  // and to force compilation with "very clever" compilers:
  test_static_const_definition();

  vcl_cout << "dmax  = " << vnl_numeric_limits<double>::max() << vcl_endl
           << "dmin  = " << vnl_numeric_limits<double>::min() << vcl_endl
           << "deps  = " << vnl_numeric_limits<double>::epsilon() << vcl_endl
           << "dnmin = " << vnl_numeric_limits<double>::denorm_min() << vcl_endl
           << "dnan  = " << vnl_numeric_limits<double>::quiet_NaN() << vcl_endl
           << "dsnan = " << vnl_numeric_limits<double>::signaling_NaN() << vcl_endl
           << "dinf  = " << vnl_numeric_limits<double>::infinity() << vcl_endl
           << "-dinf = " <<-vnl_numeric_limits<double>::infinity() << vcl_endl
           << "rnder = " << vnl_numeric_limits<double>::round_error() << vcl_endl

           << "fmax  = " << vnl_numeric_limits<float>::max() << vcl_endl
           << "fmin  = " << vnl_numeric_limits<float>::min() << vcl_endl
           << "feps  = " << vnl_numeric_limits<float>::epsilon() << vcl_endl
           << "fnmin = " << vnl_numeric_limits<float>::denorm_min() << vcl_endl
           << "fnan  = " << vnl_numeric_limits<float>::quiet_NaN() << vcl_endl
           << "fsnan = " << vnl_numeric_limits<float>::signaling_NaN() << vcl_endl
           << "finf  = " << vnl_numeric_limits<float>::infinity() << vcl_endl
           << "-finf = " <<-vnl_numeric_limits<float>::infinity() << vcl_endl
           << "rnder = " << vnl_numeric_limits<float>::round_error() << vcl_endl;

  TEST("dmax", vnl_numeric_limits<double>::max() > 1e308, true);
  TEST("dinf", vnl_numeric_limits<double>::infinity() >
               vnl_numeric_limits<double>::max(), true);
  TEST("dmin", vnl_numeric_limits<double>::min() < 1e-307 &&
               vnl_numeric_limits<double>::min() > 0, true);
  TEST("deps", vnl_numeric_limits<double>::epsilon() < 1e-12 &&
               vnl_numeric_limits<double>::epsilon() > 0, true);
  TEST("rnder",vnl_numeric_limits<double>::round_error(), 0.5);
  TEST("fmax", vnl_numeric_limits<float>::max() > 1e38f, true);
  TEST("finf", vnl_numeric_limits<float>::infinity() >
               vnl_numeric_limits<float>::max(), true);
  TEST("fmin", vnl_numeric_limits<float>::min() < 1e-37f &&
               vnl_numeric_limits<float>::min() > 0, true);
  TEST("feps", vnl_numeric_limits<float>::epsilon() < 1e-6f &&
               vnl_numeric_limits<float>::epsilon() > 0, true);
  TEST("rnder",vnl_numeric_limits<float>::round_error(), 0.5);
}

TESTMAIN(test_numeric_limits);
