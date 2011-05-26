#include <vcl_iostream.h>
#include <vcl_limits.h>

static
void test_if_bool_defined( bool )
{
}

static
void test_if_int_defined( int )
{
}


// if this function compiles and links, then all the constants have
// definitions as they should.
static
void test_static_const_definition()
{
#define TEST_TYPE( Type ) \
  test_if_bool_defined( vcl_numeric_limits< Type >::is_specialized ); \
  test_if_int_defined( vcl_numeric_limits< Type >::digits ); \
  test_if_int_defined( vcl_numeric_limits< Type >::digits10 ); \
  test_if_bool_defined( vcl_numeric_limits< Type >::is_signed ); \
  test_if_bool_defined( vcl_numeric_limits< Type >::is_integer ); \
  test_if_bool_defined( vcl_numeric_limits< Type >::is_exact ); \
  test_if_int_defined( vcl_numeric_limits< Type >::radix ); \
  test_if_int_defined( vcl_numeric_limits< Type >::min_exponent ); \
  test_if_int_defined( vcl_numeric_limits< Type >::min_exponent10 ); \
  test_if_int_defined( vcl_numeric_limits< Type >::max_exponent ); \
  test_if_int_defined( vcl_numeric_limits< Type >::max_exponent10 ); \
  test_if_bool_defined( vcl_numeric_limits< Type >::has_infinity ); \
  test_if_bool_defined( vcl_numeric_limits< Type >::has_quiet_NaN ); \
  test_if_bool_defined( vcl_numeric_limits< Type >::has_signaling_NaN ); \
  test_if_bool_defined( vcl_numeric_limits< Type >::has_denorm ); \
  test_if_bool_defined( vcl_numeric_limits< Type >::is_iec559 ); \
  test_if_bool_defined( vcl_numeric_limits< Type >::is_bounded ); \
  test_if_bool_defined( vcl_numeric_limits< Type >::is_modulo ); \
  test_if_bool_defined( vcl_numeric_limits< Type >::traps ); \
  test_if_bool_defined( vcl_numeric_limits< Type >::tinyness_before );

  TEST_TYPE(int);
  TEST_TYPE(long);
  TEST_TYPE(unsigned long);
  TEST_TYPE(short);
  TEST_TYPE(unsigned short);
  TEST_TYPE(float);
  TEST_TYPE(double);
#undef TEST_TYPE
}

#define TEST(m,x,y)    if ((x)!=(y)) { vcl_cout<< "FAIL: " << m << '\n'; fail=true; } \
                       else { vcl_cout<< "PASS: " << m << '\n'; }

int test_limits_main(int /*argc*/, char* /*argv*/[])
{
  // call it to avoid "unused function" compiler warnings,
  // and to force compilation with "very clever" compilers:
  test_static_const_definition();

  bool fail=false;
  vcl_cout << "dmax  = " << vcl_numeric_limits<double>::max() << vcl_endl
           << "dmin  = " << vcl_numeric_limits<double>::min() << vcl_endl
           << "deps  = " << vcl_numeric_limits<double>::epsilon() << vcl_endl
           << "dnmin = " << vcl_numeric_limits<double>::denorm_min() << vcl_endl
           << "dnan  = " << vcl_numeric_limits<double>::quiet_NaN() << vcl_endl
           << "dsnan = " << vcl_numeric_limits<double>::signaling_NaN() << vcl_endl
           << "dinf  = " << vcl_numeric_limits<double>::infinity() << vcl_endl
           << "-dinf = " <<-vcl_numeric_limits<double>::infinity() << vcl_endl
           << "rnder = " << vcl_numeric_limits<double>::round_error() << vcl_endl

           << "fmax  = " << vcl_numeric_limits<float>::max() << vcl_endl
           << "fmin  = " << vcl_numeric_limits<float>::min() << vcl_endl
           << "feps  = " << vcl_numeric_limits<float>::epsilon() << vcl_endl
           << "fnmin = " << vcl_numeric_limits<float>::denorm_min() << vcl_endl
           << "fnan  = " << vcl_numeric_limits<float>::quiet_NaN() << vcl_endl
           << "fsnan = " << vcl_numeric_limits<float>::signaling_NaN() << vcl_endl
           << "finf  = " << vcl_numeric_limits<float>::infinity() << vcl_endl
           << "-finf = " <<-vcl_numeric_limits<float>::infinity() << vcl_endl
           << "rnder = " << vcl_numeric_limits<float>::round_error() << vcl_endl

           << "s8max  = " << int(vcl_numeric_limits<signed char>::max()) << vcl_endl
           << "s8min  = " << int(vcl_numeric_limits<signed char>::min()) << vcl_endl

           << "u8max  = " << int(vcl_numeric_limits<unsigned char>::max()) << vcl_endl
           << "u8min  = " << int(vcl_numeric_limits<unsigned char>::min()) << vcl_endl

           << "s16max  = " << vcl_numeric_limits<signed short>::max() << vcl_endl
           << "s16min  = " << vcl_numeric_limits<signed short>::min() << vcl_endl

           << "u16max  = " << vcl_numeric_limits<unsigned short>::max() << vcl_endl
           << "u16min  = " << vcl_numeric_limits<unsigned short>::min() << vcl_endl

           << "s32max  = " << vcl_numeric_limits<signed int>::max() << vcl_endl
           << "s32min  = " << vcl_numeric_limits<signed int>::min() << vcl_endl

           << "u32max  = " << vcl_numeric_limits<unsigned int>::max() << vcl_endl
           << "u32min  = " << vcl_numeric_limits<unsigned int>::min() << vcl_endl;

  TEST("dmax", vcl_numeric_limits<double>::max() > 1e308, true);
  if (vcl_numeric_limits<double>::has_infinity) {
    TEST("dinf", vcl_numeric_limits<double>::infinity() >
                 vcl_numeric_limits<double>::max(), true);
  }
  TEST("dmin", vcl_numeric_limits<double>::min() < 1e-307 &&
               vcl_numeric_limits<double>::min() > 0, true);
  TEST("deps", vcl_numeric_limits<double>::epsilon() < 1e-12 &&
               vcl_numeric_limits<double>::epsilon() > 0, true);
  TEST("rnder",vcl_numeric_limits<double>::round_error() <= 1.0, true);
  TEST("fmax", vcl_numeric_limits<float>::max() > 1e38f, true);
  if (vcl_numeric_limits<float>::has_infinity) {
    TEST("finf", vcl_numeric_limits<float>::infinity() >
                 vcl_numeric_limits<float>::max(), true);
  }
  TEST("fmin", vcl_numeric_limits<float>::min() < 1e-37f &&
               vcl_numeric_limits<float>::min() > 0, true);
  TEST("feps", vcl_numeric_limits<float>::epsilon() < 1e-6f &&
               vcl_numeric_limits<float>::epsilon() > 0, true);
  TEST("rnder",vcl_numeric_limits<float>::round_error() <= 1.0, true);
  return fail?1:0;
}

