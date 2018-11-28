#include <iostream>
#include <limits>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

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
  test_if_bool_defined( std::numeric_limits< Type >::is_specialized ); \
  test_if_int_defined( std::numeric_limits< Type >::digits ); \
  test_if_int_defined( std::numeric_limits< Type >::digits10 ); \
  test_if_bool_defined( std::numeric_limits< Type >::is_signed ); \
  test_if_bool_defined( std::numeric_limits< Type >::is_integer ); \
  test_if_bool_defined( std::numeric_limits< Type >::is_exact ); \
  test_if_int_defined( std::numeric_limits< Type >::radix ); \
  test_if_int_defined( std::numeric_limits< Type >::min_exponent ); \
  test_if_int_defined( std::numeric_limits< Type >::min_exponent10 ); \
  test_if_int_defined( std::numeric_limits< Type >::max_exponent ); \
  test_if_int_defined( std::numeric_limits< Type >::max_exponent10 ); \
  test_if_bool_defined( std::numeric_limits< Type >::has_infinity ); \
  test_if_bool_defined( std::numeric_limits< Type >::has_quiet_NaN ); \
  test_if_bool_defined( std::numeric_limits< Type >::has_signaling_NaN ); \
  test_if_bool_defined( std::numeric_limits< Type >::has_denorm ); \
  test_if_bool_defined( std::numeric_limits< Type >::is_iec559 ); \
  test_if_bool_defined( std::numeric_limits< Type >::is_bounded ); \
  test_if_bool_defined( std::numeric_limits< Type >::is_modulo ); \
  test_if_bool_defined( std::numeric_limits< Type >::traps ); \
  test_if_bool_defined( std::numeric_limits< Type >::tinyness_before );

  TEST_TYPE(int);
  TEST_TYPE(long);
  TEST_TYPE(unsigned long);
  TEST_TYPE(short);
  TEST_TYPE(unsigned short);
  TEST_TYPE(float);
  TEST_TYPE(double);
#undef TEST_TYPE
}

#define TEST(m,x,y)    if ((x)!=(y)) { std::cout<< "FAIL: " << (m) << '\n'; fail=true; } \
                       else { std::cout<< "PASS: " << (m) << '\n'; }

int test_limits_main(int /*argc*/, char* /*argv*/[])
{
  // call it to avoid "unused function" compiler warnings,
  // and to force compilation with "very clever" compilers:
  test_static_const_definition();

  bool fail=false;
  std::cout << "dmax  = " << std::numeric_limits<double>::max() << std::endl
           << "dmin  = " << std::numeric_limits<double>::min() << std::endl
           << "deps  = " << std::numeric_limits<double>::epsilon() << std::endl
           << "dnmin = " << std::numeric_limits<double>::denorm_min() << std::endl
           << "dnan  = " << std::numeric_limits<double>::quiet_NaN() << std::endl
           << "dsnan = " << std::numeric_limits<double>::signaling_NaN() << std::endl
           << "dinf  = " << std::numeric_limits<double>::infinity() << std::endl
           << "-dinf = " <<-std::numeric_limits<double>::infinity() << std::endl
           << "rnder = " << std::numeric_limits<double>::round_error() << std::endl

           << "fmax  = " << std::numeric_limits<float>::max() << std::endl
           << "fmin  = " << std::numeric_limits<float>::min() << std::endl
           << "feps  = " << std::numeric_limits<float>::epsilon() << std::endl
           << "fnmin = " << std::numeric_limits<float>::denorm_min() << std::endl
           << "fnan  = " << std::numeric_limits<float>::quiet_NaN() << std::endl
           << "fsnan = " << std::numeric_limits<float>::signaling_NaN() << std::endl
           << "finf  = " << std::numeric_limits<float>::infinity() << std::endl
           << "-finf = " <<-std::numeric_limits<float>::infinity() << std::endl
           << "rnder = " << std::numeric_limits<float>::round_error() << std::endl

           << "s8max  = " << int(std::numeric_limits<signed char>::max()) << std::endl
           << "s8min  = " << int(std::numeric_limits<signed char>::min()) << std::endl

           << "u8max  = " << int(std::numeric_limits<unsigned char>::max()) << std::endl
           << "u8min  = " << int(std::numeric_limits<unsigned char>::min()) << std::endl

           << "s16max  = " << std::numeric_limits<signed short>::max() << std::endl
           << "s16min  = " << std::numeric_limits<signed short>::min() << std::endl

           << "u16max  = " << std::numeric_limits<unsigned short>::max() << std::endl
           << "u16min  = " << std::numeric_limits<unsigned short>::min() << std::endl

           << "s32max  = " << std::numeric_limits<signed int>::max() << std::endl
           << "s32min  = " << std::numeric_limits<signed int>::min() << std::endl

           << "u32max  = " << std::numeric_limits<unsigned int>::max() << std::endl
           << "u32min  = " << std::numeric_limits<unsigned int>::min() << std::endl;

  TEST("dmax", std::numeric_limits<double>::max() > 1e308, true);
  if (std::numeric_limits<double>::has_infinity) {
    TEST("dinf", std::numeric_limits<double>::infinity() >
                 std::numeric_limits<double>::max(), true);
  }
  TEST("dmin", std::numeric_limits<double>::min() < 1e-307 &&
               std::numeric_limits<double>::min() > 0, true);
  TEST("deps", std::numeric_limits<double>::epsilon() < 1e-12 &&
               std::numeric_limits<double>::epsilon() > 0, true);
  TEST("rnder",std::numeric_limits<double>::round_error() <= 1.0, true);
  TEST("fmax", std::numeric_limits<float>::max() > 1e38f, true);
  if (std::numeric_limits<float>::has_infinity) {
    TEST("finf", std::numeric_limits<float>::infinity() >
                 std::numeric_limits<float>::max(), true);
  }
  TEST("fmin", std::numeric_limits<float>::min() < 1e-37f &&
               std::numeric_limits<float>::min() > 0, true);
  TEST("feps", std::numeric_limits<float>::epsilon() < 1e-6f &&
               std::numeric_limits<float>::epsilon() > 0, true);
  TEST("rnder",std::numeric_limits<float>::round_error() <= 1.0, true);
  return fail?1:0;
}
