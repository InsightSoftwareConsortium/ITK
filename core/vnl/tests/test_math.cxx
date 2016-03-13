#include <iostream>
#include <iomanip>
#include <string>
#include <limits>
#include <vcl_compiler.h>
#include <vxl_config.h> // for VCL_STATIC_CONST_INIT_FLOAT_NO_DEFN
#include <vnl/vnl_math.h>
#include <vnl/vnl_complex.h> // for vnl_math::abs(std::complex)
#include <testlib/testlib_test.h>

//Utility function for printing hex representations
template<typename T>
std::string print_hex(const T p)
  {
  std::stringstream str;
  str << std::hex<<std::setfill('0')<<std::setw(2);
  for(int i = 0; i < (16-sizeof(p) ); ++i)
    {
    str << ".." ;
    }
  for (int i=(sizeof(p) -1 ); i>=0; --i)
    {
    str<<std::setfill('0')<<std::setw(2);
    const short curr_value = static_cast<short>( (reinterpret_cast<unsigned char const *>(&p))[i] );
    str<<curr_value;
    }
  str<<std::dec;
  return str.str();
  }

#if !VCL_STATIC_CONST_INIT_FLOAT_NO_DEFN
static
void check_pointer( const void * )
{
}

static
void test_static_const_definition()
{
  check_pointer( &vnl_math::e );
  check_pointer( &vnl_math::euler );
  check_pointer( &vnl_math::log2e );
  check_pointer( &vnl_math::log10e );
  check_pointer( &vnl_math::ln2 );
  check_pointer( &vnl_math::ln10 );
  check_pointer( &vnl_math::pi );
  check_pointer( &vnl_math::twopi );
  check_pointer( &vnl_math::pi_over_2 );
  check_pointer( &vnl_math::pi_over_4 );
  check_pointer( &vnl_math::pi_over_180 );
  check_pointer( &vnl_math::one_over_pi );
  check_pointer( &vnl_math::two_over_pi );
  check_pointer( &vnl_math::sqrt2pi );
  check_pointer( &vnl_math::one_over_sqrt2pi );
  check_pointer( &vnl_math::two_over_sqrtpi );
  check_pointer( &vnl_math::deg_per_rad );
  check_pointer( &vnl_math::sqrt2 );
  check_pointer( &vnl_math::sqrt1_2 );
  check_pointer( &vnl_math::sqrt1_3 );
  check_pointer( &vnl_math::eps );
  check_pointer( &vnl_math::sqrteps );
}
#endif

// Test that the vnl_math constants don't have weird values
static void test_math_constants()
{
#define TEST_CONSTANT(a,v) TEST_NEAR("value: ", vnl_math::a, v, 1e-9);
  TEST_NEAR("log of e is 1"     , log(vnl_math::e), 1.0, 1e-9);
  TEST_CONSTANT(e               , 2.7182818285);
  TEST_NEAR("log2e * ln2 = 1"   , vnl_math::log2e * vnl_math::ln2, 1.0, 1e-9);
  TEST_CONSTANT(log2e           , 1.4426950409);
  TEST_CONSTANT(ln2             , 0.6931471806);
  TEST_NEAR("log10e * ln10 = 1" , vnl_math::log10e * vnl_math::ln10, 1.0, 1e-9);
  TEST_CONSTANT(log10e          , 0.4342944819);
  TEST_CONSTANT(ln10            , 2.3025850930);
  TEST_NEAR("cos(pi) = -1"      , cos(vnl_math::pi), -1.0, 1e-9);
  TEST_CONSTANT(pi              , 3.1415926536);
  TEST_NEAR("twopi = 2*pi"      , vnl_math::twopi, 2.0*vnl_math::pi, 1e-9);
  TEST_CONSTANT(twopi           , 6.2831853072);
  TEST_NEAR("pi_over_2 = pi/2"  , vnl_math::pi_over_2, 0.5*vnl_math::pi, 1e-9);
  TEST_CONSTANT(pi_over_2       , 1.5707963268);
  TEST_NEAR("pi_over_4 = pi/4"  , vnl_math::pi_over_4, 0.25*vnl_math::pi, 1e-9);
  TEST_CONSTANT(pi_over_4       , 0.7853981634);
  TEST_NEAR("pi_over_180=pi/180", vnl_math::pi_over_180, vnl_math::pi/180.0, 1e-9);
  TEST_CONSTANT(pi_over_180     , 0.0174532925);
  TEST_NEAR("pi*one_over_pi=1"  , vnl_math::pi*vnl_math::one_over_pi, 1.0, 1e-9);
  TEST_CONSTANT(one_over_pi     , 0.3183098862);
  TEST_NEAR("pi*two_over_pi=2"  , vnl_math::pi*vnl_math::two_over_pi, 2.0, 1e-9);
  TEST_CONSTANT(two_over_pi     , 0.6366197724);
  TEST_NEAR("deg_per_rad=180/pi", vnl_math::deg_per_rad, 180.0/vnl_math::pi, 1e-9);
  TEST_CONSTANT(deg_per_rad     , 57.295779513);
  TEST_NEAR("sqrt2pi^2"         , vnl_math::sqrt2pi*vnl_math::sqrt2pi, vnl_math::twopi, 1e-9);
  TEST_CONSTANT(sqrt2pi         , 2.5066282746);
  TEST_NEAR("two_over_sqrtpi"   , vnl_math::two_over_sqrtpi, 2.0/sqrt(vnl_math::pi), 1e-9);
  TEST_CONSTANT(two_over_sqrtpi , 1.1283791671);
  TEST_NEAR("one_over_sqrt2pi"  , vnl_math::one_over_sqrt2pi, 1.0/sqrt(vnl_math::twopi), 1e-9);
  TEST_CONSTANT(one_over_sqrt2pi, 0.3989422804);
  TEST_NEAR("sqrt2*sqrt2=2"     , vnl_math::sqrt2*vnl_math::sqrt2, 2.0, 1e-9);
  TEST_CONSTANT(sqrt2           , 1.4142135624);
  TEST_NEAR("sqrt1_2*sqrt2=1"   , vnl_math::sqrt1_2*vnl_math::sqrt2, 1.0, 1e-9);
  TEST_CONSTANT(sqrt1_2         , 0.7071067812);
  TEST_NEAR("sqrt1_3^2=1/3"     , vnl_math::sqrt1_3*vnl_math::sqrt1_3, 1.0/3.0, 1e-9);
  TEST_CONSTANT(sqrt1_3         , 0.5773502692);
  TEST_NEAR("euler"             , vnl_math::euler, 0.5772156649, 1e-9);
#undef TEST_CONSTANT
}

static void test_math()
{
#if !VCL_STATIC_CONST_INIT_FLOAT_NO_DEFN
  // Call it to avoid compiler warnings
  test_static_const_definition();
#endif
  test_math_constants();

  int n = -11;
  float f = -7.5f;
  double d = -vnl_math::pi;
  std::complex<double> i(0,1);
  std::complex<double> z(-1,2);
  std::complex<double> e_ipi = std::exp(d*i);

  std::cout << "n = " << n << '\n'
           << "f = " << f << '\n'
           << "d = " << d << '\n'
           << "i = " << i << '\n'
           << "z = " << z << '\n'
           << "exp(d*i) = " << e_ipi << '\n'
           << '\n'

           << "abs(n) = " << vnl_math::abs(n) << '\n'
           << "abs(f) = " << vnl_math::abs(f) << '\n'
           << "abs(d) = " << vnl_math::abs(d) << '\n'
           << "abs(i) = " << vnl_math::abs(i) << '\n'
           << "abs(z) = " << vnl_math::abs(z) << '\n'
           <<"norm(z) = " << vnl_math::squared_magnitude(z) << '\n'
           << std::endl;

  TEST("abs(n) == 11", vnl_math::abs(n), 11);
  TEST("abs(f) == 7.5f", vnl_math::abs(f), 7.5f);
  TEST("abs(d) == pi", vnl_math::abs(d), vnl_math::pi);
  TEST("abs(i) == 1", vnl_math::abs(i), 1.0);
  TEST_NEAR("abs(-1+2i)~=sqrt(5)",vnl_math::abs(z),std::sqrt(5.0), 1e-12);
  TEST_NEAR("norm(-1+2i) ~= 5", vnl_math::squared_magnitude(z),5, 1e-12);
  TEST_NEAR("exp(d*i) ~= -1", vnl_math::abs(e_ipi+1.0), 0, 1e-12);
  std::cout << std::endl;

  TEST("rnd(-8.4999)  == -8  ", vnl_math::rnd(-8.4999), -8);
  TEST("rnd(-8.4999f) == -8  ", vnl_math::rnd(-8.4999f), -8);
  TEST("rnd(-8.50)    == -8/9", vnl_math::rnd(-8.50)/2, -4);
  TEST("rnd(-8.50f)   == -8/9", vnl_math::rnd(-8.50f)/2, -4);
  TEST("rnd(-8.5001)  == -9  ", vnl_math::rnd(-8.5001), -9);
  TEST("rnd(-8.5001f) == -9  ", vnl_math::rnd(-8.5001f), -9);
  TEST("rnd(8.4999)   ==  8  ", vnl_math::rnd(8.4999),  8);
  TEST("rnd(8.4999f)  ==  8  ", vnl_math::rnd(8.4999f),  8);
  TEST("rnd(8.50)     ==  8/9", vnl_math::rnd(8.50)/2,  4);
  TEST("rnd(8.50f)    ==  8/9", vnl_math::rnd(8.50f)/2,  4);
  TEST("rnd(8.5001)   ==  9  ", vnl_math::rnd(8.5001),  9);
  TEST("rnd(8.5001f)  ==  9  ", vnl_math::rnd(8.5001f),  9);

  TEST("rnd(-9.4999)  == -9   ", vnl_math::rnd(-9.4999), -9);
  TEST("rnd(-9.4999f) == -9   ", vnl_math::rnd(-9.4999f), -9);
  TEST("rnd(-9.50)    == -9/10", (vnl_math::rnd(-9.50)+1)/2, -4);
  TEST("rnd(-9.50f)   == -9/10", (vnl_math::rnd(-9.50f)+1)/2, -4);
  TEST("rnd(-9.5001)  == -10  ", vnl_math::rnd(-9.5001), -10);
  TEST("rnd(-9.5001f) == -10  ", vnl_math::rnd(-9.5001f), -10);
  TEST("rnd(9.4999)   ==  9   ", vnl_math::rnd(9.4999),  9);
  TEST("rnd(9.4999f)  ==  9   ", vnl_math::rnd(9.4999f),  9);
  TEST("rnd(9.50)     ==  9/10", (vnl_math::rnd(9.50)-1)/2,  4);
  TEST("rnd(9.50f)    ==  9/10", (vnl_math::rnd(9.50f)-1)/2,  4);
  TEST("rnd(9.5001)   ==  10  ", vnl_math::rnd(9.5001),  10);
  TEST("rnd(9.5001f)  ==  10  ", vnl_math::rnd(9.5001f),  10);

  TEST("rnd_halfinttoeven(-8.4999)  == -8", vnl_math::rnd_halfinttoeven(-8.4999), -8);
  TEST("rnd_halfinttoeven(-8.4999f) == -8", vnl_math::rnd_halfinttoeven(-8.4999f), -8);
  TEST("rnd_halfinttoeven(-8.50)    == -8", vnl_math::rnd_halfinttoeven(-8.50), -8);
  TEST("rnd_halfinttoeven(-8.50f)   == -8", vnl_math::rnd_halfinttoeven(-8.50f), -8);
  TEST("rnd_halfinttoeven(-8.5001)  == -9", vnl_math::rnd_halfinttoeven(-8.5001), -9);
  TEST("rnd_halfinttoeven(-8.5001f) == -9", vnl_math::rnd_halfinttoeven(-8.5001f), -9);
  TEST("rnd_halfinttoeven(8.4999)   ==  8", vnl_math::rnd_halfinttoeven(8.4999),  8);
  TEST("rnd_halfinttoeven(8.4999f)  ==  8", vnl_math::rnd_halfinttoeven(8.4999f),  8);
  TEST("rnd_halfinttoeven(8.50)     ==  8", vnl_math::rnd_halfinttoeven(8.50),  8);
  TEST("rnd_halfinttoeven(8.50f)    ==  8", vnl_math::rnd_halfinttoeven(8.50f),  8);
  TEST("rnd_halfinttoeven(8.5001)   ==  9", vnl_math::rnd_halfinttoeven(8.5001),  9);
  TEST("rnd_halfinttoeven(8.5001f)  ==  9", vnl_math::rnd_halfinttoeven(8.5001f),  9);

  TEST("rnd_halfinttoeven(-9.4999)  == -9 ", vnl_math::rnd_halfinttoeven(-9.4999), -9);
  TEST("rnd_halfinttoeven(-9.4999f) == -9 ", vnl_math::rnd_halfinttoeven(-9.4999f), -9);
  TEST("rnd_halfinttoeven(-9.50)    == -10", vnl_math::rnd_halfinttoeven(-9.50), -10);
  TEST("rnd_halfinttoeven(-9.50f)   == -10", vnl_math::rnd_halfinttoeven(-9.50f), -10);
  TEST("rnd_halfinttoeven(-9.5001)  == -10", vnl_math::rnd_halfinttoeven(-9.5001), -10);
  TEST("rnd_halfinttoeven(-9.5001f) == -10", vnl_math::rnd_halfinttoeven(-9.5001f), -10);
  TEST("rnd_halfinttoeven(9.4999)   ==  9 ", vnl_math::rnd_halfinttoeven(9.4999),  9);
  TEST("rnd_halfinttoeven(9.4999f)  ==  9 ", vnl_math::rnd_halfinttoeven(9.4999f),  9);
  TEST("rnd_halfinttoeven(9.50)     ==  10", vnl_math::rnd_halfinttoeven(9.50),  10);
  TEST("rnd_halfinttoeven(9.50f)    ==  10", vnl_math::rnd_halfinttoeven(9.50f),  10);
  TEST("rnd_halfinttoeven(9.5001)   ==  10", vnl_math::rnd_halfinttoeven(9.5001),  10);
  TEST("rnd_halfinttoeven(9.5001f)  ==  10", vnl_math::rnd_halfinttoeven(9.5001f),  10);

  TEST("rnd_halfintup(-8.4999)  == -8", vnl_math::rnd_halfintup(-8.4999), -8);
  TEST("rnd_halfintup(-8.4999f) == -8", vnl_math::rnd_halfintup(-8.4999f), -8);
  TEST("rnd_halfintup(-8.50)    == -8", vnl_math::rnd_halfintup(-8.50), -8);
  TEST("rnd_halfintup(-8.50f)   == -8", vnl_math::rnd_halfintup(-8.50f), -8);
  TEST("rnd_halfintup(-8.5001)  == -9", vnl_math::rnd_halfintup(-8.5001), -9);
  TEST("rnd_halfintup(-8.5001f) == -9", vnl_math::rnd_halfintup(-8.5001f), -9);
  TEST("rnd_halfintup(8.4999)   ==  8", vnl_math::rnd_halfintup(8.4999),  8);
  TEST("rnd_halfintup(8.4999f)  ==  8", vnl_math::rnd_halfintup(8.4999f),  8);
  TEST("rnd_halfintup(8.50)     ==  9", vnl_math::rnd_halfintup(8.50),  9);
  TEST("rnd_halfintup(8.50f)    ==  9", vnl_math::rnd_halfintup(8.50f),  9);
  TEST("rnd_halfintup(8.5001)   ==  9", vnl_math::rnd_halfintup(8.5001),  9);
  TEST("rnd_halfintup(8.5001f)  ==  9", vnl_math::rnd_halfintup(8.5001f),  9);

  TEST("rnd_halfintup(-9.4999)  == -9 ", vnl_math::rnd_halfintup(-9.4999), -9);
  TEST("rnd_halfintup(-9.4999f) == -9 ", vnl_math::rnd_halfintup(-9.4999f), -9);
  TEST("rnd_halfintup(-9.50)    == -9 ", vnl_math::rnd_halfintup(-9.50), -9);
  TEST("rnd_halfintup(-9.50f)   == -9 ", vnl_math::rnd_halfintup(-9.50f), -9);
  TEST("rnd_halfintup(-9.5001)  == -10", vnl_math::rnd_halfintup(-9.5001), -10);
  TEST("rnd_halfintup(-9.5001f) == -10", vnl_math::rnd_halfintup(-9.5001f), -10);
  TEST("rnd_halfintup(9.4999)   ==  9 ", vnl_math::rnd_halfintup(9.4999),  9);
  TEST("rnd_halfintup(9.4999f)  ==  9 ", vnl_math::rnd_halfintup(9.4999f),  9);
  TEST("rnd_halfintup(9.50)     ==  10", vnl_math::rnd_halfintup(9.50),  10);
  TEST("rnd_halfintup(9.50f)    ==  10", vnl_math::rnd_halfintup(9.50f),  10);
  TEST("rnd_halfintup(9.5001)   ==  10", vnl_math::rnd_halfintup(9.5001),  10);
  TEST("rnd_halfintup(9.5001f)  ==  10", vnl_math::rnd_halfintup(9.5001f),  10);

  TEST("floor(8.0)      ==  8", vnl_math::floor(8.0),  8);
  TEST("floor(8.0f)     ==  8", vnl_math::floor(8.0f),  8);
  TEST("floor(8.9999)   ==  8", vnl_math::floor(8.9999),  8);
  TEST("floor(8.9999f)  ==  8", vnl_math::floor(8.9999f),  8);
  TEST("floor(8.0001)   ==  8", vnl_math::floor(8.0001),  8);
  TEST("floor(8.0001f)  ==  8", vnl_math::floor(8.0001f),  8);
  TEST("floor(-8.0)     == -8", vnl_math::floor(-8.0), -8);
  TEST("floor(-8.0f)    == -8", vnl_math::floor(-8.0f), -8);
  TEST("floor(-8.9999)  == -9", vnl_math::floor(-8.9999), -9);
  TEST("floor(-8.9999f) == -9", vnl_math::floor(-8.9999f), -9);
  TEST("floor(-8.0001)  == -9", vnl_math::floor(-8.0001), -9);
  TEST("floor(-8.0001f) == -9", vnl_math::floor(-8.0001f), -9);

  TEST("floor(9.0)      ==  9 ", vnl_math::floor(9.0),  9);
  TEST("floor(9.0f)     ==  9 ", vnl_math::floor(9.0f),  9);
  TEST("floor(9.9999)   ==  9 ", vnl_math::floor(9.9999),  9);
  TEST("floor(9.9999f)  ==  9 ", vnl_math::floor(9.9999f),  9);
  TEST("floor(9.0001)   ==  9 ", vnl_math::floor(9.0001),  9);
  TEST("floor(9.0001f)  ==  9 ", vnl_math::floor(9.0001f),  9);
  TEST("floor(-9.0)     == -9 ", vnl_math::floor(-9.0), -9);
  TEST("floor(-9.0f)    == -9 ", vnl_math::floor(-9.0f), -9);
  TEST("floor(-9.9999)  == -10", vnl_math::floor(-9.9999), -10);
  TEST("floor(-9.9999f) == -10", vnl_math::floor(-9.9999f), -10);
  TEST("floor(-9.0001)  == -10", vnl_math::floor(-9.0001), -10);
  TEST("floor(-9.0001f) == -10", vnl_math::floor(-9.0001f), -10);

  TEST("ceil(8.0)      ==  8", vnl_math::ceil(8.0),  8);
  TEST("ceil(8.0f)     ==  8", vnl_math::ceil(8.0f),  8);
  TEST("ceil(8.9999)   ==  9", vnl_math::ceil(8.9999),  9);
  TEST("ceil(8.9999f)  ==  9", vnl_math::ceil(8.9999f),  9);
  TEST("ceil(8.0001)   ==  9", vnl_math::ceil(8.0001),  9);
  TEST("ceil(8.0001f)  ==  9", vnl_math::ceil(8.0001f),  9);
  TEST("ceil(-8.0)     == -8", vnl_math::ceil(-8.0), -8);
  TEST("ceil(-8.0f)    == -8", vnl_math::ceil(-8.0f), -8);
  TEST("ceil(-8.9999)  == -8", vnl_math::ceil(-8.9999), -8);
  TEST("ceil(-8.9999f) == -8", vnl_math::ceil(-8.9999f), -8);
  TEST("ceil(-8.0001)  == -8", vnl_math::ceil(-8.0001), -8);
  TEST("ceil(-8.0001f) == -8", vnl_math::ceil(-8.0001f), -8);

  TEST("ceil(9.0)      ==  9", vnl_math::ceil(9.0),  9);
  TEST("ceil(9.0f)     ==  9", vnl_math::ceil(9.0f),  9);
  TEST("ceil(9.9999)   == 10", vnl_math::ceil(9.9999), 10);
  TEST("ceil(9.9999f)  == 10", vnl_math::ceil(9.9999f), 10);
  TEST("ceil(9.0001)   == 10", vnl_math::ceil(9.0001), 10);
  TEST("ceil(9.0001f)  == 10", vnl_math::ceil(9.0001f), 10);
  TEST("ceil(-9.0)     == -9", vnl_math::ceil(-9.0), -9);
  TEST("ceil(-9.0f)    == -9", vnl_math::ceil(-9.0f), -9);
  TEST("ceil(-9.9999)  == -9", vnl_math::ceil(-9.9999), -9);
  TEST("ceil(-9.9999f) == -9", vnl_math::ceil(-9.9999f), -9);
  TEST("ceil(-9.0001)  == -9", vnl_math::ceil(-9.0001), -9);
  TEST("ceil(-9.0001f) == -9", vnl_math::ceil(-9.0001f), -9);

  TEST(" isfinite(f)    ",  vnl_math::isfinite(f), true);
  TEST(" isfinite(d)    ",  vnl_math::isfinite(d), true);
  TEST(" isfinite(i)    ",  vnl_math::isfinite(i), true);
  TEST(" isfinite(z)    ",  vnl_math::isfinite(z), true);


  // There is an assumption in this code that std::numeric_limits<float/double>::has_infinity==true

  TEST("std::numeric_limits<float>::has_infinity==true assumption",std::numeric_limits<float>::has_infinity, true);
  TEST("std::numeric_limits<double>::has_infinity==true assumption",std::numeric_limits<double>::has_infinity, true);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
  TEST("std::numeric_limits<ldouble>::has_infinity==true assumption",std::numeric_limits<long double>::has_infinity, true);
#endif
  if (! std::numeric_limits<float>::has_infinity && ! std::numeric_limits<double>::has_infinity)
  {
    std::cout << "Your platform doesn't appear to have an infinity. VXL is in places relatively\n"
             << "dependent on the existence of an infinity. There are two solutions.\n"
             << "A. If your platform really doesn't have an infinity, VXL's configuration code\n"
             << "   can be modified to correctly detect and use the infinity.\n"
             << "B. Fix VXL so that it can cope with the lack of an infinity.\n" << std::endl;
  }
  TEST("std::numeric_limits<float>::has_quiet_NaN==true assumption",std::numeric_limits<float>::has_quiet_NaN, true);
  TEST("std::numeric_limits<double>::has_quiet_NaN==true assumption",std::numeric_limits<double>::has_quiet_NaN, true);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
  TEST("std::numeric_limits<ldouble>::has_quiet_NaN==true assumption",std::numeric_limits<long double>::has_quiet_NaN, true);
#endif
  if (! std::numeric_limits<float>::has_quiet_NaN && ! std::numeric_limits<double>::has_quiet_NaN)
  {
    std::cout << "Your platform doesn't appear to have a quiet NaN. VXL is in places relatively\n"
             << "dependent on the existence of a quiet NaN. There are two solutions.\n"
             << "A. If your platform really doesn't have a quiet NaN, VXL's configuration code\n"
             << "   can be modified to correctly detect and use the NaN.\n"
             << "B. Fix VXL so that it can cope with the lack of a quiet NaN.\n" << std::endl;
  }
  // Create Inf and -Inf:
  const float pinf_f =   std::numeric_limits<float>::infinity();
  const float ninf_f = - std::numeric_limits<float>::infinity();
  const double pinf_d =   std::numeric_limits<double>::infinity();
  const double ninf_d = - std::numeric_limits<double>::infinity();
  // Create NaN
  const float qnan_f = std::numeric_limits<float>::quiet_NaN();
  const double qnan_d = std::numeric_limits<double>::quiet_NaN();
#ifdef INCLUDE_LONG_DOUBLE_TESTS
  const long double pinf_q =  std::numeric_limits<long double>::infinity();
  const long double ninf_q = -std::numeric_limits<long double>::infinity();
  const long double qnan_q = std::numeric_limits<long double>::quiet_NaN();
#endif

  std::cout << "pinf_f = " << pinf_f << " =  "<< print_hex(pinf_f) << "    sizeof " << sizeof(pinf_f) << '\n'
           << "ninf_f = " << ninf_f << " = " << print_hex(ninf_f) << "    sizeof " << sizeof(ninf_f) << '\n'
           << "pinf_d = " << pinf_d << " =  "<< print_hex(pinf_d) << "    sizeof " << sizeof(pinf_d) << '\n'
           << "ninf_d = " << ninf_d << " = " << print_hex(ninf_d) << "    sizeof " << sizeof(ninf_d) << '\n'
           << "qnan_f = " << qnan_f << " =  "<< print_hex(qnan_f) << "    sizeof " << sizeof(qnan_f) << '\n'
           << "qnan_d = " << qnan_d << " =  "<< print_hex(qnan_d) << "    sizeof " << sizeof(qnan_d) << '\n'
#ifdef INCLUDE_LONG_DOUBLE_TESTS
           << "pinf_q = " << pinf_q << " =  "<< print_hex(pinf_q) << "    sizeof " << sizeof(pinf_q) << '\n'
           << "ninf_q = " << ninf_q << " = " << print_hex(ninf_q) << "    sizeof " << sizeof(ninf_q) << '\n'
           << "qnan_q = " << qnan_q << " =  "<< print_hex(qnan_q) << "    sizeof " << sizeof(qnan_q) << '\n'
#endif
           << std::endl;

  TEST("!isfinite(pinf_f)", vnl_math::isfinite(pinf_f), false);
  TEST("!isfinite(ninf_f)", vnl_math::isfinite(ninf_f), false);
  TEST(" isinf(pinf_f)   ",  vnl_math::isinf(pinf_f), true);
  TEST(" isinf(ninf_f)   ",  vnl_math::isinf(ninf_f), true);
  TEST("!isnan(pinf_f)   ", vnl_math::isnan(pinf_f), false);
  TEST("!isnan(ninf_f)   ", vnl_math::isnan(ninf_f), false);
  TEST("!isfinite(qnan_f)", vnl_math::isfinite(qnan_f), false);
  TEST("!isinf(qnan_f)   ", vnl_math::isinf(qnan_f), false);
  TEST(" isnan(qnan_f)   ",  vnl_math::isnan(qnan_f), true);

  TEST("!isfinite(pinf_d)", vnl_math::isfinite(pinf_d), false);
  TEST("!isfinite(ninf_d)", vnl_math::isfinite(ninf_d), false);
  TEST(" isinf(pinf_d)   ",  vnl_math::isinf(pinf_d), true);
  TEST(" isinf(ninf_d)   ",  vnl_math::isinf(ninf_d), true);
  TEST("!isnan(pinf_d)   ", vnl_math::isnan(pinf_d), false);
  TEST("!isnan(ninf_d)   ", vnl_math::isnan(ninf_d), false);
  TEST("!isfinite(qnan_d)", vnl_math::isfinite(qnan_d), false);
  TEST("!isinf(qnan_d)   ", vnl_math::isinf(qnan_d), false);
  TEST(" isnan(qnan_d)   ",  vnl_math::isnan(qnan_d), true);

#ifdef INCLUDE_LONG_DOUBLE_TESTS
#ifndef __ICC // "long double" has no standard internal representation on different platforms/compilers
  TEST("!isfinite(pinf_q)", vnl_math::isfinite(pinf_q), false);
  TEST("!isfinite(ninf_q)", vnl_math::isfinite(ninf_q), false);
  TEST(" isinf(pinf_q)   ",  vnl_math::isinf(pinf_q), true);
  TEST(" isinf(ninf_q)   ",  vnl_math::isinf(ninf_q), true);
  TEST("!isnan(pinf_q)   ", vnl_math::isnan(pinf_q), false);
  TEST("!isnan(ninf_q)   ", vnl_math::isnan(ninf_q), false);
  TEST("!isfinite(qnan_q)", vnl_math::isfinite(qnan_q), false);
  TEST("!isinf(qnan_q)   ", vnl_math::isinf(qnan_q), false);
#endif // __ICC
#endif

  TEST("!isfinite(huge_val(double))", vnl_math::isfinite(vnl_huge_val(double())), false);
  TEST("!isfinite(huge_val(float))",  vnl_math::isfinite(vnl_huge_val(float())),  false);

  //Test for math_sgn
  TEST("vnl_math::sgn(+7)  ", vnl_math::sgn(+7),  1);
  TEST("vnl_math::sgn(-7)  ", vnl_math::sgn(-7), -1);
  TEST("vnl_math::sgn( 0)  ", vnl_math::sgn( 0),  0);

  TEST("vnl_math::sgn(+7.0)  ", vnl_math::sgn(+7.0),  1);
  TEST("vnl_math::sgn(-7.0)  ", vnl_math::sgn(-7.0), -1);
  TEST("vnl_math::sgn(-0.0)  ", vnl_math::sgn(-0.0),  0);
  TEST("vnl_math::sgn(+0.0)  ", vnl_math::sgn(-0.0),  0);

  TEST("vnl_math::sgn(+7.0F)  ", vnl_math::sgn(+7.0F),  1);
  TEST("vnl_math::sgn(-7.0F)  ", vnl_math::sgn(-7.0F), -1);
  TEST("vnl_math::sgn(-0.0F)  ", vnl_math::sgn(-0.0F),  0);
  TEST("vnl_math::sgn(+0.0F)  ", vnl_math::sgn(-0.0F),  0);

  std::cout << std::endl;

  // test vnl_math::angle_0_to_2pi() for "extreme values":
  TEST("vnl_math::angle_0_to_2pi(2pi)", vnl_math::angle_0_to_2pi(vnl_math::twopi), 0.0);
  double eps = 2e-16; // which is smaller than the precision of vnl_math::pi
  double conv_eps = vnl_math::angle_0_to_2pi(-eps);
  std::cout << "conv_eps = " << conv_eps << " = 2pi - " << vnl_math::twopi-conv_eps << std::endl;
  TEST("vnl_math::angle_0_to_2pi(-eps)", conv_eps < vnl_math::twopi && conv_eps > 6.283, true);
  eps = 2e-15; // which is larger than the precision of vnl_math::pi
  conv_eps = vnl_math::angle_0_to_2pi(-eps);
  std::cout << "conv_eps = " << conv_eps << " = 2pi - " << vnl_math::twopi-conv_eps << std::endl;
  TEST("vnl_math::angle_0_to_2pi(-10eps)", conv_eps < vnl_math::twopi - 1e-15 && conv_eps > 6.283, true);
  double ang = vnl_math::twopi - eps;
  double conv_ang = vnl_math::angle_0_to_2pi(ang);
  std::cout << "conv_ang = " << conv_ang << " = 2pi - " << vnl_math::twopi-conv_ang << std::endl;
  TEST("vnl_math::angle_0_to_2pi(2pi-10eps)", conv_ang, ang);
  // test vnl_math::angle_minuspi_to_pi() for "extreme values":
  TEST("vnl_math::angle_minuspi_to_pi(2pi)", vnl_math::angle_minuspi_to_pi(vnl_math::twopi), 0.0);
  TEST("vnl_math::angle_minuspi_to_pi(pi)", vnl_math::angle_minuspi_to_pi(vnl_math::pi), vnl_math::pi);
  TEST("vnl_math::angle_minuspi_to_pi(-pi)", vnl_math::angle_minuspi_to_pi(-vnl_math::pi), -vnl_math::pi);
  eps = 2e-16; // which is smaller than the precision of vnl_math::pi
  conv_eps = vnl_math::angle_minuspi_to_pi(-eps);
  std::cout << "conv_eps = " << conv_eps << std::endl;
  TEST("vnl_math::angle_minuspi_to_pi(-eps)", conv_eps, -eps);
  eps = 2e-15; // which is larger than the precision of vnl_math::pi
  conv_eps = vnl_math::angle_minuspi_to_pi(-eps);
  std::cout << "conv_eps = " << conv_eps << std::endl;
  TEST("vnl_math::angle_minuspi_to_pi(-10eps)", conv_eps, -eps);

  ///////////////
  // TRUNCATED //
  ///////////////

  std::cout << "Truncated Remainder:" << std::endl;
  // Truncated Remainder (% for integers, fmod for floating point)
  // This behavior is most familiar to c++ programmers, but is unusual
  // in mathematical terms.

    {

    std::cout << "+ +" << std::endl;

    unsigned short x_short_u     = 7;
    unsigned short y_short_u     = 2;
      signed short x_short_s     = 7;
      signed short y_short_s     = 2;
    unsigned int   x_int_u       = 7;
    unsigned int   y_int_u       = 2;
      signed int   x_int_s       = 7;
      signed int   y_int_s       = 2;
    unsigned long  x_long_u      = 7;
    unsigned long  y_long_u      = 2;
      signed long  x_long_s      = 7;
      signed long  y_long_s      = 2;
    float          x_float       = 7;
    float          y_float       = 2;
    double         x_double      = 7;
    double         y_double      = 2;
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    long double    x_long_double = 7;
    long double    y_long_double = 2;
#endif

    TEST("vnl_math::remainder_truncated(x_short_u    ,y_short_u    )",vnl_math::remainder_truncated(x_short_u    ,y_short_u    ),+1);
    TEST("vnl_math::remainder_truncated(x_short_s    ,y_short_s    )",vnl_math::remainder_truncated(x_short_s    ,y_short_s    ),+1);
    TEST("vnl_math::remainder_truncated(x_int_u      ,y_int_u      )",vnl_math::remainder_truncated(x_int_u      ,y_int_u      ),+1);
    TEST("vnl_math::remainder_truncated(x_int_s      ,y_int_s      )",vnl_math::remainder_truncated(x_int_s      ,y_int_s      ),+1);
    TEST("vnl_math::remainder_truncated(x_long_u     ,y_long_u     )",vnl_math::remainder_truncated(x_long_u     ,y_long_u     ),+1);
    TEST("vnl_math::remainder_truncated(x_long_s     ,y_long_s     )",vnl_math::remainder_truncated(x_long_s     ,y_long_s     ),+1);
    TEST("vnl_math::remainder_truncated(x_float      ,y_float      )",vnl_math::remainder_truncated(x_float      ,y_float      ),+1);
    TEST("vnl_math::remainder_truncated(x_double     ,y_double     )",vnl_math::remainder_truncated(x_double     ,y_double     ),+1);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    TEST("vnl_math::remainder_truncated(x_long_double,y_long_double)",vnl_math::remainder_truncated(x_long_double,y_long_double),+1);
#endif

    std::cout << "+ -" << std::endl;

    y_short_s     *= -1;
    y_int_s       *= -1;
    y_long_s      *= -1;
    y_float       *= -1;
    y_double      *= -1;
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    y_long_double *= -1;
#endif

    TEST("vnl_math::remainder_truncated(x_short_s    ,y_short_s    )",vnl_math::remainder_truncated(x_short_s    ,y_short_s    ),+1);
    TEST("vnl_math::remainder_truncated(x_int_s      ,y_int_s      )",vnl_math::remainder_truncated(x_int_s      ,y_int_s      ),+1);
    TEST("vnl_math::remainder_truncated(x_long_s     ,y_long_s     )",vnl_math::remainder_truncated(x_long_s     ,y_long_s     ),+1);
    TEST("vnl_math::remainder_truncated(x_float      ,y_float      )",vnl_math::remainder_truncated(x_float      ,y_float      ),+1);
    TEST("vnl_math::remainder_truncated(x_double     ,y_double     )",vnl_math::remainder_truncated(x_double     ,y_double     ),+1);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    TEST("vnl_math::remainder_truncated(x_long_double,y_long_double)",vnl_math::remainder_truncated(x_long_double,y_long_double),+1);
#endif

    std::cout << "- -" << std::endl;

    x_short_s     *= -1;
    x_int_s       *= -1;
    x_long_s      *= -1;
    x_float       *= -1;
    x_double      *= -1;
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    x_long_double *= -1;
#endif

    TEST("vnl_math::remainder_truncated(x_short_s    ,y_short_s    )",vnl_math::remainder_truncated(x_short_s    ,y_short_s    ),-1);
    TEST("vnl_math::remainder_truncated(x_int_s      ,y_int_s      )",vnl_math::remainder_truncated(x_int_s      ,y_int_s      ),-1);
    TEST("vnl_math::remainder_truncated(x_long_s     ,y_long_s     )",vnl_math::remainder_truncated(x_long_s     ,y_long_s     ),-1);
    TEST("vnl_math::remainder_truncated(x_float      ,y_float      )",vnl_math::remainder_truncated(x_float      ,y_float      ),-1);
    TEST("vnl_math::remainder_truncated(x_double     ,y_double     )",vnl_math::remainder_truncated(x_double     ,y_double     ),-1);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    TEST("vnl_math::remainder_truncated(x_long_double,y_long_double)",vnl_math::remainder_truncated(x_long_double,y_long_double),-1);
#endif

    std::cout << "- +" << std::endl;

    y_short_s     *= -1;
    y_int_s       *= -1;
    y_long_s      *= -1;
    y_float       *= -1;
    y_double      *= -1;
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    y_long_double *= -1;
#endif

    TEST("vnl_math::remainder_truncated(x_short_s    ,y_short_s    )",vnl_math::remainder_truncated(x_short_s    ,y_short_s    ),-1);
    TEST("vnl_math::remainder_truncated(x_int_s      ,y_int_s      )",vnl_math::remainder_truncated(x_int_s      ,y_int_s      ),-1);
    TEST("vnl_math::remainder_truncated(x_long_s     ,y_long_s     )",vnl_math::remainder_truncated(x_long_s     ,y_long_s     ),-1);
    TEST("vnl_math::remainder_truncated(x_float      ,y_float      )",vnl_math::remainder_truncated(x_float      ,y_float      ),-1);
    TEST("vnl_math::remainder_truncated(x_double     ,y_double     )",vnl_math::remainder_truncated(x_double     ,y_double     ),-1);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    TEST("vnl_math::remainder_truncated(x_long_double,y_long_double)",vnl_math::remainder_truncated(x_long_double,y_long_double),-1);
#endif
    }

  /////////////
  // FLOORED //
  /////////////

  std::cout << "Floored Remainder:" << std::endl;
  // This definition is identical to truncated_remainder for two positive arguments.
  // When one or both of the arguments are negative, this attempts to mimick Python's modulus behavior.

    {

    std::cout << "+ +" << std::endl;

    unsigned short x_short_u     = 7;
    unsigned short y_short_u     = 2;
      signed short x_short_s     = 7;
      signed short y_short_s     = 2;
    unsigned int   x_int_u       = 7;
    unsigned int   y_int_u       = 2;
      signed int   x_int_s       = 7;
      signed int   y_int_s       = 2;
    unsigned long  x_long_u      = 7;
    unsigned long  y_long_u      = 2;
      signed long  x_long_s      = 7;
      signed long  y_long_s      = 2;
    float          x_float       = 7;
    float          y_float       = 2;
    double         x_double      = 7;
    double         y_double      = 2;
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    long double    x_long_double = 7;
    long double    y_long_double = 2;
#endif

    TEST("vnl_math::remainder_floored(x_short_u    ,y_short_u    )",vnl_math::remainder_floored(x_short_u    ,y_short_u    ),+1);
    TEST("vnl_math::remainder_floored(x_short_s    ,y_short_s    )",vnl_math::remainder_floored(x_short_s    ,y_short_s    ),+1);
    TEST("vnl_math::remainder_floored(x_int_u      ,y_int_u      )",vnl_math::remainder_floored(x_int_u      ,y_int_u      ),+1);
    TEST("vnl_math::remainder_floored(x_int_s      ,y_int_s      )",vnl_math::remainder_floored(x_int_s      ,y_int_s      ),+1);
    TEST("vnl_math::remainder_floored(x_long_u     ,y_long_u     )",vnl_math::remainder_floored(x_long_u     ,y_long_u     ),+1);
    TEST("vnl_math::remainder_floored(x_long_s     ,y_long_s     )",vnl_math::remainder_floored(x_long_s     ,y_long_s     ),+1);
    TEST("vnl_math::remainder_floored(x_float      ,y_float      )",vnl_math::remainder_floored(x_float      ,y_float      ),+1);
    TEST("vnl_math::remainder_floored(x_double     ,y_double     )",vnl_math::remainder_floored(x_double     ,y_double     ),+1);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    TEST("vnl_math::remainder_floored(x_long_double,y_long_double)",vnl_math::remainder_floored(x_long_double,y_long_double),+1);
#endif

    std::cout << "+ -" << std::endl;

    y_short_s     *= -1;
    y_int_s       *= -1;
    y_long_s      *= -1;
    y_float       *= -1;
    y_double      *= -1;
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    y_long_double *= -1;
#endif

    TEST("vnl_math::remainder_floored(x_short_s    ,y_short_s    )",vnl_math::remainder_floored(x_short_s    ,y_short_s    ),-1);
    TEST("vnl_math::remainder_floored(x_int_s      ,y_int_s      )",vnl_math::remainder_floored(x_int_s      ,y_int_s      ),-1);
    TEST("vnl_math::remainder_floored(x_long_s     ,y_long_s     )",vnl_math::remainder_floored(x_long_s     ,y_long_s     ),-1);
    TEST("vnl_math::remainder_floored(x_float      ,y_float      )",vnl_math::remainder_floored(x_float      ,y_float      ),-1);
    TEST("vnl_math::remainder_floored(x_double     ,y_double     )",vnl_math::remainder_floored(x_double     ,y_double     ),-1);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    TEST("vnl_math::remainder_floored(x_long_double,y_long_double)",vnl_math::remainder_floored(x_long_double,y_long_double),-1);
#endif

    std::cout << "- -" << std::endl;

    x_short_s     *= -1;
    x_int_s       *= -1;
    x_long_s      *= -1;
    x_float       *= -1;
    x_double      *= -1;
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    x_long_double *= -1;
#endif

    TEST("vnl_math::remainder_floored(x_short_s    ,y_short_s    )",vnl_math::remainder_floored(x_short_s    ,y_short_s    ),-1);
    TEST("vnl_math::remainder_floored(x_int_s      ,y_int_s      )",vnl_math::remainder_floored(x_int_s      ,y_int_s      ),-1);
    TEST("vnl_math::remainder_floored(x_long_s     ,y_long_s     )",vnl_math::remainder_floored(x_long_s     ,y_long_s     ),-1);
    TEST("vnl_math::remainder_floored(x_float      ,y_float      )",vnl_math::remainder_floored(x_float      ,y_float      ),-1);
    TEST("vnl_math::remainder_floored(x_double     ,y_double     )",vnl_math::remainder_floored(x_double     ,y_double     ),-1);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    TEST("vnl_math::remainder_floored(x_long_double,y_long_double)",vnl_math::remainder_floored(x_long_double,y_long_double),-1);
#endif

    std::cout << "- +" << std::endl;

    y_short_s     *= -1;
    y_int_s       *= -1;
    y_long_s      *= -1;
    y_float       *= -1;
    y_double      *= -1;
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    y_long_double *= -1;
#endif

    TEST("vnl_math::remainder_floored(x_short_s    ,y_short_s    )",vnl_math::remainder_floored(x_short_s    ,y_short_s    ),+1);
    TEST("vnl_math::remainder_floored(x_int_s      ,y_int_s      )",vnl_math::remainder_floored(x_int_s      ,y_int_s      ),+1);
    TEST("vnl_math::remainder_floored(x_long_s     ,y_long_s     )",vnl_math::remainder_floored(x_long_s     ,y_long_s     ),+1);
    TEST("vnl_math::remainder_floored(x_float      ,y_float      )",vnl_math::remainder_floored(x_float      ,y_float      ),+1);
    TEST("vnl_math::remainder_floored(x_double     ,y_double     )",vnl_math::remainder_floored(x_double     ,y_double     ),+1);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    TEST("vnl_math::remainder_floored(x_long_double,y_long_double)",vnl_math::remainder_floored(x_long_double,y_long_double),+1);
#endif
    }

}

TESTMAIN(test_math);
