// This is core/vnl/tests/test_numeric_traits.cxx
#include <complex>
#include <iostream>
#include <vnl/vnl_numeric_traits.h>
#include <testlib/testlib_test.h>
#include <vcl_compiler.h>
#include <vxl_config.h> // for VXL_BIG_ENDIAN

static
void check_pointer( const void* )
{
}

static
void test_static_const_definition()
{
#define ONE_ZERO( Type ) \
  do {\
    check_pointer( &vnl_numeric_traits< Type >::zero );\
    check_pointer( &vnl_numeric_traits< Type >::one );\
    check_pointer( &vnl_numeric_traits< const Type >::zero );\
    check_pointer( &vnl_numeric_traits< const Type >::one );\
  } while (false)
#define ALL( Type ) \
  ONE_ZERO( Type ); \
  do {\
    check_pointer( &vnl_numeric_traits< Type >::maxval );\
    check_pointer( &vnl_numeric_traits< const Type >::maxval );\
  } while (false)

  ALL(bool);
  ALL(char);
  ALL(unsigned char);
  ALL(signed char);
  ALL(short);
  ALL(unsigned short);
  ALL(int);
  ALL(unsigned int);
  ALL(long);
  ALL(unsigned long);
  ALL(float);
  ALL(double);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
  ALL(long double);
#endif
  ONE_ZERO( std::complex<float> );
  ONE_ZERO( std::complex<double> );
#ifdef INCLUDE_LONG_DOUBLE_TESTS
  ONE_ZERO( std::complex<long double> );
#endif

#undef ONE_ZERO
#undef ALL
}

#ifdef INCLUDE_LONG_DOUBLE_TESTS
extern "C" { long double increment(long double x) { return x+1; } }
#endif

void test_numeric_traits()
{
  // call it to avoid compiler warnings
  test_static_const_definition();

  TEST("vnl_numeric_traits<bool>::zero", vnl_numeric_traits<bool>::zero, false);
  TEST("vnl_numeric_traits<bool>::one", vnl_numeric_traits<bool>::one, true);
  TEST("vnl_numeric_traits<char>::zero", vnl_numeric_traits<char>::zero, '\0');
  TEST("vnl_numeric_traits<char>::one", vnl_numeric_traits<char>::one, 1);
  TEST("vnl_numeric_traits<unsigned char>::zero", vnl_numeric_traits<unsigned char>::zero, '\0');
  TEST("vnl_numeric_traits<unsigned char>::one", vnl_numeric_traits<unsigned char>::one, 1);
  TEST("vnl_numeric_traits<signed char>::zero", vnl_numeric_traits<signed char>::zero, '\0');
  TEST("vnl_numeric_traits<signed char>::one", vnl_numeric_traits<signed char>::one, 1);
  TEST("vnl_numeric_traits<short>::zero", vnl_numeric_traits<short>::zero, 0);
  TEST("vnl_numeric_traits<short>::one", vnl_numeric_traits<short>::one, 1);
  TEST("vnl_numeric_traits<unsigned short>::zero", vnl_numeric_traits<unsigned short>::zero, 0);
  TEST("vnl_numeric_traits<unsigned short>::one", vnl_numeric_traits<unsigned short>::one, 1);
  TEST("vnl_numeric_traits<signed short>::zero", vnl_numeric_traits<signed short>::zero, 0);
  TEST("vnl_numeric_traits<signed short>::one", vnl_numeric_traits<signed short>::one, 1);
  TEST("vnl_numeric_traits<int>::zero", vnl_numeric_traits<int>::zero, 0);
  TEST("vnl_numeric_traits<int>::one", vnl_numeric_traits<int>::one, 1);
  TEST("vnl_numeric_traits<signed int>::zero", vnl_numeric_traits<signed int>::zero, 0);
  TEST("vnl_numeric_traits<signed int>::one", vnl_numeric_traits<signed int>::one, 1);
  TEST("vnl_numeric_traits<unsigned int>::zero", vnl_numeric_traits<unsigned int>::zero, 0);
  TEST("vnl_numeric_traits<unsigned int>::one", vnl_numeric_traits<unsigned int>::one, 1);
  TEST("vnl_numeric_traits<long>::zero", vnl_numeric_traits<long>::zero, 0L);
  TEST("vnl_numeric_traits<long>::one", vnl_numeric_traits<long>::one, 1L);
  TEST("vnl_numeric_traits<signed long>::zero", vnl_numeric_traits<signed long>::zero, 0L);
  TEST("vnl_numeric_traits<signed long>::one", vnl_numeric_traits<signed long>::one, 1L);
  TEST("vnl_numeric_traits<unsigned long>::zero", vnl_numeric_traits<unsigned long>::zero, 0L);
  TEST("vnl_numeric_traits<unsigned long>::one", vnl_numeric_traits<unsigned long>::one, 1L);
  TEST("vnl_numeric_traits<float>::zero", vnl_numeric_traits<float>::zero, 0.0f);
  TEST("vnl_numeric_traits<float>::one", vnl_numeric_traits<float>::one, 1.0f);
  TEST("vnl_numeric_traits<double>::zero", vnl_numeric_traits<double>::zero, 0.0);
  TEST("vnl_numeric_traits<double>::one", vnl_numeric_traits<double>::one, 1.0);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
  TEST("vnl_numeric_traits<long double>::zero", vnl_numeric_traits<long double>::zero, 0.0);
  TEST("vnl_numeric_traits<long double>::one", vnl_numeric_traits<long double>::one, 1.0);
#endif
  TEST("vnl_numeric_traits<std::complex<float> >::zero",
       vnl_numeric_traits<std::complex<float> >::zero, std::complex<float>(0.0f));
  TEST("vnl_numeric_traits<std::complex<float> >::one",
       vnl_numeric_traits<std::complex<float> >::one, std::complex<float>(1.0f));
  TEST("vnl_numeric_traits<std::complex<double> >::zero",
       vnl_numeric_traits<std::complex<double> >::zero, std::complex<double>(0.0));
  TEST("vnl_numeric_traits<std::complex<double> >::one",
       vnl_numeric_traits<std::complex<double> >::one, std::complex<double>(1.0));
#ifdef INCLUDE_LONG_DOUBLE_TESTS
  TEST("vnl_numeric_traits<std::complex<long double> >::zero",
       vnl_numeric_traits<std::complex<long double> >::zero, std::complex<long double>(0.0));
  TEST("vnl_numeric_traits<std::complex<long double> >::one",
       vnl_numeric_traits<std::complex<long double> >::one, std::complex<long double>(1.0));
#endif
  // Testing maxval values

  char cm = vnl_numeric_traits<char>::maxval;
  signed char scm = vnl_numeric_traits<signed char>::maxval;
  unsigned char ucm = vnl_numeric_traits<unsigned char>::maxval;
  short sm = vnl_numeric_traits<short>::maxval;
  unsigned short usm = vnl_numeric_traits<unsigned short>::maxval;
  int im = vnl_numeric_traits<int>::maxval;
  unsigned int uim = vnl_numeric_traits<unsigned int>::maxval;
  long lm = vnl_numeric_traits<long>::maxval;
  unsigned long ulm = vnl_numeric_traits<unsigned long>::maxval;
  float fm = vnl_numeric_traits<float>::maxval;
  double dm = vnl_numeric_traits<double>::maxval;
#ifdef INCLUDE_LONG_DOUBLE_TESTS
  long double ldm = vnl_numeric_traits<long double>::maxval;
#endif

  std::cout << " vnl_numeric_traits<bool>::maxval = " << vnl_numeric_traits<bool>::maxval << '\n'
           << " vnl_numeric_traits<char>::maxval = " << (int)cm << '\n'
           << " vnl_numeric_traits<signed char>::maxval = " << (int)scm << '\n'
           << " vnl_numeric_traits<unsigned char>::maxval = " << (int)ucm << '\n'
           << " vnl_numeric_traits<short>::maxval = " << sm << '\n'
           << " vnl_numeric_traits<unsigned short>::maxval = " << usm << '\n'
           << " vnl_numeric_traits<int>::maxval = " << im << '\n'
           << " vnl_numeric_traits<unsigned int>::maxval = " << uim << '\n'
           << " vnl_numeric_traits<long>::maxval = " << lm << '\n'
           << " vnl_numeric_traits<unsigned long>::maxval = " << ulm << '\n'
           << " vnl_numeric_traits<float>::maxval = " << fm << '\n'
           << " vnl_numeric_traits<double>::maxval = " << dm << '\n'
#ifdef INCLUDE_LONG_DOUBLE_TESTS
           << " vnl_numeric_traits<long double>::maxval = " << ldm << '\n'
#endif
           << '\n';

  // Verify that these values are positive and satisfy certain constraints:
  TEST("vnl_numeric_traits<char>::maxval must be at least 127", cm >= 127, true);
  TEST("vnl_numeric_traits<signed char>::maxval must be at least 127", scm >= 127, true);
  TEST("vnl_numeric_traits<unsigned char>::maxval must be larger than that", ucm>scm, true);
  TEST("vnl_numeric_traits<short>::maxval must be larger than that", sm>ucm, true);
  TEST("vnl_numeric_traits<int>::maxval must be at least as large", im>=sm, true);
  TEST("vnl_numeric_traits<unsigned short>::maxval must be larger than <short>", usm>sm, true);
  TEST("vnl_numeric_traits<unsigned int>::maxval must be at least as large", uim>=usm && uim>(unsigned int)im, true);
  TEST("vnl_numeric_traits<long>::maxval must be at least equal to <int>", lm>=im, true);
  TEST("vnl_numeric_traits<unsigned long>::maxval must be larger than that", ulm>(unsigned long)lm, true);
  TEST("vnl_numeric_traits<float>::maxval must be at least 1e33", fm>1e33, true);
  TEST("vnl_numeric_traits<double>::maxval must be larger than that", dm>fm, true);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
  TEST("vnl_numeric_traits<long double>::maxval must be at least as large", ldm>=dm, true);
#endif

  // Verify that there is nothing larger than these maxval values:
  // unsigned cases:
  ++ucm; TEST("vnl_numeric_traits<unsigned char>::maxval must be the largest possible", ucm==0, true);
  if (ucm > 0) std::cout << ucm << " is larger\n";
  ++usm; TEST("vnl_numeric_traits<unsigned short>::maxval must be the largest possible", usm==0, true);
  if (usm > 0) std::cout << usm << " is larger\n";
  ++uim; TEST("vnl_numeric_traits<unsigned int>::maxval must be the largest possible", uim==0, true);
  if (uim > 0) std::cout << uim << " is larger\n";
  ++ulm; TEST("vnl_numeric_traits<unsigned long>::maxval must be the largest possible", ulm==0, true);
  if (ulm > 0) std::cout << ulm << " is larger\n";
#ifdef TEST_SIGNED_OVERFLOW // "signed overflow" might give compiler warnings or even worse ...
  // signed cases:
  ++scm; TEST("vnl_numeric_traits<signed char>::maxval must be the largest possible", scm<0, true);
  if (scm > 0) std::cout << scm << " is larger\n";
  ++cm;  TEST("vnl_numeric_traits<char>::maxval must be the largest possible", cm<=0, true);
  if (cm > 0) std::cout << cm << " is larger\n";
  ++sm;  TEST("vnl_numeric_traits<short>::maxval must be the largest possible", sm<0, true);
  if (sm > 0) std::cout << sm << " is larger\n";
  im = increment(im); TEST("vnl_numeric_traits<int>::maxval must be the largest possible", im<0, true);
  if (im > 0) std::cout << im << " is larger\n";
  lm=increment(lm);  TEST("vnl_numeric_traits<long>::maxval must be the largest possible", lm<0, true);
  if (lm > 0) std::cout << lm << " is larger\n";
#endif // TEST_SIGNED_OVERFLOW

  unsigned char* x = (unsigned char*)(&fm);
  int nr_of_ones = 0;
  std::cout << "vnl_numeric_traits<float>::maxval has internal representation ";
#if VXL_BIG_ENDIAN
  for (unsigned int i=0; i<sizeof(float); ++i)
#else
  for (int i=sizeof(float)-1; i>=0; --i)
#endif
    for (int j=7; j>=0; --j) {
      int n = int(((x[i])>>j)&1);
      nr_of_ones += n;
      std::cout << n;
    }
  std::cout << '\n';
  // there should only be 2 zeros in the representation: the sign bits of mantissa and of exponent:
  TEST("vnl_numeric_traits<float>::maxval must be the largest possible", nr_of_ones, 8*sizeof(float)-2);

  x = (unsigned char*)(&dm);
  nr_of_ones = 0;
  std::cout << "vnl_numeric_traits<double>::maxval has internal representation ";
#if VXL_BIG_ENDIAN
  for (unsigned int i=0; i<sizeof(double); ++i)
#else
  for (int i=sizeof(double)-1; i>=0; --i)
#endif
    for (int j=7; j>=0; --j) {
      int n = int(((x[i])>>j)&1);
      nr_of_ones += n;
      std::cout << n;
    }
  std::cout << '\n';
  // there should only be 2 zeros in the representation: the sign bits of mantissa and of exponent:
  TEST("vnl_numeric_traits<double>::maxval must be the largest possible", nr_of_ones, 8*sizeof(double)-2);

#ifdef INCLUDE_LONG_DOUBLE_TESTS
  x = (unsigned char*)(&ldm);
  std::cout << "vnl_numeric_traits<long double>::maxval has internal representation ";
#if VXL_BIG_ENDIAN
  for (unsigned int i=0; i<sizeof(long double); ++i)
#else
  for (int i=sizeof(long double)-1; i>=0; --i)
#endif
    for (int j=7; j>=0; --j) {
      int n = int(((x[i])>>j)&1);
      std::cout << n;
    }
  std::cout << '\n';
#endif
}

TESTMAIN(test_numeric_traits);
