// This is core/vnl/tests/test_numeric_traits.cxx
#include <vnl/vnl_numeric_traits.h>
#include <testlib/testlib_test.h>
#include <vcl_complex.h>
#include <vcl_iostream.h>
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
  ALL(long double);
  ONE_ZERO( vcl_complex<float> );
  ONE_ZERO( vcl_complex<double> );
  ONE_ZERO( vcl_complex<long double> );

#undef ONE_ZERO
#undef ALL
}

extern "C" { long increment(long x) { return x+1; } }

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
  TEST("vnl_numeric_traits<long double>::zero", vnl_numeric_traits<long double>::zero, 0.0);
  TEST("vnl_numeric_traits<long double>::one", vnl_numeric_traits<long double>::one, 1.0);
  TEST("vnl_numeric_traits<vcl_complex<float> >::zero",
       vnl_numeric_traits<vcl_complex<float> >::zero, vcl_complex<float>(0.0f));
  TEST("vnl_numeric_traits<vcl_complex<float> >::one",
       vnl_numeric_traits<vcl_complex<float> >::one, vcl_complex<float>(1.0f));
  TEST("vnl_numeric_traits<vcl_complex<double> >::zero",
       vnl_numeric_traits<vcl_complex<double> >::zero, vcl_complex<double>(0.0));
  TEST("vnl_numeric_traits<vcl_complex<double> >::one",
       vnl_numeric_traits<vcl_complex<double> >::one, vcl_complex<double>(1.0));
  TEST("vnl_numeric_traits<vcl_complex<long double> >::zero",
       vnl_numeric_traits<vcl_complex<long double> >::zero, vcl_complex<long double>(0.0));
  TEST("vnl_numeric_traits<vcl_complex<long double> >::one",
       vnl_numeric_traits<vcl_complex<long double> >::one, vcl_complex<long double>(1.0));

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
  long double ldm = vnl_numeric_traits<long double>::maxval;

  vcl_cout << " vnl_numeric_traits<bool>::maxval = " << vnl_numeric_traits<bool>::maxval << '\n'
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
           << " vnl_numeric_traits<long double>::maxval = " << ldm << '\n';

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
  TEST("vnl_numeric_traits<long double>::maxval must be at least as large", ldm>=dm, true);

  // Verify that there is nothing larger than these maxval values:
  ++cm;  TEST("vnl_numeric_traits<char>::maxval must be the largest possible", cm<=0, true);
  if (cm > 0) vcl_cout << cm << " is larger\n";
  ++scm; TEST("vnl_numeric_traits<signed char>::maxval must be the largest possible", scm<0, true);
  if (scm > 0) vcl_cout << scm << " is larger\n";
  ++ucm; TEST("vnl_numeric_traits<unsigned char>::maxval must be the largest possible", ucm==0, true);
  if (ucm > 0) vcl_cout << ucm << " is larger\n";
  ++sm;  TEST("vnl_numeric_traits<short>::maxval must be the largest possible", sm<0, true);
  if (sm > 0) vcl_cout << sm << " is larger\n";
  ++usm; TEST("vnl_numeric_traits<unsigned short>::maxval must be the largest possible", usm==0, true);
  if (usm > 0) vcl_cout << usm << " is larger\n";
  im = increment(im); TEST("vnl_numeric_traits<int>::maxval must be the largest possible", im<0, true);
  if (im > 0) vcl_cout << im << " is larger\n";
  ++uim; TEST("vnl_numeric_traits<unsigned int>::maxval must be the largest possible", uim==0, true);
  if (uim > 0) vcl_cout << uim << " is larger\n";
  lm=increment(lm);  TEST("vnl_numeric_traits<long>::maxval must be the largest possible", lm<0, true);
  if (lm > 0) vcl_cout << lm << " is larger\n";
  ++ulm; TEST("vnl_numeric_traits<unsigned long>::maxval must be the largest possible", ulm==0, true);
  if (ulm > 0) vcl_cout << ulm << " is larger\n";

  unsigned char* x = (unsigned char*)(&fm);
  int nr_of_ones = 0;
  vcl_cout << "vnl_numeric_traits<float>::maxval has internal representation ";
#if VXL_BIG_ENDIAN
  for (unsigned int i=0; i<sizeof(float); ++i)
#else
  for (int i=sizeof(float)-1; i>=0; --i)
#endif
    for (int j=7; j>=0; --j) {
      int n = int(((x[i])>>j)&1);
      nr_of_ones += n;
      vcl_cout << n;
    }
  vcl_cout << '\n';
  // there should only be 2 zeros in the representation: the sign bits of mantissa and of exponent:
  TEST("vnl_numeric_traits<float>::maxval must be the largest possible", nr_of_ones, 8*sizeof(float)-2);

  x = (unsigned char*)(&dm);
  nr_of_ones = 0;
  vcl_cout << "vnl_numeric_traits<double>::maxval has internal representation ";
#if VXL_BIG_ENDIAN
  for (unsigned int i=0; i<sizeof(double); ++i)
#else
  for (int i=sizeof(double)-1; i>=0; --i)
#endif
    for (int j=7; j>=0; --j) {
      int n = int(((x[i])>>j)&1);
      nr_of_ones += n;
      vcl_cout << n;
    }
  vcl_cout << '\n';
  // there should only be 2 zeros in the representation: the sign bits of mantissa and of exponent:
  TEST("vnl_numeric_traits<double>::maxval must be the largest possible", nr_of_ones, 8*sizeof(double)-2);

  x = (unsigned char*)(&ldm);
#if 0
      // See TODO below.  Do not set if not used.
  nr_of_ones = 0;
#endif
  vcl_cout << "vnl_numeric_traits<long double>::maxval has internal representation ";
#if VXL_BIG_ENDIAN
  for (unsigned int i=0; i<sizeof(long double); ++i)
#else
  for (int i=sizeof(long double)-1; i>=0; --i)
#endif
    for (int j=7; j>=0; --j) {
      int n = int(((x[i])>>j)&1);
#if 0
      // See TODO below.  Do not set if not used.
      nr_of_ones += n;
#endif
      vcl_cout << n;
    }
  vcl_cout << '\n';
#if 0 // TODO - long double has non-standard length on differnet platforms
  // there should only be 2 zeros in the representation: the sign bits of mantissa and of exponent:
  TEST("vnl_numeric_traits<long double>::maxval must be the largest possible", nr_of_ones, 8*sizeof(long double)-2);
#endif
}

TESTMAIN(test_numeric_traits);
