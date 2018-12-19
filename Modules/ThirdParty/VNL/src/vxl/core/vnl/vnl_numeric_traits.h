// This is core/vnl/vnl_numeric_traits.h
#ifndef vnl_numeric_traits_h_
#define vnl_numeric_traits_h_
//:
// \file
// \brief Templated zero/one/precision
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   04 Sep 96
//
//  To allow templated numerical algorithms to determine appropriate
//  values for zero, one, maxval, and types for double precision,
//  maximum product etc.
//
// \verbatim
//  Modifications
//   980212           AWF      Initial version.
//   AWF              010498   Moved to math
//   LSB (Manchester) 23/3/01  Documentation tidied
//   Peter Vanroose   14/7/01  vnl_rational added
//   Peter Vanroose   14/10/01 vnl_rational moved to vnl_rational.h
//   AWF              250202   Add const T specializations for the basic types.
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <complex>
#include <vxl_config.h> // for type vxl_uint_64
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include "vnl/vnl_export.h"

// this is an empty class template.
// only the specializations make sense.
#if !defined(_MSC_VER) || (_MSC_VER >= 1800 )
template <class T>
class VNL_EXPORT vnl_numeric_traits;
#else
// However, *some* compilers require the template to be defined
// under some circumstances...
// Since the non-specialized template doesn't make any sense, make
// sure that any types "accidently" derived from it will cause
// compiler errors.
class VNL_EXPORT vnl_numeric_traits_not_a_valid_type { };
template <class T>
class VNL_EXPORT vnl_numeric_traits
{
 public:
  //: Additive identity
  static constexpr vnl_numeric_traits_not_a_valid_type zero;

  //: Multiplicative identity
  static constexpr vnl_numeric_traits_not_a_valid_type one;

  //: Maximum value which this type can assume
  static constexpr vnl_numeric_traits_not_a_valid_type maxval;

  //: Return value of abs()
  typedef vnl_numeric_traits_not_a_valid_type abs_t;

  //: Name of a type twice as long as this one for accumulators and products.
  typedef vnl_numeric_traits_not_a_valid_type double_t;

  //: Name of type which results from multiplying this type with a double
  typedef vnl_numeric_traits_not_a_valid_type real_t;
};
#endif

#ifndef NO_STD_BOOL
template <>
class VNL_EXPORT vnl_numeric_traits<bool>
{
 public:
  //: Additive identity
  static constexpr bool zero = false;
  //: Multiplicative identity
  static constexpr bool one = true;
  //: Maximum value which this type can assume
  static constexpr bool maxval = true;
  //: Return value of abs()
  typedef unsigned int abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned int double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template <>
class VNL_EXPORT vnl_numeric_traits<bool const> : public vnl_numeric_traits<bool> {};
#endif

template<>
class VNL_EXPORT vnl_numeric_traits<char>
{
 public:
  //: Additive identity
  static constexpr char zero = 0;
  //: Multiplicative identity
  static constexpr char one = 1;
  //: Maximum value which this type can assume.
  //  It is 127 (and not 255) since "char" is not guaranteed to be unsigned.
#ifdef _MSC_VER
#ifdef _CHAR_UNSIGNED
  static constexpr char maxval = 255;
#else
  static constexpr char maxval = 127;
#endif
#else
  static constexpr char maxval = (char(255)<char(0)?char(127):char(255));
#endif
  //: Return value of abs()
  typedef unsigned char abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef short double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<char const> : public vnl_numeric_traits<char> {};

template<>
class VNL_EXPORT vnl_numeric_traits<unsigned char>
{
 public:
  //: Additive identity
  static constexpr unsigned char zero = 0;
  //: Multiplicative identity
  static constexpr unsigned char one = 1;
  //: Maximum value which this type can assume
  static constexpr unsigned char maxval = 255;
  //: Return value of abs()
  typedef unsigned char abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned short double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<unsigned char const> : public vnl_numeric_traits<unsigned char> {};

template<>
class VNL_EXPORT vnl_numeric_traits<signed char>
{
 public:
  //: Additive identity
  static constexpr signed char zero = 0;
  //: Multiplicative identity
  static constexpr signed char one = 1;
  //: Maximum value which this type can assume
  static constexpr signed char maxval = 127;
  //: Return value of abs()
  typedef unsigned char abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef signed short double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<signed char const> : public vnl_numeric_traits<signed char> {};

template<>
class VNL_EXPORT vnl_numeric_traits<short>
{
 public:
  //: Additive identity
  static constexpr short zero  = 0;
  //: Multiplicative identity
  static constexpr short one = 1;
  //: Maximum value which this type can assume
  static constexpr short maxval = 0x7fff; // = 0x7fff;
  //: Return value of abs()
  typedef unsigned short abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef int double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<short const> : public vnl_numeric_traits<short> {};

template<>
class VNL_EXPORT vnl_numeric_traits<unsigned short>
{
 public:
  //: Additive identity
  static constexpr unsigned short zero  = 0;
  //: Multiplicative identity
  static constexpr unsigned short one = 1;
  //: Maximum value which this type can assume
  static constexpr unsigned short maxval = 0xffff; // = 0xffff;
  //: Return value of abs()
  typedef unsigned short abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned int double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<unsigned short const> : public vnl_numeric_traits<unsigned short> {};

template<>
class VNL_EXPORT vnl_numeric_traits<int>
{
 public:
  //: Additive identity
  static constexpr int zero  = 0;
  //: Multiplicative identity
  static constexpr int one = 1;
  //: Maximum value which this type can assume
  static constexpr int maxval = 0x7fffffff; // = 0x7fffffff;
  //: Return value of abs()
  typedef unsigned int abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef long double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<int const> : public vnl_numeric_traits<int> {};

template<>
class VNL_EXPORT vnl_numeric_traits<unsigned int>
{
 public:
  //: Additive identity
  static constexpr unsigned int zero  = 0;
  //: Multiplicative identity
  static constexpr unsigned int one = 1;
  //: Maximum value which this type can assume
  static constexpr unsigned int maxval = 0xffffffff; // = 0xffffffff;
  //: Return value of abs()
  typedef unsigned int abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned long double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<unsigned int const> : public vnl_numeric_traits<unsigned int> {};

template<>
class VNL_EXPORT vnl_numeric_traits<long>
{
 public:
  //: Additive identity
  static constexpr long zero  = 0;
  //: Multiplicative identity
  static constexpr long one = 1;
  //: Maximum value which this type can assume
  static constexpr long maxval = sizeof(long)==8?static_cast<long>((vxl_uint_64)(-1)/2):0x7fffffffL; // = 0x7fffffffL or 0x7fffffffffffffffL;
  //: Return value of abs()
  typedef unsigned long abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vxl_sint_64 double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<long const> : public vnl_numeric_traits<long > {};

template<>
class VNL_EXPORT vnl_numeric_traits<unsigned long>
{
 public:
  //: Additive identity
  static constexpr unsigned long zero  = 0;
  //: Multiplicative identity
  static constexpr unsigned long one = 1;
  //: Maximum value which this type can assume
  static constexpr unsigned long maxval =  sizeof(unsigned long)==8?static_cast<unsigned long>((vxl_uint_64)(-1)):0xffffffffL ;
  // = 0xffffffffL or 0xffffffffffffffffL;
  //: Return value of abs()
  typedef unsigned long abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vxl_uint_64 double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<unsigned long const> : public vnl_numeric_traits<unsigned long> {};


//long long - target type will have width of at least 64 bits. (since C++11)
template<>
class VNL_EXPORT vnl_numeric_traits<long long>
{
 public:
  //: Additive identity
  static constexpr long long zero  = 0;
  //: Multiplicative identity
  static constexpr long long one = 1;
  //: Maximum value which this type can assume
  static constexpr long long maxval =  sizeof(long long)==8?((vxl_uint_64)(-1))/2:0x7fffffffL ;
  //: Return value of abs()
  typedef unsigned long long abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef long long double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<long long const> : public vnl_numeric_traits<long long> {};

template<>
class VNL_EXPORT vnl_numeric_traits<unsigned long long>
{
 public:
  //: Additive identity
  static constexpr unsigned long long zero  = 0;
  //: Multiplicative identity
  static constexpr unsigned long long one = 1;
  //: Maximum value which this type can assume
  static constexpr unsigned long long maxval = sizeof(unsigned long long)==8?(vxl_uint_64)(-1):0xffffffffL;
  //: Return value of abs()
  typedef unsigned long long abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned long long double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<unsigned long long const> : public vnl_numeric_traits<unsigned long long> {};


template<>
class VNL_EXPORT vnl_numeric_traits<float>
{
 public:
  //: Additive identity
  static constexpr float zero = 0.0F;
  //: Multiplicative identity
  static constexpr float one = 1.0F;
  //: Maximum value which this type can assume
  static constexpr float maxval = 3.40282346638528860e+38F;
  //: Return value of abs()
  typedef float abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef double double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<float const> : public vnl_numeric_traits<float> {};

template<>
class VNL_EXPORT vnl_numeric_traits<double>
{
 public:
  //: Additive identity
  static constexpr double zero = 0.0;
  //: Multiplicative identity
  static constexpr double one = 1.0;
  //: Maximum value which this type can assume
  static constexpr double maxval = 1.7976931348623157E+308;
  //: Return value of abs()
  typedef double abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef long double double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<double const> : public vnl_numeric_traits<double> {};

template<>
class VNL_EXPORT vnl_numeric_traits<long double>
{
 public:
  //: Additive identity
  static constexpr long double zero = 0.0;
  //: Multiplicative identity
  static constexpr long double one = 1.0;
  //: Maximum value which this type can assume
  static constexpr long double maxval = 1.7976931348623157E+308;
  //: Return value of abs()
  typedef long double abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef long double double_t; // ahem
  //: Name of type which results from multiplying this type with a double
  typedef long double real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<long double const> : public vnl_numeric_traits<long double> {};

template<>
class VNL_EXPORT vnl_numeric_traits< std::complex<float> >
{
 public:
  //: Additive identity
  static const std::complex<float> zero;
  //: Multiplicative identity
  static const std::complex<float> one;
  // Maximum value which this type can assume; makes no sense for this type
  //static const std::complex<float> maxval;

  //: Return value of abs()
  typedef float abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef std::complex<vnl_numeric_traits<float>::double_t> double_t;
  //: Name of type which results from multiplying this type with a double
  typedef std::complex<float> real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<std::complex<float> const> : public vnl_numeric_traits<std::complex<float> > {};

template<>
class VNL_EXPORT vnl_numeric_traits< std::complex<double> >
{
 public:
  //: Additive identity
  static const std::complex<double> zero;
  //: Multiplicative identity
  static const std::complex<double> one;
  // Maximum value which this type can assume; makes no sense for this type
  //static const std::complex<double> maxval;

  //: Return value of abs()
  typedef double abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef std::complex<vnl_numeric_traits<double>::double_t> double_t;
  //: Name of type which results from multiplying this type with a double
  typedef std::complex<double> real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<std::complex<double> const> : public vnl_numeric_traits<std::complex<double> > {};

template<>
class VNL_EXPORT vnl_numeric_traits< std::complex<long double> >
{
 public:
  //: Additive identity
  static const std::complex<long double> zero;
  //: Multiplicative identity
  static const std::complex<long double> one;
  // Maximum value which this type can assume; makes no sense for this type
  //static const std::complex<long double> maxval;

  //: Return value of abs()
  typedef long double abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef std::complex<vnl_numeric_traits<long double>::double_t> double_t;
  //: Name of type which results from multiplying this type with a double
  typedef std::complex<long double> real_t;
};

template<>
class VNL_EXPORT vnl_numeric_traits<std::complex<long double> const> : public vnl_numeric_traits<std::complex<long double> > {};

#endif // vnl_numeric_traits_h_
