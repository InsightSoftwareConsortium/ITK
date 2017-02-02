// This is core/vnl/vnl_numeric_traits.h
#ifndef vnl_numeric_traits_h_
#define vnl_numeric_traits_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
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
#include <vcl_compiler.h>
#include "vnl/vnl_export.h"

// this is an empty class template.
// only the specializations make sense.
#if !defined(VCL_VC) || (_MSC_VER >= 1800 )
template <class T>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits;
#else
// However, *some* compilers require the template to be defined
// under some circumstances...
// Since the non-specialized template doesn't make any sense, make
// sure that any types "accidently" derived from it will cause
// compiler errors.
class VNL_EXPORT vnl_numeric_traits_not_a_valid_type { };
template <class T>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR vnl_numeric_traits_not_a_valid_type zero;

  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR vnl_numeric_traits_not_a_valid_type one;

  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR vnl_numeric_traits_not_a_valid_type maxval;

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
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<bool>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR bool zero VCL_STATIC_CONST_INIT_INT_DECL(false);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR bool one VCL_STATIC_CONST_INIT_INT_DECL(true);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR bool maxval VCL_STATIC_CONST_INIT_INT_DECL(true);
  //: Return value of abs()
  typedef unsigned int abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned int double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template <>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<bool const> : public vnl_numeric_traits<bool> {};
#endif

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<char>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR char zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR char one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume.
  //  It is 127 (and not 255) since "char" is not guaranteed to be unsigned.
#ifdef _MSC_VER
#ifdef _CHAR_UNSIGNED
  static VNL_EXPORT VXL_CONSTEXPR_VAR char maxval VCL_STATIC_CONST_INIT_INT_DECL(255);
#else
  static VNL_EXPORT VXL_CONSTEXPR_VAR char maxval VCL_STATIC_CONST_INIT_INT_DECL(127);
#endif
#else
  static VNL_EXPORT VXL_CONSTEXPR_VAR char maxval VCL_STATIC_CONST_INIT_INT_DECL(char(255)<char(0)?char(127):char(255));
#endif
  //: Return value of abs()
  typedef unsigned char abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef short double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<char const> : public vnl_numeric_traits<char> {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<unsigned char>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned char zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned char one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned char maxval VCL_STATIC_CONST_INIT_INT_DECL(255);
  //: Return value of abs()
  typedef unsigned char abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned short double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<unsigned char const> : public vnl_numeric_traits<unsigned char> {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<signed char>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR signed char zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR signed char one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR signed char maxval VCL_STATIC_CONST_INIT_INT_DECL(127);
  //: Return value of abs()
  typedef unsigned char abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef signed short double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<signed char const> : public vnl_numeric_traits<signed char> {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<short>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR short zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR short one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR short maxval VCL_STATIC_CONST_INIT_INT_DECL(0x7fff); // = 0x7fff;
  //: Return value of abs()
  typedef unsigned short abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef int double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<short const> : public vnl_numeric_traits<short> {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<unsigned short>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned short zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned short one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned short maxval VCL_STATIC_CONST_INIT_INT_DECL(0xffff); // = 0xffff;
  //: Return value of abs()
  typedef unsigned short abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned int double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<unsigned short const> : public vnl_numeric_traits<unsigned short> {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<int>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR int zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR int one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR int maxval VCL_STATIC_CONST_INIT_INT_DECL(0x7fffffff); // = 0x7fffffff;
  //: Return value of abs()
  typedef unsigned int abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef long double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<int const> : public vnl_numeric_traits<int> {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<unsigned int>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned int zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned int one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned int maxval VCL_STATIC_CONST_INIT_INT_DECL(0xffffffff); // = 0xffffffff;
  //: Return value of abs()
  typedef unsigned int abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned long double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<unsigned int const> : public vnl_numeric_traits<unsigned int> {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<long>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR long zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR long one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR long maxval VCL_STATIC_CONST_INIT_INT_DECL(sizeof(long)==8?(vxl_uint_64)(-1)/2:0x7fffffffL); // = 0x7fffffffL or 0x7fffffffffffffffL;
  //: Return value of abs()
  typedef unsigned long abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vxl_sint_64 double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<long const> : public vnl_numeric_traits<long > {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<unsigned long>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned long zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned long one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned long maxval VCL_STATIC_CONST_INIT_INT_DECL( sizeof(unsigned long)==8?((vxl_uint_64)(-1)):0xffffffffL );
  // = 0xffffffffL or 0xffffffffffffffffL;
  //: Return value of abs()
  typedef unsigned long abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vxl_uint_64 double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<unsigned long const> : public vnl_numeric_traits<unsigned long> {};

#if defined(_WIN64) && !VCL_HAS_LONG_LONG
template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<size_t>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR size_t zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR size_t one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR size_t maxval VCL_STATIC_CONST_INIT_INT_DECL(0x7fffffff); // = 0x7fffffff;
  //: Return value of abs()
  typedef size_t abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef size_t double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<size_t const> : public vnl_numeric_traits<size_t> {};
#endif

#if VCL_HAS_LONG_LONG
template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<long long>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR long long zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR long long one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR long long maxval VCL_STATIC_CONST_INIT_INT_DECL( sizeof(long long)==8?((vxl_uint_64)(-1))/2:0x7fffffffL );
  //: Return value of abs()
  typedef unsigned long long abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef long long double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<long long const> : public vnl_numeric_traits<long long> {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<unsigned long long>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned long long zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned long long one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR unsigned long long maxval VCL_STATIC_CONST_INIT_INT_DECL(sizeof(unsigned long long)==8?(vxl_uint_64)(-1):0xffffffffL);
  //: Return value of abs()
  typedef unsigned long long abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned long long double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<unsigned long long const> : public vnl_numeric_traits<unsigned long long> {};
#endif

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<float>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR float zero VCL_STATIC_CONST_INIT_FLOAT_DECL(0.0F);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR float one VCL_STATIC_CONST_INIT_FLOAT_DECL(1.0F);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR float maxval VCL_STATIC_CONST_INIT_FLOAT_DECL(3.40282346638528860e+38F);
  //: Return value of abs()
  typedef float abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef double double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<float const> : public vnl_numeric_traits<float> {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<double>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR double zero VCL_STATIC_CONST_INIT_FLOAT_DECL(0.0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR double one VCL_STATIC_CONST_INIT_FLOAT_DECL(1.0);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR double maxval VCL_STATIC_CONST_INIT_FLOAT_DECL(1.7976931348623157E+308);
  //: Return value of abs()
  typedef double abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef long double double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<double const> : public vnl_numeric_traits<double> {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<long double>
{
 public:
  //: Additive identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR long double zero VCL_STATIC_CONST_INIT_FLOAT_DECL(0.0);
  //: Multiplicative identity
  static VNL_EXPORT VXL_CONSTEXPR_VAR long double one VCL_STATIC_CONST_INIT_FLOAT_DECL(1.0);
  //: Maximum value which this type can assume
  static VNL_EXPORT VXL_CONSTEXPR_VAR long double maxval VCL_STATIC_CONST_INIT_FLOAT_DECL(1.7976931348623157E+308);
  //: Return value of abs()
  typedef long double abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef long double double_t; // ahem
  //: Name of type which results from multiplying this type with a double
  typedef long double real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<long double const> : public vnl_numeric_traits<long double> {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits< std::complex<float> >
{
 public:
  //: Additive identity
  static VNL_EXPORT const std::complex<float> zero;
  //: Multiplicative identity
  static VNL_EXPORT const std::complex<float> one;
  // Maximum value which this type can assume; makes no sense for this type
  //static VNL_EXPORT const std::complex<float> maxval;

  //: Return value of abs()
  typedef float abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef std::complex<vnl_numeric_traits<float>::double_t> double_t;
  //: Name of type which results from multiplying this type with a double
  typedef std::complex<float> real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<std::complex<float> const> : public vnl_numeric_traits<std::complex<float> > {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits< std::complex<double> >
{
 public:
  //: Additive identity
  static VNL_EXPORT const std::complex<double> zero;
  //: Multiplicative identity
  static VNL_EXPORT const std::complex<double> one;
  // Maximum value which this type can assume; makes no sense for this type
  //static VNL_EXPORT const std::complex<double> maxval;

  //: Return value of abs()
  typedef double abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef std::complex<vnl_numeric_traits<double>::double_t> double_t;
  //: Name of type which results from multiplying this type with a double
  typedef std::complex<double> real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<std::complex<double> const> : public vnl_numeric_traits<std::complex<double> > {};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits< std::complex<long double> >
{
 public:
  //: Additive identity
  static VNL_EXPORT const std::complex<long double> zero;
  //: Multiplicative identity
  static VNL_EXPORT const std::complex<long double> one;
  // Maximum value which this type can assume; makes no sense for this type
  //static VNL_EXPORT const std::complex<long double> maxval;

  //: Return value of abs()
  typedef long double abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef std::complex<vnl_numeric_traits<long double>::double_t> double_t;
  //: Name of type which results from multiplying this type with a double
  typedef std::complex<long double> real_t;
};

template<>
class VNL_TEMPLATE_EXPORT vnl_numeric_traits<std::complex<long double> const> : public vnl_numeric_traits<std::complex<long double> > {};

#endif // vnl_numeric_traits_h_
