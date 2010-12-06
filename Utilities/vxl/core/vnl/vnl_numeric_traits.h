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

#include <vxl_config.h> // for type vxl_uint_64
#include <vcl_complex.h>

// this is an empty class template.
// only the specializations make sense.
#if !defined(VCL_VC)
template <class T>
class vnl_numeric_traits;
#else
// However, *some* compilers require the template to be defined
// under some circumstances...
// Since the non-specialized template doesn't make any sense, make
// sure that any types "accidently" derived from it will cause
// compiler errors.
class vnl_numeric_traits_not_a_valid_type { };
template <class T>
class vnl_numeric_traits
{
 public:
  //: Additive identity
  static const vnl_numeric_traits_not_a_valid_type zero;

  //: Multiplicative identity
  static const vnl_numeric_traits_not_a_valid_type one;

  //: Maximum value which this type can assume
  static const vnl_numeric_traits_not_a_valid_type maxval;

  //: Return value of abs()
  typedef vnl_numeric_traits_not_a_valid_type abs_t;

  //: Name of a type twice as long as this one for accumulators and products.
  typedef vnl_numeric_traits_not_a_valid_type double_t;

  //: Name of type which results from multiplying this type with a double
  typedef vnl_numeric_traits_not_a_valid_type real_t;
};
#endif

#ifndef NO_STD_BOOL
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<bool>
{
 public:
  //: Additive identity
  static const bool zero VCL_STATIC_CONST_INIT_INT_DECL(false);
  //: Multiplicative identity
  static const bool one VCL_STATIC_CONST_INIT_INT_DECL(true);
  //: Maximum value which this type can assume
  static const bool maxval VCL_STATIC_CONST_INIT_INT_DECL(true);
  //: Return value of abs()
  typedef unsigned int abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned int double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<bool const> : public vnl_numeric_traits<bool> {};
#endif
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<char>
{
 public:
  //: Additive identity
  static const char zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static const char one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume.
  //  It is 127 (and not 255) since "char" is not guaranteed to be unsigned.
#ifdef _MSC_VER
#ifdef _CHAR_UNSIGNED
  static const char maxval VCL_STATIC_CONST_INIT_INT_DECL(255);
#else
  static const char maxval VCL_STATIC_CONST_INIT_INT_DECL(127);
#endif
#else
  static const char maxval VCL_STATIC_CONST_INIT_INT_DECL(char(255)<0?127:255);
#endif
  //: Return value of abs()
  typedef unsigned char abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef short double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<char const> : public vnl_numeric_traits<char> {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<unsigned char>
{
 public:
  //: Additive identity
  static const unsigned char zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static const unsigned char one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static const unsigned char maxval VCL_STATIC_CONST_INIT_INT_DECL(255);
  //: Return value of abs()
  typedef unsigned char abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned short double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<unsigned char const> : public vnl_numeric_traits<unsigned char> {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<signed char>
{
 public:
  //: Additive identity
  static const signed char zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static const signed char one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static const signed char maxval VCL_STATIC_CONST_INIT_INT_DECL(127);
  //: Return value of abs()
  typedef unsigned char abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef signed short double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<signed char const> : public vnl_numeric_traits<signed char> {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<short>
{
 public:
  //: Additive identity
  static const short zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static const short one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static const short maxval; // = 0x7fff;
  //: Return value of abs()
  typedef unsigned short abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef int double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<short const> : public vnl_numeric_traits<short> {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<unsigned short>
{
 public:
  //: Additive identity
  static const unsigned short zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static const unsigned short one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static const unsigned short maxval; // = 0xffff;
  //: Return value of abs()
  typedef unsigned short abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned int double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<unsigned short const> : public vnl_numeric_traits<unsigned short> {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<int>
{
 public:
  //: Additive identity
  static const int zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static const int one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static const int maxval; // = 0x7fffffff;
  //: Return value of abs()
  typedef unsigned int abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef long double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<int const> : public vnl_numeric_traits<int> {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<unsigned int>
{
 public:
  //: Additive identity
  static const unsigned int zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static const unsigned int one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static const unsigned int maxval; // = 0xffffffff;
  //: Return value of abs()
  typedef unsigned int abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef unsigned long double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<unsigned int const> : public vnl_numeric_traits<unsigned int> {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<long>
{
 public:
  //: Additive identity
  static const long zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static const long one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static const long maxval; // = 0x7fffffffL or 0x7fffffffffffffffL;
  //: Return value of abs()
  typedef unsigned long abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vxl_sint_64 double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<long const> : public vnl_numeric_traits<long > {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<unsigned long>
{
 public:
  //: Additive identity
  static const unsigned long zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static const unsigned long one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static const unsigned long maxval; // = 0xffffffffL or 0xffffffffffffffffL;
  //: Return value of abs()
  typedef unsigned long abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vxl_uint_64 double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<unsigned long const> : public vnl_numeric_traits<unsigned long> {};
#endif

#ifdef _WIN64
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<size_t>
{
 public:
  //: Additive identity
  static const size_t zero VCL_STATIC_CONST_INIT_INT_DECL(0);
  //: Multiplicative identity
  static const size_t one VCL_STATIC_CONST_INIT_INT_DECL(1);
  //: Maximum value which this type can assume
  static const size_t maxval; // = 0x7fffffff;
  //: Return value of abs()
  typedef size_t abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef size_t double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<size_t const> : public vnl_numeric_traits<size_t> {};
#endif

#endif   // _WIN64

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<float>
{
 public:
  //: Additive identity
  static const float zero VCL_STATIC_CONST_INIT_FLOAT_DECL(0.0F);
  //: Multiplicative identity
  static const float one VCL_STATIC_CONST_INIT_FLOAT_DECL(1.0F);
  //: Maximum value which this type can assume
  static const float maxval VCL_STATIC_CONST_INIT_FLOAT_DECL(3.40282346638528860e+38F);
  //: Return value of abs()
  typedef float abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef double double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<float const> : public vnl_numeric_traits<float> {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<double>
{
 public:
  //: Additive identity
  static const double zero VCL_STATIC_CONST_INIT_FLOAT_DECL(0.0);
  //: Multiplicative identity
  static const double one VCL_STATIC_CONST_INIT_FLOAT_DECL(1.0);
  //: Maximum value which this type can assume
  static const double maxval VCL_STATIC_CONST_INIT_FLOAT_DECL(1.7976931348623157E+308);
  //: Return value of abs()
  typedef double abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef long double double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<double const> : public vnl_numeric_traits<double> {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<long double>
{
 public:
  //: Additive identity
  static const long double zero VCL_STATIC_CONST_INIT_FLOAT_DECL(0.0);
  //: Multiplicative identity
  static const long double one VCL_STATIC_CONST_INIT_FLOAT_DECL(1.0);
  //: Maximum value which this type can assume
  static const long double maxval VCL_STATIC_CONST_INIT_FLOAT_DECL(1.7976931348623157E+308);
  //: Return value of abs()
  typedef long double abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef long double double_t; // ahem
  //: Name of type which results from multiplying this type with a double
  typedef long double real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<long double const> : public vnl_numeric_traits<long double> {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits< vcl_complex<float> >
{
 public:
  //: Additive identity
  static const vcl_complex<float> zero;
  //: Multiplicative identity
  static const vcl_complex<float> one;
  // Maximum value which this type can assume; makes no sense for this type
  //static const vcl_complex<float> maxval;

  //: Return value of abs()
  typedef float abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vcl_complex<vnl_numeric_traits<float>::double_t> double_t;
  //: Name of type which results from multiplying this type with a double
  typedef vcl_complex<float> real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<vcl_complex<float> const> : public vnl_numeric_traits<vcl_complex<float> > {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits< vcl_complex<double> >
{
 public:
  //: Additive identity
  static const vcl_complex<double> zero;
  //: Multiplicative identity
  static const vcl_complex<double> one;
  // Maximum value which this type can assume; makes no sense for this type
  //static const vcl_complex<double> maxval;

  //: Return value of abs()
  typedef double abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vcl_complex<vnl_numeric_traits<double>::double_t> double_t;
  //: Name of type which results from multiplying this type with a double
  typedef vcl_complex<double> real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<vcl_complex<double> const> : public vnl_numeric_traits<vcl_complex<double> > {};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits< vcl_complex<long double> >
{
 public:
  //: Additive identity
  static const vcl_complex<long double> zero;
  //: Multiplicative identity
  static const vcl_complex<long double> one;
  // Maximum value which this type can assume; makes no sense for this type
  //static const vcl_complex<long double> maxval;

  //: Return value of abs()
  typedef long double abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vcl_complex<vnl_numeric_traits<long double>::double_t> double_t;
  //: Name of type which results from multiplying this type with a double
  typedef vcl_complex<long double> real_t;
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<vcl_complex<long double> const> : public vnl_numeric_traits<vcl_complex<long double> > {};
#endif

#endif // vnl_numeric_traits_h_
