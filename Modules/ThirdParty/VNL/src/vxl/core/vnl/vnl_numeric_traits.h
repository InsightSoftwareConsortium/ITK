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
#include <limits>
#include <type_traits>
#include <vxl_config.h> // for type vxl_uint_64
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include "vnl/vnl_export.h"

// this is an empty class template.
// only the specializations make sense.
#if !defined(_MSC_VER) || (_MSC_VER >= 1800)
template <class T>
class VNL_EXPORT vnl_numeric_traits;
#else
// However, *some* compilers require the template to be defined
// under some circumstances...
// Since the non-specialized template doesn't make any sense, make
// sure that any types "accidently" derived from it will cause
// compiler errors.
class VNL_EXPORT vnl_numeric_traits_not_a_valid_type{};
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
  using abs_t = vnl_numeric_traits_not_a_valid_type;

  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = vnl_numeric_traits_not_a_valid_type;

  //: Name of type which results from multiplying this type with a double
  using real_t = vnl_numeric_traits_not_a_valid_type;

  //: Name of type which results from using a unary operator-()
  using signed_t = vnl_numeric_traits_not_a_valid_type;
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
  using abs_t = unsigned int;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = unsigned int;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = bool;
  //: Name of type which results from using a unary operator-()
  using signed_t = self;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const bool> : public vnl_numeric_traits<bool>
{};
#endif

template <>
class VNL_EXPORT vnl_numeric_traits<char>
{
public:
  //: Additive identity
  static constexpr char zero = 0;
  //: Multiplicative identity
  static constexpr char one = 1;
  //: Maximum value which this type can assume.
  //  Char's signedness is implementation-defined; std::numeric_limits is authoritative.
  static constexpr char maxval = std::numeric_limits<char>::max();
  //: Return value of abs()
  using abs_t = unsigned char;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = short;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = char;
  //: Name of type which results from using a unary operator-()
  using signed_t = std::make_signed<self>::type;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const char> : public vnl_numeric_traits<char>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<unsigned char>
{
public:
  //: Additive identity
  static constexpr unsigned char zero = 0;
  //: Multiplicative identity
  static constexpr unsigned char one = 1;
  //: Maximum value which this type can assume
  static constexpr unsigned char maxval = std::numeric_limits<unsigned char>::max();
  //: Return value of abs()
  using abs_t = unsigned char;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = unsigned short;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = unsigned char;
  //: Name of type which results from using a unary operator-()
  using signed_t = std::make_signed<self>::type;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const unsigned char> : public vnl_numeric_traits<unsigned char>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<signed char>
{
public:
  //: Additive identity
  static constexpr signed char zero = 0;
  //: Multiplicative identity
  static constexpr signed char one = 1;
  //: Maximum value which this type can assume
  static constexpr signed char maxval = std::numeric_limits<signed char>::max();
  //: Return value of abs()
  using abs_t = unsigned char;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = signed short;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = signed char;
  //: Name of type which results from using a unary operator-()
  using signed_t = std::make_signed<self>::type;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const signed char> : public vnl_numeric_traits<signed char>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<short>
{
public:
  //: Additive identity
  static constexpr short zero = 0;
  //: Multiplicative identity
  static constexpr short one = 1;
  //: Maximum value which this type can assume
  static constexpr short maxval = std::numeric_limits<short>::max();
  //: Return value of abs()
  using abs_t = unsigned short;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = int;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = short;
  //: Name of type which results from using a unary operator-()
  using signed_t = std::make_signed<self>::type;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const short> : public vnl_numeric_traits<short>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<unsigned short>
{
public:
  //: Additive identity
  static constexpr unsigned short zero = 0;
  //: Multiplicative identity
  static constexpr unsigned short one = 1;
  //: Maximum value which this type can assume
  static constexpr unsigned short maxval = std::numeric_limits<unsigned short>::max();
  //: Return value of abs()
  using abs_t = unsigned short;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = unsigned int;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = unsigned short;
  //: Name of type which results from using a unary operator-()
  using signed_t = std::make_signed<self>::type;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const unsigned short> : public vnl_numeric_traits<unsigned short>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<int>
{
public:
  //: Additive identity
  static constexpr int zero = 0;
  //: Multiplicative identity
  static constexpr int one = 1;
  //: Maximum value which this type can assume
  static constexpr int maxval = std::numeric_limits<int>::max();
  //: Return value of abs()
  using abs_t = unsigned int;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = long;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = int;
  //: Name of type which results from using a unary operator-()
  using signed_t = std::make_signed<self>::type;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const int> : public vnl_numeric_traits<int>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<unsigned int>
{
public:
  //: Additive identity
  static constexpr unsigned int zero = 0;
  //: Multiplicative identity
  static constexpr unsigned int one = 1;
  //: Maximum value which this type can assume
  static constexpr unsigned int maxval = std::numeric_limits<unsigned int>::max();
  //: Return value of abs()
  using abs_t = unsigned int;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = unsigned long;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = unsigned int;
  //: Name of type which results from using a unary operator-()
  using signed_t = std::make_signed<self>::type;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const unsigned int> : public vnl_numeric_traits<unsigned int>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<long>
{
public:
  //: Additive identity
  static constexpr long zero = 0;
  //: Multiplicative identity
  static constexpr long one = 1;
  //: Maximum value which this type can assume
  static constexpr long maxval = std::numeric_limits<long>::max();
  //: Return value of abs()
  using abs_t = unsigned long;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = vxl_sint_64;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = long;
  //: Name of type which results from using a unary operator-()
  using signed_t = std::make_signed<self>::type;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const long> : public vnl_numeric_traits<long>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<unsigned long>
{
public:
  //: Additive identity
  static constexpr unsigned long zero = 0;
  //: Multiplicative identity
  static constexpr unsigned long one = 1;
  //: Maximum value which this type can assume
  static constexpr unsigned long maxval = std::numeric_limits<unsigned long>::max();
  //: Return value of abs()
  using abs_t = unsigned long;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = vxl_uint_64;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = unsigned long;
  //: Name of type which results from using a unary operator-()
  using signed_t = std::make_signed<self>::type;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const unsigned long> : public vnl_numeric_traits<unsigned long>
{};


// long long - target type will have width of at least 64 bits. (since C++11)
template <>
class VNL_EXPORT vnl_numeric_traits<long long>
{
public:
  //: Additive identity
  static constexpr long long zero = 0;
  //: Multiplicative identity
  static constexpr long long one = 1;
  //: Maximum value which this type can assume
  static constexpr long long maxval = std::numeric_limits<long long>::max();
  //: Return value of abs()
  using abs_t = unsigned long long;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = long long;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = long long;
  //: Name of type which results from using a unary operator-()
  using signed_t = std::make_signed<self>::type;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const long long> : public vnl_numeric_traits<long long>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<unsigned long long>
{
public:
  //: Additive identity
  static constexpr unsigned long long zero = 0;
  //: Multiplicative identity
  static constexpr unsigned long long one = 1;
  //: Maximum value which this type can assume
  static constexpr unsigned long long maxval = std::numeric_limits<unsigned long long>::max();
  //: Return value of abs()
  using abs_t = unsigned long long;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = unsigned long long;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = unsigned long long;
  //: Name of type which results from using a unary operator-()
  using signed_t = std::make_signed<self>::type;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const unsigned long long> : public vnl_numeric_traits<unsigned long long>
{};


template <>
class VNL_EXPORT vnl_numeric_traits<float>
{
public:
  //: Additive identity
  static constexpr float zero = 0.0F;
  //: Multiplicative identity
  static constexpr float one = 1.0F;
  //: Maximum value which this type can assume
  static constexpr float maxval = std::numeric_limits<float>::max();
  //: Return value of abs()
  using abs_t = float;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = double;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = float;
  //: Name of type which results from using a unary operator-()
  using signed_t = self;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const float> : public vnl_numeric_traits<float>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<double>
{
public:
  //: Additive identity
  static constexpr double zero = 0.0;
  //: Multiplicative identity
  static constexpr double one = 1.0;
  //: Maximum value which this type can assume
  static constexpr double maxval = std::numeric_limits<double>::max();
  //: Return value of abs()
  using abs_t = double;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = long double;
  //: Name of type which results from multiplying this type with a double
  using real_t = double;
  //: Name of this type
  using self = double;
  //: Name of type which results from using a unary operator-()
  using signed_t = self;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const double> : public vnl_numeric_traits<double>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<long double>
{
public:
  //: Additive identity
  static constexpr long double zero = 0.0;
  //: Multiplicative identity
  static constexpr long double one = 1.0;
  //: Maximum value which this type can assume
  static constexpr long double maxval = std::numeric_limits<long double>::max();
  //: Return value of abs()
  using abs_t = long double;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = long double; // ahem
  //: Name of type which results from multiplying this type with a double
  using real_t = long double;
  //: Name of this type
  using self = long double;
  //: Name of type which results from using a unary operator-()
  using signed_t = self;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const long double> : public vnl_numeric_traits<long double>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<std::complex<float>>
{
public:
  //: Additive identity
  static const std::complex<float> zero;
  //: Multiplicative identity
  static const std::complex<float> one;
  // Maximum value which this type can assume; makes no sense for this type
  // static const std::complex<float> maxval;

  //: Return value of abs()
  using abs_t = float;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = std::complex<vnl_numeric_traits<float>::double_t>;
  //: Name of type which results from multiplying this type with a double
  using real_t = std::complex<float>;
  //: Name of this type
  using self = std::complex<float>;
  //: Name of type which results from using a unary operator-()
  using signed_t = self;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const std::complex<float>> : public vnl_numeric_traits<std::complex<float>>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<std::complex<double>>
{
public:
  //: Additive identity
  static const std::complex<double> zero;
  //: Multiplicative identity
  static const std::complex<double> one;
  // Maximum value which this type can assume; makes no sense for this type
  // static const std::complex<double> maxval;

  //: Return value of abs()
  using abs_t = double;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = std::complex<vnl_numeric_traits<double>::double_t>;
  //: Name of type which results from multiplying this type with a double
  using real_t = std::complex<double>;
  //: Name of this type
  using self = std::complex<double>;
  //: Name of type which results from using a unary operator-()
  using signed_t = self;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const std::complex<double>> : public vnl_numeric_traits<std::complex<double>>
{};

template <>
class VNL_EXPORT vnl_numeric_traits<std::complex<long double>>
{
public:
  //: Additive identity
  static const std::complex<long double> zero;
  //: Multiplicative identity
  static const std::complex<long double> one;
  // Maximum value which this type can assume; makes no sense for this type
  // static const std::complex<long double> maxval;

  //: Return value of abs()
  using abs_t = long double;
  //: Name of a type twice as long as this one for accumulators and products.
  using double_t = std::complex<vnl_numeric_traits<long double>::double_t>;
  //: Name of type which results from multiplying this type with a double
  using real_t = std::complex<long double>;
  //: Name of this type
  using self = std::complex<long double>;
  //: Name of type which results from using a unary operator-()
  using signed_t = self;
};

template <>
class VNL_EXPORT vnl_numeric_traits<const std::complex<long double>>
  : public vnl_numeric_traits<std::complex<long double>>
{};

#endif // vnl_numeric_traits_h_
