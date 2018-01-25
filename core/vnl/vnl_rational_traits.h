// This is core/vnl/vnl_rational_traits.h
#ifndef vnl_rational_traits_h_
#define vnl_rational_traits_h_
//:
// \file
// \brief numeric traits for vnl_rational

#include <iosfwd>
#include <vnl/vnl_rational.h>
#include <vnl/vnl_numeric_traits.h>
#include <vcl_compiler.h>
#include <vnl/vnl_export.h>

template <>
class vnl_numeric_traits<vnl_rational>
{
 public:
  //: Additive identity
  static VNL_EXPORT const vnl_rational zero; // = 0L
  //: Multiplicative identity
  static VNL_EXPORT const vnl_rational one; // = 1L
  //: Maximum value which this type can assume
  static VNL_EXPORT const vnl_rational maxval; // = vnl_numeric_traits<long>::maxval;
  //: Return value of abs()
  typedef vnl_rational abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vnl_rational double_t;
  //: Name of type which results from multiplying this type with a double.
  //  Note that this requires an explicit cast from double to vnl_rational.
  //  This must be a built-in type: do not set this to vnl_rational, since
  //  that would require std::sqrt(vnl_rational) etc., which is not allowed.
  typedef double real_t;
};

template <>
class vnl_numeric_traits<vnl_rational const> : public vnl_numeric_traits<vnl_rational>
{
};

std::ostream& operator<<(std::ostream&, std::complex<vnl_rational>);

template <>
class vnl_numeric_traits<std::complex<vnl_rational> >
{
 public:
  //: Additive identity
  static const std::complex<vnl_rational> zero; // = std::complex<vnl_rational>(0L,0L)
  //: Multiplicative identity
  static const std::complex<vnl_rational> one; // = std::complex<vnl_rational>(1L,0L)
  //: Maximum value which this type can assume; makes no sense for this type
  static const std::complex<vnl_rational> maxval;
  //: Return value of abs()
  typedef vnl_rational abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef std::complex<vnl_rational> double_t;
  //: Name of type which results from multiplying this type with a double
  typedef std::complex<vnl_rational> real_t; // should be std::complex<double>, but that gives casting problems
};

template <>
class vnl_numeric_traits<std::complex<vnl_rational> const> : public vnl_numeric_traits<std::complex<vnl_rational> >
{
};

namespace vnl_math
{
  vnl_rational squared_magnitude(std::complex<vnl_rational> const& );
  vnl_rational abs(std::complex<vnl_rational> const& );
}

#endif // vnl_rational_traits_h_
