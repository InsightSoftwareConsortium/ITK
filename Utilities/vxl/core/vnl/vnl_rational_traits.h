// This is core/vnl/vnl_rational_traits.h
#ifndef vnl_rational_traits_h_
#define vnl_rational_traits_h_
//:
// \file
// \brief numeric traits for vnl_rational

#include <vnl/vnl_rational.h>
#include <vnl/vnl_numeric_traits.h>
#include <vcl_iosfwd.h>

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<vnl_rational>
{
 public:
  //: Additive identity
  static const vnl_rational zero; // = 0L
  //: Multiplicative identity
  static const vnl_rational one; // = 1L
  //: Maximum value which this type can assume
  static const vnl_rational maxval; // = vnl_numeric_traits<long>::maxval;
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

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<vnl_rational const> : public vnl_numeric_traits<vnl_rational>
{
};
#endif

vcl_ostream& operator<<(vcl_ostream&, vcl_complex<vnl_rational>);

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<vcl_complex<vnl_rational> >
{
 public:
  //: Additive identity
  static const vcl_complex<vnl_rational> zero; // = vcl_complex<vnl_rational>(0L,0L)
  //: Multiplicative identity
  static const vcl_complex<vnl_rational> one; // = vcl_complex<vnl_rational>(1L,0L)
  //: Maximum value which this type can assume; makes no sense for this type
  static const vcl_complex<vnl_rational> maxval;
  //: Return value of abs()
  typedef vnl_rational abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vcl_complex<vnl_rational> double_t;
  //: Name of type which results from multiplying this type with a double
  typedef vcl_complex<vnl_rational> real_t; // should be vcl_complex<double>, but that gives casting problems
};

#if !VCL_CANNOT_SPECIALIZE_CV
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<vcl_complex<vnl_rational> const> : public vnl_numeric_traits<vcl_complex<vnl_rational> >
{
};
#endif

vnl_rational vnl_math_squared_magnitude(vcl_complex<vnl_rational> const& );
vnl_rational vnl_math_abs(vcl_complex<vnl_rational> const& );

#endif // vnl_rational_traits_h_
