// This is core/vnl/vnl_bignum_traits.h
#ifndef vnl_bignum_traits_h_
#define vnl_bignum_traits_h_
//:
// \file
// \brief numeric traits for vnl_bignum

#include <vnl/vnl_bignum.h>
#include <vnl/vnl_numeric_traits.h>

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<vnl_bignum>
{
 public:
  //: Additive identity
  static const vnl_bignum zero; // = 0L
  //: Multiplicative identity
  static const vnl_bignum one; // = 1L
  //: Maximum value which this type can assume
  static const vnl_bignum maxval; // = infinity
  //: Return value of abs()
  typedef vnl_bignum abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vnl_bignum double_t;
  //: Name of type which results from multiplying this type with a double
  typedef double real_t;
};

#endif // vnl_bignum_traits_h_
