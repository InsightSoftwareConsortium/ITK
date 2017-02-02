// This is core/vnl/vnl_decnum_traits.h
#ifndef vnl_decnum_traits_h_
#define vnl_decnum_traits_h_
//:
// \file
// \brief numeric traits for vnl_decnum

#include <vnl/vnl_decnum.h>
#include <vnl/vnl_numeric_traits.h>
#include "vnl/vnl_export.h"

template <>
class VNL_EXPORT vnl_numeric_traits<vnl_decnum>
{
 public:
  //: Additive identity
  static const vnl_decnum zero; // = 0L
  //: Multiplicative identity
  static const vnl_decnum one; // = 1L
  //: Maximum value which this type can assume
  static const vnl_decnum maxval; // = infinity
  //: Return value of abs()
  typedef vnl_decnum abs_t;
  //: Name of a type twice as long as this one for accumulators and products.
  typedef vnl_decnum double_t;
  //: Name of type which results from multiplying this type with a double
  typedef vnl_decnum real_t;
};

#endif // vnl_decnum_traits_h_
