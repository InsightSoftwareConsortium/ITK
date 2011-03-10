// This is core/vnl/vnl_bignum_traits.cxx
#include "vnl_bignum_traits.h"
//:
// \file
// \author Peter Vanroose
// \date   6 September 2002
//
//-----------------------------------------------------------------------------

const vnl_bignum vnl_numeric_traits<vnl_bignum>::zero = vnl_bignum(0L);
const vnl_bignum vnl_numeric_traits<vnl_bignum>::one = vnl_bignum(1L);
const vnl_bignum vnl_numeric_traits<vnl_bignum>::maxval = vnl_bignum("+Inf");
