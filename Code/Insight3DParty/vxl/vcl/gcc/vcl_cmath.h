#ifndef vcl_gcc_cmath_h_
#define vcl_gcc_cmath_h_

#include "../iso/vcl_cmath.h"

// 1.5 fix system header.
#if defined (linux) && defined (__OPTIMIZE__)
// * avoid infinite recursion when calling vnl_math::isfinite().
// * avoid symbol in object file being called vnl_math::_isinf.
# undef isinf  
// * avoid that vnl_math::isnan is redefined in <math.h>.
# undef isnan  
#endif

#endif // vcl_cmath_h_
