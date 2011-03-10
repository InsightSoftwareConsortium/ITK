#ifndef vcl_cfloat_h_
#define vcl_cfloat_h_
//:
// \file
// \author Peter Vanroose, esat.kuleuven.be
// This should define C-style numeric floating point macros

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CFLOAT
# include <float.h>
#elif defined(VCL_BORLAND_55)
# include "borland55/vcl_cfloat.h"
#else
# include "iso/vcl_cfloat.h"
#endif

#ifndef FLT_MAX
# ifdef MAXFLOAT
#  define FLT_MAX MAXFLOAT
# endif
#endif
#ifndef DBL_MAX
# ifdef MAXDOUBLE
#  define DBL_MAX MAXDOUBLE
# endif
#endif
#ifndef FLT_MIN
# ifdef MINFLOAT
#  define FLT_MIN MINFLOAT
# endif
#endif
#ifndef DBL_MIN
# ifdef MINDOUBLE
#  define DBL_MIN MINDOUBLE
# endif
#endif
#ifndef FLT_MAX
# include <vcl_cmath.h>
#endif
#ifndef FLT_MAX
# include <values.h>
#endif
#ifndef FLT_MAX
# include <limits.h>
#endif
#ifndef FLT_MAX
# define FLT_MAX 3.40282346638528860e38F
#endif
#ifndef DBL_MAX
# define DBL_MAX 1.7976931348623157e308
#endif
#ifndef FLT_MIN
# define FLT_MIN 1.1754943508222875e-38F
#endif
#ifndef DBL_MIN
# define DBL_MIN 2.2250738585072014e-308
#endif

// Don't define MAXFLOAT, MAXDOUBLE and relatives here. They are not
// part of the standard (see table 17, section 18.2.2/3), and may not
// be defined in cfloat. Defining them here may cause unnecessary
// redefinition warnings depending on the order that a user includes
// header files.
// If you absolutely need MAXFLOAT, look in the C header file math.h

#endif // vcl_cfloat_h_
