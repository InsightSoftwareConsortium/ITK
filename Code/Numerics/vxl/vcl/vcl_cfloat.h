#ifndef vcl_cfloat_h_
#define vcl_cfloat_h_
/*
  Peter.Vanroose@esat.kuleuven.ac.be
*/

/* This should define C-style numeric floating point macros */

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CFLOAT
# include <float.h>
#else
# include "iso/vcl_cfloat.h"
#endif

#endif // vcl_cfloat_h_
