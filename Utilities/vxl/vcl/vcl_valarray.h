#ifndef vcl_valarray_h_
#define vcl_valarray_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if defined(VCL_GCC)
# include <valarray> // 2.95
# define vcl_valarray valarray

#elif defined(VCL_VC60)
# include "win32-vc60/vcl_valarray.h"

#elif defined(VCL_VC70)
# include "win32-vc70/vcl_valarray.h"

#else
# include "iso/vcl_valarray.h"
#endif

#endif // vcl_valarray_h_
