#ifndef vcl_cstdarg_h_
#define vcl_cstdarg_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CSTDARG
# include <stdarg.h>
# define vcl_va_list va_list
#elif defined(VCL_VC60)
# include "win32-vc60/vcl_cstdarg.h"
#else
# include "iso/vcl_cstdarg.h"
#endif

#endif // vcl_cstdarg_h_
