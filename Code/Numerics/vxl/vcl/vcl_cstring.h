#ifndef vcl_cstring_h_
#define vcl_cstring_h_

#include "vcl_compiler.h"

#if defined(VCL_SGI_CC_720)
# include <string.h>
# define vcl_generic_cstring_STD /* */
# include "generic/vcl_cstring.h"
#elif defined(VCL_VC)
# include "win32/vcl_cstring.h"
#else
# include "iso/vcl_cstring.h"
#endif

#endif // vcl_cstring_h_
