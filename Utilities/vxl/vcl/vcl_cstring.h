#ifndef vcl_cstring_h_
#define vcl_cstring_h_

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CSTRING
# include <string.h>
# define vcl_generic_cstring_STD /* */
# include "generic/vcl_cstring.h"
#elif VCL_VC60
# include <cstring>
# define vcl_generic_cstring_STD /* */
# include "generic/vcl_cstring.h"
#elif defined(VCL_METRO_WERKS)
# include <cstring>
# define vcl_generic_cstring_STD /* */
# include "generic/vcl_cstring.h"
#else
# include "iso/vcl_cstring.h"
#endif

#endif // vcl_cstring_h_
