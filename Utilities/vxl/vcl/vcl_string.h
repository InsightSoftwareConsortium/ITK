//-*- c++ -*-------------------------------------------------------------------
#ifndef vcl_string_h_
#define vcl_string_h_

#include "vcl_compiler.h"

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_string.h"

#elif defined(VCL_GCC) && !defined(GNU_LIBSTDCXX_V3)
# include "iso/vcl_string.h"
# undef  vcl_char_traits
# define vcl_char_traits   string_char_traits

#elif defined(VCL_VC60)
# include "win32-vc60/vcl_string.h"

#elif defined(VCL_SGI_CC_7)
# include "sgi/vcl_string.h"

#else
# include "iso/vcl_string.h"
#endif

//// who needs to know this?
//#if defined(VCL_GCC_EGCS) || defined(VCL_SUNPRO_CC)
//# define VCL_STRING_IS_TYPEDEF 1
//#endif

#define VCL_BASIC_STRING_INSTANTIATE \
extern "include vcl_string.txx instead"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "vcl_string.txx"
#endif

#endif // vcl_string_h_
