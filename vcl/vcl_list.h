#ifndef vcl_list_h_
#define vcl_list_h_

#include "vcl_compiler.h"

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_list.h"

#else
# include "iso/vcl_list.h"
#endif

#define VCL_LIST_INSTANTIATE \
extern "include vcl_list.txx instead"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "vcl_list.txx"
#endif

#endif // vcl_list_h_
