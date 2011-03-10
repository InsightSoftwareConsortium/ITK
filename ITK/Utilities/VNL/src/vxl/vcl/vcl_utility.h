#ifndef vcl_utility_h_
#define vcl_utility_h_

#include "vcl_compiler.h"

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_utility.h"
# define vcl_make_pair make_pair

#else // e.g. VCL_SUNPRO_CC or VCL_VC60
# include "iso/vcl_utility.h"
#endif

#define VCL_PAIR_INSTANTIATE extern "you must include vcl_utility.txx first"
#define VCL_PAIR_const_INSTANTIATE extern "you must include vcl_utility.txx first"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "vcl_utility.txx"
#endif

#endif // vcl_utility_h_
