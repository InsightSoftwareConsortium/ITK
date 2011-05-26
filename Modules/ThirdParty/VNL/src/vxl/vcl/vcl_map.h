#ifndef vcl_map_h_
#define vcl_map_h_

#include "vcl_compiler.h"

// vcl_less<> is a default argument to vcl_map<> and vcl_multimap<>
// so we need this for compilers where vcl_less is a macro.
#include "vcl_functional.h"

// -------------------- emulation
#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_map.h"
# include "emulation/vcl_multimap.h"

// -------------------- sunpro 5.0
#elif defined(VCL_SUNPRO_CC_50)
# include "sunpro/vcl_map.h"

// -------------------- iso
#else
# include "iso/vcl_map.h"
#endif

#define VCL_MAP_INSTANTIATE(T1, T2, L) extern "please include vcl_map.txx"
#define VCL_MULTIMAP_INSTANTIATE(T1, T2, L) extern "please include vcl_multimap.txx"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "vcl_map.txx"
#endif

#endif // vcl_map_h_
