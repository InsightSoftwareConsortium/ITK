#ifndef vcl_set_h_
#define vcl_set_h_

#include "vcl_compiler.h"

// vcl_less<> is a default argument to vcl_set<> and vcl_multiset<>
// so we need this for compilers where vcl_less is a macro.
#include "vcl_functional.h"

// -------------------- emulation
#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_set.h"
# include "emulation/vcl_multiset.h"

// -------------------- iso
#else
# include "iso/vcl_set.h"
#endif

#define VCL_SET_INSTANTIATE extern "you must include vcl_set.txx first"
#define VCL_MULTISET_INSTANTIATE extern "you must include vcl_set.txx first"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "vcl_set.txx"
#endif

#endif // vcl_set_h_
