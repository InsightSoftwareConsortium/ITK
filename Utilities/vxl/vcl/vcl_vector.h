#ifndef vcl_vector_h_
#define vcl_vector_h_

#include "vcl_compiler.h"

// -------------------- emulation
#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_vector.h"

// -------------------- sunpro 5.0
#elif defined(VCL_SUNPRO_CC_50)
# include "sunpro/vcl_vector.h"

// -------------------- iso
#else
# include "iso/vcl_vector.h"
#endif

#define VCL_VECTOR_INSTANTIATE extern "you must include vcl_vector.txx first"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "vcl_vector.txx"
#endif

#endif // vcl_vector_h_
