#ifndef vcl_vector_h_
#define vcl_vector_h_

#include "vcl_compiler.h"

// -------------------- emulation
#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_vector.h"

// -------------------- sunpro 5.0
#elif defined(VCL_SUNPRO_CC_50)
# include "sunpro/vcl_vector.h"
# define vcl_vector vcl_vector_sunpro_50

// -------------------- iso
#else
# include "iso/vcl_vector.h"
#endif

#endif // vcl_vector_h_
