#ifndef vcl_deque_h_
#define vcl_deque_h_

#include "vcl_compiler.h"

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_deque.h"

//#elif defined(VCL_GCC_WITH_LIBSTDCXX_V2)
//# include <deque.h>
//# define vcl_deque deque

#else
# include "iso/vcl_deque.h"
#endif

#define VCL_DEQUE_INSTANTIATE \
extern "include vcl_deque.txx instead"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "vcl_deque.txx"
#endif

#endif // vcl_deque_h_
