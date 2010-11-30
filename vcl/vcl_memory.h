#ifndef vcl_memory_h_
#define vcl_memory_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_defalloc.h"
# include "emulation/vcl_iterator.h"
# include "emulation/vcl_algorithm.h"
# define vcl_auto_ptr auto_ptr // fixme

# if VCL_INCLUDE_CXX_0X
// This is where C++0x emulation goes when available
****Error: shared_ptr emulation not available****
// #include "emulation/vcl_shared_ptr.h"
# endif // VCL_INCLUDE_CXX_0X

#elif defined(VCL_VC60)
# include "win32-vc60/vcl_memory.h"
#elif defined(VCL_GCC_295)
# include "gcc-295/vcl_memory.h"
#elif defined(VCL_BORLAND_55)
# include "borland55/vcl_memory.h"
#else

# include "iso/vcl_memory.h"

# if VCL_INCLUDE_CXX_0X
#  if VCL_MEMORY_HAS_SHARED_PTR
#   include "iso/vcl_memory_tr1.h"
#  elif VCL_TR1_MEMORY_HAS_SHARED_PTR
#   include "tr1/vcl_memory.h"
#  else
// This is where C++0x emulation goes when available
****Error: shared_ptr emulation not available****
// #include "emulation/vcl_shared_ptr.h"
#  endif // VCL_MEMORY_HAS_SHARED_PTR
# endif // VCL_INCLUDE_CXX_0X

#endif // !VCL_USE_NATIVE_STL

#endif // vcl_memory_h_
