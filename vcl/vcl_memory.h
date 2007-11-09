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
#elif defined(VCL_VC60)
# include "win32-vc60/vcl_memory.h"
#elif defined(VCL_GCC_295)
# include "gcc-295/vcl_memory.h"
#elif defined(VCL_BORLAND_55)
# include "borland55/vcl_memory.h"
#else
# include "iso/vcl_memory.h"
#endif

#endif // vcl_memory_h_
