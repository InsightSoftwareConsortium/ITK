#ifndef vcl_bitset_h_
#define vcl_bitset_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if defined(VCL_GCC)
# include <bitset> // 2.95
# define vcl_bitset bitset

#elif defined(VCL_SGI_CC_7)
# include "sgi/vcl_bitset.h"

#else
# include "iso/vcl_bitset.h"
#endif

#endif // vcl_bitset_h_
