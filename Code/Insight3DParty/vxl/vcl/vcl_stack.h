#ifndef vcl_stack_h_
#define vcl_stack_h_

#include "vcl_compiler.h"

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_stack.h"

//#elif defined(__GNUC__)
//# include <stack.h>
//# define vcl_stack stack

#else
# include "iso/vcl_stack.h"
#endif

#endif // vcl_stack_h_
