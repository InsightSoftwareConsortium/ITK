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

#define VCL_STACK_INSTANTIATE extern "you must include vcl_stack.txx first"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "vcl_stack.txx"
#endif

#endif // vcl_stack_h_
