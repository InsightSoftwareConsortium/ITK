#ifndef vcl_queue_h_
#define vcl_queue_h_

#include "vcl_compiler.h"

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_stack.h"

#elif defined(VCL_GCC_which_one_question_mark)
// egcs and 2.95 have <queue> -- fsm
# include <stack.h>
# define vcl_queue queue

#else
# include "iso/vcl_queue.h"
#endif

#define VCL_QUEUE_INSTANTIATE(T) extern "you must #include vcl_queue.txx"
#define VCL_PRIORITY_QUEUE_INSTANTIATE(T) extern "you must #include vcl_queue.txx"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "vcl_queue.txx"
#endif

#endif // vcl_queue_h_
