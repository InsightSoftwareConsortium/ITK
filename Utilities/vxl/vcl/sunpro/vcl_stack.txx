#ifndef vcl_sunpro_stack_txx_
#define vcl_sunpro_stack_txx_
/*
  fsm
*/

#include <vcl_stack.h>

#undef VCL_STACK_INSTANTIATE
#define VCL_STACK_INSTANTIATE(T) \
template class vcl_stack<T >

#endif // vcl_sunpro_stack_txx_
