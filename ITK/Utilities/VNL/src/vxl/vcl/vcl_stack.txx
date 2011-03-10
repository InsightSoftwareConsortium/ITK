#ifndef vcl_stack_txx_
#define vcl_stack_txx_
// -*- c++ -*-

#include "vcl_stack.h"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "iso/vcl_stack.txx"
#else // only need "template class vcl_stack<T>", so include any one:
# include "sunpro/vcl_stack.txx"
#endif

#endif
