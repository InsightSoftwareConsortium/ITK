#ifndef vcl_queue_txx_
#define vcl_queue_txx_
// -*- c++ -*-

#include "vcl_queue.h"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "iso/vcl_queue.txx"
#elif defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# include "gcc-295/vcl_queue.txx"
#else
# include "iso/vcl_queue.txx"
#endif

#endif
