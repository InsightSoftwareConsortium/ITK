#ifndef vcl_deque_txx_
#define vcl_deque_txx_
// -*- c++ -*-

#include "vcl_deque.h"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "iso/vcl_deque.txx"
#elif !VCL_USE_NATIVE_STL
# include "emulation/vcl_deque.txx"
#elif defined(VCL_EGCS)
# include "egcs/vcl_deque.txx"
#elif defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# include "gcc-295/vcl_deque.txx"
#elif defined(GNU_LIBSTDCXX_V3)
# include "gcc-libstdcxx-v3/vcl_deque.txx"
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_deque.txx"
#elif defined(VCL_SGI_CC)
# include "sgi/vcl_deque.txx"
#else
# include "iso/vcl_deque.txx"
#endif

#endif
