#ifndef vcl_algorithm_txx_
#define vcl_algorithm_txx_
// -*- c++ -*-

#include "vcl_algorithm.h"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "iso/vcl_algorithm.txx"
#elif !VCL_USE_NATIVE_STL
# include "emulation/vcl_algorithm.txx"
#elif defined(VCL_EGCS)
# include "egcs/vcl_algorithm.txx"
#elif defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# include "gcc-295/vcl_algorithm.txx"
#elif defined(GNU_LIBSTDCXX_V3)
# include "gcc-libstdcxx-v3/vcl_algorithm.txx"
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_algorithm.txx"
#elif defined(VCL_SGI_CC)
# include "sgi/vcl_algorithm.txx"
#else
# include "iso/vcl_algorithm.txx"
#endif

#endif
