#ifndef vcl_vector_txx_
#define vcl_vector_txx_
// -*- c++ -*-

#include "vcl_vector.h"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "iso/vcl_vector.txx"
#elif !VCL_USE_NATIVE_STL
# include "emulation/vcl_vector.txx"
#elif defined(VCL_EGCS)
# include "egcs/vcl_vector.txx"
#elif defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# include "gcc-295/vcl_vector.txx"
#elif defined(GNU_LIBSTDCXX_V3)
# include "gcc-libstdcxx-v3/vcl_vector.txx"
#elif defined(VCL_SUNPRO_CC) && !defined(VCL_SUNPRO_CC_56)
# include "sunpro/vcl_vector.txx"
#elif defined(VCL_SGI_CC)
# include "sgi/vcl_vector.txx"
#else
# include "iso/vcl_vector.txx"
#endif

#endif
