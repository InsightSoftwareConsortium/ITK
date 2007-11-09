#ifndef vcl_functional_txx_
#define vcl_functional_txx_
// -*- c++ -*-

#include "vcl_functional.h"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "iso/vcl_functional.txx"
#elif !VCL_USE_NATIVE_STL
# include "emulation/vcl_functional.txx"
#elif defined(VCL_EGCS)
# include "egcs/vcl_functional.txx"
#elif defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# include "gcc-295/vcl_functional.txx"
#elif defined(GNU_LIBSTDCXX_V3)
# include "gcc-libstdcxx-v3/vcl_functional.txx"
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_functional.txx"
#elif defined(VCL_SGI_CC)
# include "sgi/vcl_functional.txx"
#else
# include "iso/vcl_functional.txx"
#endif

#endif
