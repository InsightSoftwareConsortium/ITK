#ifndef vcl_string_txx_
#define vcl_string_txx_
// -*- c++ -*-

#include "vcl_string.h"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "iso/vcl_string.txx"
#elif defined(VCL_EGCS)
# include "egcs/vcl_string.txx"
#elif defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# include "gcc-295/vcl_string.txx"
#elif defined(GNU_LIBSTDCXX_V3)
# include "gcc-libstdcxx-v3/vcl_string.txx"
#elif defined(VCL_SUNPRO_CC)
# include "iso/vcl_string.txx"
#elif defined(VCL_SGI_CC)
# include "sgi/vcl_string.txx"
#else
# include "iso/vcl_string.txx"
#endif

#endif
