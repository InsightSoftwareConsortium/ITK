#ifndef vcl_complex_txx_
#define vcl_complex_txx_
// -*- c++ -*-

#include "vcl_complex.h"

#if VCL_USE_IMPLICIT_TEMPLATES
# include "iso/vcl_complex.txx"
#elif !VCL_USE_NATIVE_COMPLEX
# include "emulation/vcl_complex.txx"
#elif defined(VCL_EGCS)
# include "egcs/vcl_complex.txx"
#elif defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# include "gcc-295/vcl_complex.txx"
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_complex.txx"
#elif defined(VCL_SGI_CC)
# include "sgi/vcl_complex.txx"
#else
# include "iso/vcl_complex.txx"
#endif

#if defined(VCL_GCC_31) && defined(sun)
# include "gcc/vcl_cmath.h" // for sqrtf(), sinf(), cosf(), sinl(), cosl()
#endif // sun

#endif
