// -*- c++ -*-
#include "vcl_iterator.h"

#include "vcl_compiler.h"

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_iterator.txx"
#elif defined(VCL_EGCS)
# include "egcs/vcl_iterator.txx"
#elif defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# include "gcc-295/vcl_iterator.txx"
#elif defined(VCL_GCC_295) && defined(GNU_LIBSTDCXX_V3)
# include "gcc-libstdcxx-v3/vcl_iterator.txx"
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_iterator.txx"
#elif defined(VCL_SGI_CC)
# include "sgi/vcl_iterator.txx"
#elif defined(VCL_WIN32)
# include "win32/vcl_iterator.txx"
#else
# include "iso/vcl_iterator.txx"
#endif
