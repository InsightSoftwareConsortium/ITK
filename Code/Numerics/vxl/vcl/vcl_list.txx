// -*- c++ -*-
#ifndef vcl_list_txx_
#define vcl_list_txx_

#include "vcl_list.h"

#undef VCL_LIST_INSTANTIATE

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_list.txx"
#elif defined(VCL_EGCS)
# include "egcs/vcl_list.txx"
#elif defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# include "gcc-295/vcl_list.txx"
#elif defined(VCL_GCC_295) && defined(GNU_LIBSTDCXX_V3)
# include "gcc-libstdcxx-v3/vcl_list.txx"
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_list.txx"
#elif defined(VCL_SGI_CC)
# include "sgi/vcl_list.txx"
#elif defined(VCL_WIN32)
# include "win32/vcl_list.txx"
#else
# include "iso/vcl_list.txx"
#endif

#endif
