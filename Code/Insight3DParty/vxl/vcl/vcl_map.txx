#ifndef vcl_map_txx_
#define vcl_map_txx_

#include "vcl_map.h"

// It's vcl_map<Key, T, Compare = less<Key>, Allocator = allocator<pair<Key const, T> > >
// The key_type is Key
// The value_type is pair<Key const, T>

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_map.txx"
#elif defined(VCL_EGCS)
# include "egcs/vcl_map.txx"
#elif defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# include "gcc-295/vcl_map.txx"
#elif defined(VCL_GCC_295) && defined(GNU_LIBSTDCXX_V3)
# include "gcc-libstdcxx-v3/vcl_map.txx"
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_map.txx"
#elif defined(VCL_SGI_CC)
# include "sgi/vcl_map.txx"
#elif defined(_MSC_VER)
# include "win32/vcl_map.txx"
#else
# include "iso/vcl_map.txx"
#endif

#endif
