#ifndef vcl_emulation_multimap_txx_
#define vcl_emulation_multimap_txx_

#include "vcl_multimap.h"

#undef VCL_MULTIMAP_INSTANTIATE
#define VCL_MULTIMAP_INSTANTIATE(T, Key, Comp) \
template class vcl_multimap<T, Key, Comp >

#endif // vcl_emulation_multimap_txx_
