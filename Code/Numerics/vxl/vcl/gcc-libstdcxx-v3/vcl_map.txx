#ifndef vcl_gcc_libstdcxx_v3_map_txx_
#define vcl_gcc_libstdcxx_v3_map_txx_

#include <vcl_map.h>

// Macro to instantiate a vcl_map.
#undef VCL_MAP_INSTANTIATE
#define VCL_MAP_INSTANTIATE(Key, T, Comp) \
template class vcl_map<Key, T, Comp >; \
/* member template : */ \
template void vcl_map<Key, T, Comp >::insert(vcl_map<Key, T, Comp >::iterator, vcl_map<Key, T, Comp >::iterator)

// Macro to instantiate a vcl_multimap.
#undef VCL_MULTIMAP_INSTANTIATE
#define VCL_MULTIMAP_INSTANTIATE(Key, T, Comp) \
template class vcl_multimap<Key, T, Comp >

#endif
