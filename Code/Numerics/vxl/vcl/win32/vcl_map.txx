#ifndef vcl_win32_map_txx_
#define vcl_win32_map_txx_

#undef VCL_MAP_INSTANTIATE
#define VCL_MAP_INSTANTIATE(K, V, C) \
template class vcl_map<K, V, C >;

//-------------------- multimap

#undef VCL_MULTIMAP_INSTANTIATE
#define VCL_MULTIMAP_INSTANTIATE(T, Key, Comp)\
template class vcl_multimap<T, Key, Comp >;

#endif
