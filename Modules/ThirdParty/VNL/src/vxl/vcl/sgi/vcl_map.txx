#ifndef vcl_sgi_map_txx_
#define vcl_sgi_map_txx_

#undef VCL_MAP_INSTANTIATE
#if VCL_USE_NATIVE_STL
#define VCL_MAP_INSTANTIATE(K, V, C) \
template class vcl_map<K, V, C >; \
template class std::rb_tree<K,std::pair<const K,V >,std::select1st<std::pair<const K,V > >,C,std::__default_alloc_template<true,0> >
#else
#define VCL_MAP_INSTANTIATE(K, V, C) \
template class vcl_map<K, V, C >
#endif

//-------------------- multimap

#undef VCL_MULTIMAP_INSTANTIATE
#define VCL_MULTIMAP_INSTANTIATE(T, Key, Comp) \
template class vcl_multimap<T, Key, Comp >

#endif // vcl_sgi_map_txx_
