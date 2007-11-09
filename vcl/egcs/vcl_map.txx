#ifndef vcl_egcs_map_txx_
#define vcl_egcs_map_txx_

#include <vcl_map.h>

// Macro to instantiate the underlying rb_tree and its member templates.
//template class rb_tree<Key,pair<Key const,T >,select1st<pair<Key const,T > >,Comp,__default_alloc_template<true,0> >
#define VCL_MAP_INSTANTIATE_RB_TREE_tagged(tag, Key, T, Comp) \
template class rb_tree<Key,pair<Key const,T >,select1st<pair<Key const,T > >,Comp,__default_alloc_template<true,0> >;\
typedef        rb_tree<Key,pair<Key const,T >,select1st<pair<Key const,T > >,Comp,__default_alloc_template<true,0> > cont##tag;\
template void cont##tag::insert_unique(cont##tag::iterator, cont##tag::iterator)
#define VCL_MAP_INSTANTIATE_RB_TREE_passthrough(tag, Key, T, Comp) \
VCL_MAP_INSTANTIATE_RB_TREE_tagged(tag, Key, T, Comp)
#define VCL_MAP_INSTANTIATE_RB_TREE(Key, T, Comp) \
VCL_MAP_INSTANTIATE_RB_TREE_passthrough(__LINE__, Key, T, Comp)

// Macro to instantiate something.
#define VCL_MAP_INSTANTIATE_MT_InputIterator(maptype, Key, T, Comp, InputIterator) \
template maptype<Key, T, Comp >::maptype(InputIterator, InputIterator);\
template maptype<Key, T, Comp >::maptype(InputIterator first, InputIterator last, Comp const&);\
template void maptype<Key, T, Comp >::insert(InputIterator first, InputIterator last)

// Macro to instantiate vcl_map<Key, T, Comp>
#undef VCL_MAP_INSTANTIATE
#define VCL_MAP_INSTANTIATE(Key, T, Comp) \
template class vcl_map<Key, T, Comp >; \
VCL_MAP_INSTANTIATE_MT_InputIterator(map, Key, T, Comp, vcl_map<Key VCL_COMMA T VCL_COMMA Comp >::iterator); \
VCL_MAP_INSTANTIATE_RB_TREE(Key, T, Comp)

// Macro to instantiate vcl_multimap<Key, T, Comp>
#undef VCL_MULTIMAP_INSTANTIATE
#define VCL_MULTIMAP_INSTANTIATE(Key, T, Comp) \
template class vcl_multimap<Key, T, Comp >

#endif // vcl_egcs_map_txx_
