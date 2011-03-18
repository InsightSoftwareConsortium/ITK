#ifndef vcl_gcc_295_map_txx_
#define vcl_gcc_295_map_txx_

#include <vcl_map.h>

// Both the
//
//   map<Key, T, Compare>
//
// and
//
//   multimap<Key, T, Compare>
//
// class templates use an
//
//  _Rb_tree<Key,                                  "Key"
//           pair<Key const, T>,                   "Value"
//           _Select1st<pair<Key const, T> >,      "KeyOfValue"
//           Compare,                              "Compare"
//           allocator<T> >                        "Alloc"
//
// as the underlying implementation.

// Macro which expands into the underlying tree type (avoid typedefs).
#define VCL_MAP_IMPL(Key, T, Compare) \
_Rb_tree<Key, pair<Key const,T >, _Select1st<pair<Key const,T > >, Compare, allocator<T > >


// Macro to instantiate the underlying tree and its member templates.
#define VCL_MAP_INSTANTIATE_RB_TREE(Key, T, Compare) \
template class VCL_MAP_IMPL(Key, T, Compare); \
template void VCL_MAP_IMPL(Key, T, Compare)::insert_unique(VCL_MAP_IMPL(Key, T, Compare)::iterator,\
                                                           VCL_MAP_IMPL(Key, T, Compare)::iterator)


// Macro to instantiate some methods templated over an iterator
// type. `maptype' can be either `map' or `multimap'.
#define VCL_MAP_INSTANTIATE_MT_InputIterator(maptype, Key, T, Compare, InputIterator) \
template maptype<Key, T, Compare >::maptype(InputIterator, InputIterator); \
template maptype<Key, T, Compare >::maptype(InputIterator, InputIterator, Compare const&); \
template void maptype<Key, T, Compare >::insert(InputIterator, InputIterator)


// Macro to instantiate a vcl_map.
#undef VCL_MAP_INSTANTIATE
#define VCL_MAP_INSTANTIATE(Key, T, Compare) \
template class vcl_map<Key, T, Compare >; \
VCL_MAP_INSTANTIATE_MT_InputIterator(map, Key, T, Compare, vcl_map<Key VCL_COMMA T VCL_COMMA Compare >::iterator); \
VCL_MAP_INSTANTIATE_RB_TREE(Key, T, Compare)


// Macro to instantiate a vcl_multimap.
#undef VCL_MULTIMAP_INSTANTIATE
#define VCL_MULTIMAP_INSTANTIATE(Key, T, Compare) \
template class vcl_multimap<Key, T, Compare >; \
VCL_MAP_INSTANTIATE_MT_InputIterator(multimap, Key, T, Compare,\
                                     vcl_multimap<Key VCL_COMMA T VCL_COMMA Compare >::iterator)


#endif
