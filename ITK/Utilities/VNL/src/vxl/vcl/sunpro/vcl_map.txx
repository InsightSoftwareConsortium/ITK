#ifndef vcl_sunpro_map_txx_
#define vcl_sunpro_map_txx_

#include <vcl_map.h>
#include <vcl_iterator.h>

// the business
#define VCL_MAP_INSTANTIATE_internal(tag, Key, T, Comp) \
typedef vcl_map_sunpro_50<Key, T, Comp >::base grik##tag; \
template class grik##tag; \
template class grik##tag :: __rep_type; \
template void std::__distance(grik##tag::iterator, grik##tag::iterator, unsigned &, vcl_bidirectional_iterator_tag); \
template struct vcl_map_sunpro_50<Key, T, Comp >


// the purpose of this macro is to expand out 'tag', before passing it on.
#define VCL_MAP_INSTANTIATE_internal_expandline(tag, Key, T, Comp) \
VCL_MAP_INSTANTIATE_internal(tag, Key, T, Comp)

// clients call this
#undef VCL_MAP_INSTANTIATE
#define VCL_MAP_INSTANTIATE(Key, T, Comp) \
VCL_MAP_INSTANTIATE_internal_expandline(__LINE__, Key, T, Comp)


//-------------------- multimap
#undef VCL_MULTIMAP_INSTANTIATE
#define VCL_MULTIMAP_INSTANTIATE(Key, T, Comp) \
template class vcl_multimap<Key, T, Comp >; \
template struct vcl_multimap_sunpro_50<Key, T, Comp >

#endif // vcl_sunpro_map_txx_
