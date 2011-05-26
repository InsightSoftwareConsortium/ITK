#ifndef vcl_gcc_295_set_txx_
#define vcl_gcc_295_set_txx_

#include <vcl_set.h>
#include <vcl_cstddef.h> // for vcl_size_t

// Both
//   set<Key, Compare>
// and
//   multiset<Key, Compare>
// use
//   _Rb_tree<Key,
//            Key,
//            _Identity<Key>,
//            Compare,
//            allocator<Key> >
// as the underlying implementation.

#define VCL_SET_IMPL(Key, Compare) \
_Rb_tree<Key, Key, _Identity<Key >, Compare, allocator<Key > >

#undef VCL_SET_INSTANTIATE
#define VCL_SET_INSTANTIATE(T, Compare) \
template class vcl_set<T, Compare >; \
template void distance (vcl_set<T, Compare >::iterator, vcl_set<T, Compare >::iterator, unsigned &); \
template pair<VCL_SET_IMPL(T, Compare)::iterator, bool > VCL_SET_IMPL(T, Compare)::insert_unique(T const&); \
template VCL_SET_IMPL(T, Compare)::const_iterator VCL_SET_IMPL(T, Compare)::find(T const&) const; \
template void VCL_SET_IMPL(T, Compare)::_M_erase(_Rb_tree_node*); \
template vcl_size_t VCL_SET_IMPL(T, Compare)::erase(T const&)

#endif // vcl_gcc_295_set_txx_
