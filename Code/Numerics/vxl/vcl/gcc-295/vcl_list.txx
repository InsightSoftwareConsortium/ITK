#ifndef vcl_gcc295_list_txx_
#define vcl_gcc295_list_txx_

#include <vcl_list.h>

#define VCL_LIST_INSTANTIATE_MT_InputIterator(T, InputIterator) \
template list<T >::list(InputIterator first, InputIterator last); \
template void list<T >::insert(list<T >::iterator, InputIterator, InputIterator); \
template void list<T >::_M_insert_dispatch(list<T >::iterator, InputIterator, InputIterator, __false_type)

//PVr removed(for gcc 2.95): template void list<T >::range_initialize(InputIterator first, InputIterator last);

#undef VCL_LIST_INSTANTIATE
#define VCL_LIST_INSTANTIATE(T) \
template class list<T >;\
template void _List_base<T,allocator<T > >::clear();\
VCL_LIST_INSTANTIATE_MT_InputIterator(T, list<T >::iterator); \
VCL_LIST_INSTANTIATE_MT_InputIterator(T, list<T >::const_iterator)

#endif
