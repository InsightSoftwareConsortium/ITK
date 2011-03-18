#ifndef vcl_egcs_set_txx_
#define vcl_egcs_set_txx_

#include <vcl_set.h>

#define VCL_SET_INSTANTIATE_ITERATOR(InputIterator, Distance) \
template void distance (InputIterator , InputIterator , Distance& )

#undef VCL_SET_INSTANTIATE
#define VCL_SET_INSTANTIATE(T, Comp) \
template class vcl_set<T, Comp >; \
template class rb_tree<T, T, vcl_identity<T >, Comp >; \
VCL_SET_INSTANTIATE_ITERATOR(vcl_set<T VCL_COMMA Comp >::iterator, unsigned)

#endif // vcl_egcs_set_txx_
