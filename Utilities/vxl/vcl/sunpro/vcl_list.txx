#ifndef vcl_sunpro_list_txx_
#define vcl_sunpro_list_txx_

#include <vcl_list.h>
#include <vcl_iterator.h>

#define VCL_LIST_INSTANTIATE_ITERATOR(Inp, Fwd, Diff) \
template Fwd std::copy(Inp, Inp, Fwd);\
template Fwd std::copy_backward(Inp, Inp, Fwd);\
template void std::advance(Fwd&, Diff);\
template void std::__advance(Fwd&, int, vcl_bidirectional_iterator_tag)

#undef VCL_LIST_INSTANTIATE
#define VCL_LIST_INSTANTIATE(T) \
VCL_LIST_INSTANTIATE_ITERATOR(vcl_list<T >::iterator, vcl_list<T >::iterator, vcl_list<T >::difference_type);\
template class vcl_list<T >; \
template std::pair<vcl_list<T >::const_iterator, vcl_list<T >::const_iterator> \
  std::mismatch(vcl_list<T >::const_iterator, vcl_list<T >::const_iterator, vcl_list<T >::const_iterator)

#endif
