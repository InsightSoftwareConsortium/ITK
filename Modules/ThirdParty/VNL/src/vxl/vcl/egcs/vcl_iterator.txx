#ifndef vcl_egcs_iterator_txx_
#define vcl_egcs_iterator_txx_
/*
  fsm
*/

#include <vcl_iterator.h>

#define INSTANTIATE_TAGS(I, TAG) \
VCL_INSTANTIATE_INLINE(TAG iterator_category(I const &))

#define INSTANTIATE_ITER_FWD(ForwardIterator) \
INSTANTIATE_OPERATOR_NE(ForwardIterator);\
INSTANTIATE_TAGS(ForwardIterator, forward_iterator_tag)

#define INSTANTIATE_ITER_BD_Distance(BidirectionalIterator,Distance) \
VCL_INSTANTIATE_INLINE(void advance(BidirectionalIterator&,Distance));\
VCL_INSTANTIATE_INLINE(void __advance(BidirectionalIterator&,Distance,bidirectional_iterator_tag));\
VCL_INSTANTIATE_INLINE(void distance(BidirectionalIterator,BidirectionalIterator,Distance&));\
VCL_INSTANTIATE_INLINE(void __distance(BidirectionalIterator,BidirectionalIterator const&,Distance&,bidirectional_iterator_tag))

#define INSTANTIATE_ITER_BD(BidirectionalIterator) \
INSTANTIATE_ITER_BD_Distance(BidirectionalIterator, BidirectionalIterator::difference_type);\
INSTANTIATE_OPERATOR_NE(BidirectionalIterator);\
INSTANTIATE_TAGS(BidirectionalIterator, bidirectional_iterator_tag)

#define INSTANTIATE_ITER_RA_Distance(RandomAccessIterator,Distance) \
VCL_INSTANTIATE_INLINE(void advance(RandomAccessIterator&,Distance));\
VCL_INSTANTIATE_INLINE(void __advance(RandomAccessIterator&,Distance,random_access_iterator_tag));\
VCL_INSTANTIATE_INLINE(void distance(RandomAccessIterator,RandomAccessIterator,Distance&));\
VCL_INSTANTIATE_INLINE(void __distance(RandomAccessIterator const&,RandomAccessIterator const&,\
                                       Distance&,random_access_iterator_tag))

#define INSTANTIATE_ITER_RA(RandomAccessIterator) \
INSTANTIATE_ITER_RA_Distance(RandomAccessIterator, vcl_ptrdiff_t);\
INSTANTIATE_OPERATOR_NE(RandomAccessIterator);\
INSTANTIATE_TAGS(RandomAccessIterator, random_access_iterator_tag)

#endif // vcl_egcs_iterator_txx_
