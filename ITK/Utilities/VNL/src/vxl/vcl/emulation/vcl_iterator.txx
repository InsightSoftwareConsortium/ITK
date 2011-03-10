//-*- c++ -*-
#ifndef vcl_emulation_iterator_txx_
#define vcl_emulation_iterator_txx_

#include "vcl_iterator.h"
#include "vcl_algorithm.txx"

#define VCL_TAGS_INSTANTIATE(I, TAG) \
VCL_INSTANTIATE_INLINE(TAG iterator_category(I const &))

#define VCL_ITER_FWD_INSTANTIATE(ForwardIterator) \
VCL_OPERATOR_NE_INSTANTIATE(ForwardIterator)\
VCL_TAGS_INSTANTIATE(ForwardIterator, vcl_forward_iterator_tag)

#define VCL_ITER_BD_INSTANTIATE_Distance(BidirectionalIterator, Distance) \
VCL_INSTANTIATE_INLINE(void vcl_advance(BidirectionalIterator&,Distance));\
VCL_INSTANTIATE_INLINE(void __advance(BidirectionalIterator&,Distance,vcl_bidirectional_iterator_tag));\
VCL_INSTANTIATE_INLINE(void vcl_distance(BidirectionalIterator,BidirectionalIterator,Distance&));\
VCL_INSTANTIATE_INLINE(void __distance(BidirectionalIterator,BidirectionalIterator const&,Distance&,vcl_bidirectional_iterator_tag))

#define VCL_ITER_BD_INSTANTIATE(BidirectionalIterator) \
VCL_ITER_BD_INSTANTIATE_Distance(BidirectionalIterator, BidirectionalIterator::difference_type) \
VCL_OPERATOR_NE_INSTANTIATE(BidirectionalIterator)\
VCL_TAGS_INSTANTIATE(BidirectionalIterator, vcl_bidirectional_iterator_tag)

#define VCL_ITER_RA_INSTANTIATE_Distance(RandomAccessIterator, Distance) \
VCL_INSTANTIATE_INLINE(void vcl_advance(RandomAccessIterator&,Distance));\
VCL_INSTANTIATE_INLINE(void __advance(RandomAccessIterator&,Distance,vcl_random_access_iterator_tag));\
VCL_INSTANTIATE_INLINE(void vcl_distance(RandomAccessIterator,RandomAccessIterator,Distance&));\
VCL_INSTANTIATE_INLINE(void __distance(RandomAccessIterator const&,RandomAccessIterator const&,\
                                       Distance&,vcl_random_access_iterator_tag))

#define VCL_ITER_RA_INSTANTIATE(RandomAccessIterator) \
VCL_ITER_RA_INSTANTIATE_Distance(RandomAccessIterator, vcl_ptrdiff_t) \
VCL_OPERATOR_NE_INSTANTIATE(RandomAccessIterator)\
VCL_TAGS_INSTANTIATE(RandomAccessIterator, vcl_random_access_iterator_tag)

#define VCL_ITER_BD_Distance_INSTANTIATE(BidirectionalIterator, Distance) \
VCL_INSTANTIATE_INLINE(void vcl_advance(BidirectionalIterator&,Distance));\
VCL_INSTANTIATE_INLINE(void __advance(BidirectionalIterator&,Distance,vcl_bidirectional_iterator_tag));\
VCL_INSTANTIATE_INLINE(void vcl_distance(BidirectionalIterator,BidirectionalIterator,Distance&));\
VCL_INSTANTIATE_INLINE(void __distance(BidirectionalIterator,BidirectionalIterator const&,Distance&,vcl_bidirectional_iterator_tag))

#endif // vcl_emulation_iterator_txx_
