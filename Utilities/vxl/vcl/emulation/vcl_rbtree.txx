#ifndef vcl_emulation_rbtree_txx_
#define vcl_emulation_rbtree_txx_

#include "vcl_algorithm.txx"
#include "vcl_iterator.txx"
#include "vcl_utility.txx"

//-*- c++ -*-
// --- Feature testing ---
#ifdef __STL_LOOP_INLINE_PROBLEMS
#define VCL_INSTANTIATE_INLINE_LOOP(f) template f
#else
#define VCL_INSTANTIATE_INLINE_LOOP(f) VCL_INSTANTIATE_INLINE(f)
#endif

// --- Unary templates ---
// Templates with one type mentioned, no requirements on type

#define VCL_OPERATOR_NE_INSTANTIATE(T) \
VCL_INSTANTIATE_INLINE(bool operator!=(T const&, T const &))

#undef VCL_COMPARISONS_INSTANTIATE
#define VCL_COMPARISONS_INSTANTIATE(T) \
VCL_OPERATOR_NE_INSTANTIATE(T) \
VCL_INSTANTIATE_INLINE(bool operator >  (T const &, T const &)); \
VCL_INSTANTIATE_INLINE(bool operator <= (T const &, T const &)); \
VCL_INSTANTIATE_INLINE(bool operator >= (T const &, T const &))

// --- Iterators ---

#define VCL_TAGS_INSTANTIATE(I, TAG) \
VCL_INSTANTIATE_INLINE(TAG iterator_category(I const &))

#define VCL_ITER_FWD_INSTANTIATE(ForwardIterator) \
VCL_OPERATOR_NE_INSTANTIATE(ForwardIterator)\
VCL_TAGS_INSTANTIATE(ForwardIterator, vcl_forward_iterator_tag)

#if 0
#define VCL_ITER_BD_Distance_INSTANTIATE(BidirectionalIterator, Distance) \
VCL_INSTANTIATE_INLINE(void vcl_advance(BidirectionalIterator&,Distance));\
VCL_INSTANTIATE_INLINE(void __advance(BidirectionalIterator&,Distance,vcl_bidirectional_iterator_tag));\
VCL_INSTANTIATE_INLINE(void vcl_distance(BidirectionalIterator,BidirectionalIterator,Distance&));\
VCL_INSTANTIATE_INLINE(void __distance(BidirectionalIterator,BidirectionalIterator const&,Distance&,vcl_bidirectional_iterator_tag))

#define VCL_ITER_RA_Distance_INSTANTIATE(RandomAccessIterator, Distance) \
VCL_INSTANTIATE_INLINE(void vcl_advance(RandomAccessIterator&,Distance));\
VCL_INSTANTIATE_INLINE(void __advance(RandomAccessIterator&,Distance,vcl_random_access_iterator_tag));\
VCL_INSTANTIATE_INLINE(void vcl_distance(RandomAccessIterator,RandomAccessIterator,Distance&));\
VCL_INSTANTIATE_INLINE(void __distance(RandomAccessIterator const&,RandomAccessIterator const&,\
                                       Distance&,vcl_random_access_iterator_tag))
#endif

// --- Vcl_List ---

#define STLINST_uninitialized_copy(I, F) \
VCL_INSTANTIATE_INLINE_LOOP(F uninitialized_copy(I, I, F))


/////////////////////////////////////////////////////////////////////////////


// --- RB TREE ---
#define VCL_RBTREE_INSTANTIATE(Key, Value, GetKey, Compare) \
template class rb_tree<Key, Value, GetKey, Compare, vcl_alloc >

#define VCL_RBTREE_VALUE_INSTANTIATE(Value) \
template class __rb_tree_base<Value, vcl_alloc >;\
template struct __rb_tree_iterator<Value >;\
template struct __rb_tree_const_iterator<Value >;\
template class vcl_simple_alloc<__rb_tree_node<Value >, vcl_alloc >;\
VCL_ITER_BD_Distance_INSTANTIATE(__rb_tree_iterator<Value >, vcl_size_t);\
VCL_ITER_BD_Distance_INSTANTIATE(__rb_tree_const_iterator<Value >, vcl_size_t);\
VCL_OPERATOR_NE_INSTANTIATE(__rb_tree_const_iterator<Value >) \
VCL_OPERATOR_NE_INSTANTIATE(__rb_tree_iterator<Value >) \
VCL_PAIR_INSTANTIATE(__rb_tree_iterator<Value >, __rb_tree_iterator<Value > ); \
VCL_PAIR_INSTANTIATE(__rb_tree_const_iterator<Value >, __rb_tree_const_iterator<Value > ); \
VCL_PAIR_INSTANTIATE(__rb_tree_const_iterator<Value >, bool ); \
VCL_PAIR_INSTANTIATE(__rb_tree_iterator<Value >, bool ); \
template class vcl_reverse_bidirectional_iterator<__rb_tree_const_iterator<Value >, Value, Value const &, vcl_ptrdiff_t>;\
template class vcl_reverse_bidirectional_iterator<__rb_tree_iterator<Value >, Value, Value &, vcl_ptrdiff_t>;\
VCL_SWAP_INSTANTIATE(__rb_tree_node<Value > *)

#endif // vcl_emulation_rbtree_txx_
