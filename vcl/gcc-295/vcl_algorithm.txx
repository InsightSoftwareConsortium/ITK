#ifndef vcl_gcc_295_algorithm_txx_
#define vcl_gcc_295_algorithm_txx_

#include <vcl_algorithm.h>
#include <vcl_iterator.h>

#define VCL_SWAP_INSTANTIATE(T) \
VCL_INSTANTIATE_INLINE(void swap(T&, T&))

#define VCL_OPERATOR_NE_INSTANTIATE(T)

#define VCL_CONTAINABLE_INSTANTIATE(T)

// I is a random access iterator
#define VCL_SORT_INSTANTIATE(I, T) \
template void __final_insertion_sort(I, I); \
template void __introsort_loop(I, I, I, int); \
template void __introsort_loop(I, I, I, long)

#define VCL_SORT_INSTANTIATE_CMP(I, T, C) \
/* fix as needed */

#define VCL_COPY_INSTANTIATE(Inp, Out) \
template Out copy(Inp, Inp, Out)

// The instantiation macro for find() uses the helper function find(I, I, T, tag)
// However, there seems to be no way to get the iterator category of I other than
// using iterator_traits<I>::iterator_category and this is a problem because the
// helper is only defined for input_iterators and random_access_iterators. Since a
// bidirectional_iterator_tag is an input_iterator, the following should be harmless.
//
// An alternative is to use this fantastic trick:
//   template <int N> struct fsm_find_tickler; /* empty template */ \|
//   template <> struct fsm_find_tickler<__LINE__> { void method(I, I, T const &); }; \|
//   void fsm_find_tickler<__LINE__>::method(I b, I e, T const &v) { find(b, e, v); } \|
// which causes the compiler to emit the necessary helper functions as part of implicit
// instantiation. However, such helpers are weak symbols and apparently don't work
// on certain architectures (gcc 2.95 on HP ?).
template <typename _BdIter, typename _Tp>
inline _BdIter find(_BdIter __first,
                    _BdIter __last,
                    _Tp const & __val,
                    vcl_bidirectional_iterator_tag)
{
  return ::find(__first, __last, __val, vcl_input_iterator_tag());
}

template <typename _BdIter, typename _Pred>
inline _BdIter find_if(_BdIter __first,
                       _BdIter __last,
                       _Pred   __pred,
                       vcl_bidirectional_iterator_tag)
{
  return ::find_if(__first, __last, __pred, vcl_input_iterator_tag());
}

#define VCL_FIND_INSTANTIATE(I, T) \
template < int N > struct fsm_find_tickler; /* empty template */ \
template <> struct fsm_find_tickler< __LINE__ > { void method(I, I, T const &); }; \
void fsm_find_tickler< __LINE__ >::method(I b, I e, T const &v) { find(b, e, v); } \
template I find<I, T >(I, I, T const&); \
template I find<I, T >(I, I, T const&, vcl_iterator_traits<I >::iterator_category)

#define VCL_FIND_IF_INSTANTIATE(I, P) \
template I find_if<I, P >(I, I, P); \
template I find_if<I, P >(I, I, P, vcl_iterator_traits<I >::iterator_category)

#define VCL_REMOVE_INSTANTIATE(I, T) \
template I remove(I, I, T const &)

#define VCL_UNIQUE_INSTANTIATE(I) \
template I unique<I >(I, I)

#endif // vcl_gcc_295_algorithm_txx_
