#ifndef vcl_sunpro_vcl_algorithm_txx_
#define vcl_sunpro_vcl_algorithm_txx_

#include <vcl_algorithm.h>

#define VCL_COPY_INSTANTIATE(Inp, Out) \
template Out std::copy(Inp, Inp, Out)

// this one can't live with copy() as we sometimes instantiate
// copy() for iterators that can only move forward.
#define VCL_COPY_BACKWARD_INSTANTIATE(Inp, Out) \
template Out std::copy_backward(Inp, Inp, Out)

#define VCL_FILL_INSTANTIATE(Out, T) \
template void std::fill(Out, Out, T const &)

#define VCL_SWAP_INSTANTIATE(T) \
VCL_INSTANTIATE_INLINE(void swap(T&, T&))

#define VCL_OPERATOR_NE_INSTANTIATE(T) \
VCL_INSTANTIATE_INLINE(bool operator!=(T const&, T const &))

#define VCL_CONTAINABLE_INSTANTIATE(T) \
VCL_INSTANTIATE_INLINE(void construct(T *, T const &)); \
VCL_INSTANTIATE_INLINE(void destroy(T *))

#define VCL_LESS_INSTANTIATE(T) \
template struct vcl_less<T >

#define VCL_FIND_INSTANTIATE(I, T) \
template I vcl_find(I, I, T const&)

// vcl_sort(). We really do need to know both the element type
// and the iterator type.
//
// I is a random access iterator
// T is the element type
// C is the comparator type
#define VCL_SORT_INSTANTIATE(I, T) \
template void std::__final_insertion_sort(I, I); \
template void std::__quick_sort_loop_aux(I, I, T *); \
template void std::__insertion_sort(I, I); \
template I    std::__unguarded_partition(I, I, T); \
template void std::__unguarded_insertion_sort_aux(I, I, T *); \
template void std::__unguarded_linear_insert(I, T)
#define VCL_SORT_INSTANTIATE_CMP(I, T, C) \
template void std::__final_insertion_sort(I, I, C); \
template void std::__quick_sort_loop_aux(I, I, T *, C); \
template void std::__insertion_sort(I, I, C); \
template I    std::__unguarded_partition(I, I, T, C); \
template void std::__unguarded_insertion_sort_aux(I, I, T *, C); \
template void std::__unguarded_linear_insert(I, T, C)

#define VCL_FIND_IF_INSTANTIATE(I, P) \
VCL_INSTANTIATE_INLINE(I find_if(I, I, P))

#endif // vcl_sunpro_vcl_algorithm_txx_
