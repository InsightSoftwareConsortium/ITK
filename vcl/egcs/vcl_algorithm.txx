#ifndef vcl_egcs_algorithm_txx_
#define vcl_egcs_algorithm_txx_

#include <vcl_algorithm.h>

#define VCL_COPY_INSTANTIATE(Inp, Out) \
template Out std::copy(Inp, Inp, Out)

#define VCL_COPY_BACKWARD_INSTANTIATE(I, O)

#define VCL_SWAP_INSTANTIATE(T) \
VCL_INSTANTIATE_INLINE(void swap(T&, T&))

#define VCL_OPERATOR_NE_INSTANTIATE(T)

#define VCL_CONTAINABLE_INSTANTIATE(T)

#define VCL_LESS_INSTANTIATE(T) \
template struct less<T >

#define VCL_FIND_INSTANTIATE(I, T) \
template I std::find(I, I, T const&)

#define VCL_SORT_INSTANTIATE(I, T) \
template void __final_insertion_sort(I, I); \
template void __introsort_loop(I, I, I, int)
#define VCL_SORT_INSTANTIATE_CMP(I, T, C) \
/* fix it if you need it */

#define VCL_FIND_IF_INSTANTIATE(I, P) \
template I find_if(I, I, P)

#endif // vcl_egcs_algorithm_txx_
