#ifndef vcl_sunpro_vector_txx_
#define vcl_sunpro_vector_txx_

#include <vcl_utility.h>
#include <vcl_vector.h>

#define VCL_VECTOR_STLINST_uninitialized_copy(Inp, Fwd, Size) \
template Fwd std::copy(Inp, Inp, Fwd);\
template Fwd std::copy_backward(Inp, Inp, Fwd)

// --- Vector ---
#define VCL_VECTOR_INSTANTIATE(T) \
template class std::vector<T, std::allocator<T > >; \
/* member templates */ \
/* !!this is not a member template!! */ \
/* template void std::vector<T, std::allocator<T > >::__insert_aux(std::vector<T >::iterator, T const &); */ \
/* helper functions */ \
template void std::fill(T *, T *, T const &); \
template T   *std::copy(T *, T *, T *); \
template T   *std::copy(T const *, T const *, T *); \
template T   *std::copy_backward(T *, T *, T *); \
/* the wrapper */ \
template struct vcl_vector_sunpro_50<T >;


//VCL_VECTOR_STLINST_uninitialized_copy(vcl_vector<T >::const_iterator, vcl_vector<T >::iterator, vcl_vector<T >::size_type);\
//VCL_VECTOR_STLINST_uninitialized_copy(vcl_vector<T >::iterator, vcl_vector<T >::iterator, vcl_vector<T >::size_type);\

#endif
