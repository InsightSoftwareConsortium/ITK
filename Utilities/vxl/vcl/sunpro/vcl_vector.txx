#ifndef vcl_sunpro_vector_txx_
#define vcl_sunpro_vector_txx_

#include <vcl_vector.h>

// --- Vector ---
#undef VCL_VECTOR_INSTANTIATE
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
template struct vcl_vector_sunpro_50<T >

#endif
