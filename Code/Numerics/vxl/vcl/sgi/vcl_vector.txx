#ifndef vcl_sgi_vector_txx_
#define vcl_sgi_vector_txx_

#include <vcl_vector.h>

#define VCL_VECTOR_STLINST_uninitialized_copy(Inp, Fwd, Size) \
template Fwd std::copy(Inp, Inp, Fwd);\
template Fwd std::copy_backward(Inp, Inp, Fwd)

// --- Vector ---
#undef VCL_VECTOR_INSTANTIATE
#if VCL_USE_NATIVE_STL
#define VCL_VECTOR_INSTANTIATE(T) \
template class std::vector<T,std::__default_alloc_template<true,0> >
#else
#define VCL_VECTOR_INSTANTIATE(T) \
//VCL_VECTOR_STLINST_uninitialized_copy(vcl_vector<T >::const_iterator, vcl_vector<T >::iterator, vcl_vector<T >::size_type);\
//VCL_VECTOR_STLINST_uninitialized_copy(vcl_vector<T >::iterator, vcl_vector<T >::iterator, vcl_vector<T >::size_type);\
//template class vcl_vector<T >;\
//template void std::fill(vcl_vector<T >::iterator, vcl_vector<T >::iterator, int const&)
#endif

#endif // vcl_sgi_vector_txx_
