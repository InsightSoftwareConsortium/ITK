#ifndef vcl_emulation_algorithm_txx_
#define vcl_emulation_algorithm_txx_

#include <vcl_algorithm.h>

#undef VCL_SWAP_INSTANTIATE
#define VCL_SWAP_INSTANTIATE(T) \
VCL_INSTANTIATE_INLINE(void vcl_swap(T&, T&))

#undef VCL_OPERATOR_NE_INSTANTIATE
#define VCL_OPERATOR_NE_INSTANTIATE(T) \
VCL_INSTANTIATE_INLINE(bool operator!=(T const&, T const &))

#define VCL_CONTAINABLE_INSTANTIATE(T) \
VCL_INSTANTIATE_INLINE(void vcl_construct(T *, T const &)); \
VCL_INSTANTIATE_INLINE(void vcl_destroy(T *))
//;VCL_SWAP_INSTANTIATE(T)

//SGI CC does not allow explicit instantiation of inlines.
//So use "VCL_INSTANTIATE_INLINE" instead of "template".
#define VCL_FIND_INSTANTIATE(I, T) \
VCL_INSTANTIATE_INLINE( I vcl_find(I, I, T const&) )

#define VCL_REMOVE_INSTANTIATE(I, T) \
VCL_INSTANTIATE_INLINE( I vcl_remove(I, I, T const&) )

// I is a random access iterator.
// this works for vector<double>::iterator with gcc 2.7 and irix6-CC-n32 :
#define VCL_SORT_INSTANTIATE(I, T) \
VCL_INSTANTIATE_INLINE( void vcl_sort(I, I ) )
#define VCL_SORT_INSTANTIATE_CMP(I, T, C) \
VCL_INSTANTIATE_INLINE( void vcl_sort(I, I, C ) )

#undef VCL_COPY_INSTANTIATE
#define VCL_COPY_INSTANTIATE(Inp, Out) \
VCL_INSTANTIATE_INLINE(Out vcl_copy(Inp, Inp, Out))

#undef VCL_COPY_BACKWARD_INSTANTIATE
#define VCL_COPY_BACKWARD_INSTANTIATE(Inp, Out) \
VCL_INSTANTIATE_INLINE(Out vcl_copy_backward(Inp, Inp, Out))

#define VCL_FIND_IF_INSTANTIATE(I, P) \
VCL_INSTANTIATE_INLINE(I vcl_find_if(I, I, P))

#define VCL_UNIQUE_INSTANTIATE(I) /* */

#endif // vcl_emulation_algorithm_txx_
