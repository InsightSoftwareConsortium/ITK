#ifndef vcl_win32_algorithm_txx_
#define vcl_win32_algorithm_txx_

#define VCL_COPY_INSTANTIATE(Inp, Out) \
// template Out std::copy(Inp, Inp, Out)

#define VCL_SWAP_INSTANTIATE(T) \
//VCL_INSTANTIATE_INLINE(void swap(T&, T&))

#define VCL_OPERATOR_NE_INSTANTIATE(T)

#define VCL_CONTAINABLE_INSTANTIATE(T)

#define VCL_LESS_INSTANTIATE(T) \
//template struct vcl_less<T >

#define VCL_FIND_INSTANTIATE(I, T) \
// template I std::find(I, I, T const&)

#define VCL_FIND_IF_INSTANTIATE(I, P) \
// template I std::find_if(I, I, P)

//
#define VCL_SORT_INSTANTIATE(I, T)
#define VCL_SORT_INSTANTIATE_CMP(I, T, C)

#endif // vcl_win32_algorithm_txx_
