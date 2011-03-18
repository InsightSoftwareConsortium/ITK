#ifndef vcl_sgi_algorithm_txx_
#define vcl_sgi_algorithm_txx_

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
//template I find_if(I, I, P)

#if VCL_USE_NATIVE_STL
#define VCL_SORT_INSTANTIATE(I, T) \
template void std::__insertion_sort(I, I); \
template I std::__unguarded_partition(I, I, T); \
template void std::__unguarded_insertion_sort_aux(I, I, I); \
template void std::__partial_sort(I, I, I, I); \
template void std::__final_insertion_sort(I, I); \
template void std::__introsort_loop(I, I, I, int); \
template void std::__unguarded_linear_insert(I, T); \
template void std::sort_heap(I, I); \
template void std::__make_heap(I, I, I, int*); \
template void std::__adjust_heap(I, int, int, T); \
template void std::__push_heap(I,int,int, T)
#else
#define VCL_SORT_INSTANTIATE(I, T)
#endif
#define VCL_SORT_INSTANTIATE_CMP(I, T, C)

#endif // vcl_sgi_algorithm_txx_
