#ifndef vcl_sgi_deque_txx_
#define vcl_sgi_deque_txx_

#undef VCL_DEQUE_INSTANTIATE
#if VCL_USE_NATIVE_STL
#define VCL_DEQUE_INSTANTIATE(T) \
template class std::deque<T,std::__default_alloc_template<true,0>, 0U>
#else
#define VCL_DEQUE_INSTANTIATE(T)
#endif

#endif // vcl_sgi_deque_txx_
