#ifndef vcl_gcc_libstdcxx_v3_list_txx_
#define vcl_gcc_libstdcxx_v3_list_txx_

#include <vcl_list.h>

#undef VCL_LIST_INSTANTIATE
#ifdef VCL_GCC_31
#define VCL_LIST_INSTANTIATE(T) \
template class vcl_list<T >
#else
#define VCL_LIST_INSTANTIATE(T) \
template class vcl_list<T >; \
/* a member template : */ \
template void \
std::list<T >::_M_insert_dispatch(std::list<T >::iterator,std::list<T >::iterator,std::list<T >::iterator,_Bool<false >)
#endif

#if 0
void std::list<int, std::allocator<int > >::_M_insert_dispatch
<std::_List_iterator<int, int &, int *> >
(std::_List_iterator<int, int &, int *>,
 std::_List_iterator<int, int &, int *>,
 std::_List_iterator<int, int &, int *>,
 _Bool<false >)
#endif

#endif // vcl_gcc_libstdcxx_v3_list_txx_
