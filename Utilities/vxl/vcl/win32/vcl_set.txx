#include <vcl_set.h>

#undef VCL_SET_INSTANTIATE
#define VCL_SET_INSTANTIATE(T, Comp)\
template class vcl_set<T, Comp >
