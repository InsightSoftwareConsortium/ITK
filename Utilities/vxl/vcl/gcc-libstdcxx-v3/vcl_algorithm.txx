#ifndef vcl_gcc_libstdcxx_v3_algorithm_txx_
#define vcl_gcc_libstdcxx_v3_algorithm_txx_

#include <vcl_algorithm.h>

#define VCL_SWAP_INSTANTIATE(T) /* */

#define VCL_OPERATOR_NE_INSTANTIATE(T) /* */

#define VCL_CONTAINABLE_INSTANTIATE(T) /* */

#define VCL_SORT_INSTANTIATE(I, T) \
namespace std { template void sort(I, I); }

#define VCL_SORT_INSTANTIATE_CMP(I, T, C) /* */

#define VCL_COPY_INSTANTIATE(Inp, Out) /* */

#define VCL_FIND_INSTANTIATE(I, T) \
namespace std { template I std::find(I, I, T const &); }

#define VCL_FIND_IF_INSTANTIATE(I, P) \
namespace std { template I std::find_if(I, I, P); }

#define VCL_REMOVE_INSTANTIATE(I, T) /* */

#define VCL_UNIQUE_INSTANTIATE(I) /* */

#endif // vcl_gcc_libstdcxx_v3_algorithm_txx_
