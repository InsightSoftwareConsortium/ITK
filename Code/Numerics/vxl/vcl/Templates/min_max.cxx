#include <vcl_algorithm.h>

#if defined(VCL_GCC_27)
template int vcl_min(int const &, int const &);
template int vcl_max(int const &, int const &);

template float vcl_max(float,float);
template float vcl_min(float,float);
#endif
