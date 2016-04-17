#include <vcl_compiler.h>
#if VCL_HAS_LONG_LONG
#include <vnl/vnl_vector.hxx>
VNL_VECTOR_INSTANTIATE(long long);
#else
void vnl_vector_longlong_dummy(void) {}
#endif
