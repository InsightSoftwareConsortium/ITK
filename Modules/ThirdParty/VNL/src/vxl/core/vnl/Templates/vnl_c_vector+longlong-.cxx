#include <vcl_compiler.h>
#if VCL_HAS_LONG_LONG
#include <vnl/vnl_c_vector.h>
#include <vnl/vnl_c_vector.hxx>

VNL_C_VECTOR_INSTANTIATE_ordered(long long);
#else
void vnl_c_vector_longlong_dummy(void) {}
#endif
