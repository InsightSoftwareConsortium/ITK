#include <vcl_compiler.h>
#if VCL_HAS_LONG_LONG
#include <vnl/vnl_matrix.hxx>
VNL_MATRIX_INSTANTIATE(long long);
#else
void vnl_matrix_longlong_dummy(void) {}
#endif
