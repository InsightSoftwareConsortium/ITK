#include <vcl_compiler.h>
#if VCL_HAS_LONG_LONG
// Disable warning
#ifdef VCL_VC_DOTNET
// 4146: unary minus operator applied to unsigned type, result still unsigned
# pragma warning(disable:4146)
#endif //VCL_VC_DOTNET

#include <vnl/vnl_vector.hxx>
VNL_VECTOR_INSTANTIATE(unsigned long long);
#else
void vnl_vector_ulonglong_dummy(void) {}
#endif
