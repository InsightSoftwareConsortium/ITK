#undef VCL_COMPLEX_INSTANTIATE
#if VCL_USE_NATIVE_COMPLEX
#include <vcl_iostream.h>
#define VCL_COMPLEX_INSTANTIATE(T) \
template vcl_ostream& std::operator<<(vcl_ostream&,const std::complex<T>&)
#else
#define VCL_COMPLEX_INSTANTIATE(T)
#endif
