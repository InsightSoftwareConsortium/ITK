#ifndef vcl_sgi_complex_txx_
#define vcl_sgi_complex_txx_

#undef VCL_COMPLEX_INSTANTIATE
#if VCL_USE_NATIVE_COMPLEX
#include <vcl_iostream.h>
#define VCL_COMPLEX_INSTANTIATE(T) \
template vcl_ostream& operator<<(vcl_ostream&,const std::complex<T >&)
#else
#define VCL_COMPLEX_INSTANTIATE(T)
#endif

#endif // vcl_sgi_complex_txx_
