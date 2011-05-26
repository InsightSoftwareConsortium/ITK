#ifndef vcl_egcs_complex_txx_
#define vcl_egcs_complex_txx_

#include <vcl_complex.h>

#include <std/complext.cc>

#define VCL_COMPLEX_INSTANTIATE_INLINE(T) template T

#undef VCL_COMPLEX_INSTANTIATE
#define VCL_COMPLEX_INSTANTIATE(FLOAT) \
VCL_COMPLEX_INSTANTIATE_INLINE(bool operator==(complex<FLOAT >const&,complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(bool operator==(FLOAT,complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(bool operator==(complex<FLOAT >const&,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(FLOAT imag(complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(FLOAT real(complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > sqrt (complex<FLOAT >const& x));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > operator + (complex<FLOAT >const&,complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > operator + (complex<FLOAT >const&,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > operator + (FLOAT,complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > operator - (complex<FLOAT >const&,complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > operator - (complex<FLOAT >const&,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > operator - (FLOAT,complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > operator * (complex<FLOAT >const&,complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > operator * (complex<FLOAT >const&,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > operator * (FLOAT,complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > operator / (complex<FLOAT >const&,complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > operator / (complex<FLOAT >const&,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > operator / (FLOAT,complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > polar (FLOAT,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > pow (complex<FLOAT >const&,complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > pow (complex<FLOAT >const&,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > pow (complex<FLOAT >const&,int));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > pow (FLOAT,complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > exp (complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(complex<FLOAT > log (complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(FLOAT arg (complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(FLOAT abs (complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(FLOAT norm (complex<FLOAT >const&));\
template complex<FLOAT >& __doadv (complex<FLOAT >* ths, const complex<FLOAT >& y);\
template vcl_ostream& operator<<(vcl_ostream &, complex<FLOAT > const &)

#endif // vcl_egcs_complex_txx_
