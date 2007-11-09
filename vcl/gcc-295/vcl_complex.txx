#ifndef vcl_gcc295_complex_txx_
#define vcl_gcc295_complex_txx_

#include <vcl_complex.h>

#include <std/complext.cc>

#define VCL_COMPLEX_INSTANTIATE_INLINE(f) template f

// awf: So, I think this macro is here for users to instantiate
// complex<rational>.  Therefore it must have template class complex<FLOAT>
// in it, therefore it cannot be used to instantiate the native complices

// fsm: But, vcl/Templates/vcl_complex.cxx needs to call _some_ macro in
// order to produce an instance definition for compilers that use emulation.
// I've put an #ifdef in there, but it would be nice to avoid that.

#undef VCL_COMPLEX_INSTANTIATE
#define VCL_COMPLEX_INSTANTIATE(FLOAT) \
template class complex<FLOAT >; \
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
VCL_COMPLEX_INSTANTIATE_INLINE(FLOAT norm (complex<FLOAT >const&)); \
/* no doadv */ \
template vcl_ostream& operator << (vcl_ostream&, complex<FLOAT > const &); \
template vcl_istream& operator >> (vcl_istream&, complex<FLOAT >&)

#endif // vcl_gcc295_complex_txx_
