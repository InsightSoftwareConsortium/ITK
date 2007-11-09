#ifndef vcl_emulation_complex_txx_
#define vcl_emulation_complex_txx_
//-*- c++ -*-------------------------------------------------------------------
//
// Module: complex
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: MOT
//
//-----------------------------------------------------------------------------

// Including emulation/complex.txx implies emulation/complex.h is included
#include "vcl_complex.h"

#ifdef VCL_SUNPRO_CC
#undef VCL_INSTANTIATE_INLINE
#define VCL_INSTANTIATE_INLINE(fn_decl)  template  fn_decl
#endif

#undef VCL_COMPLEX_INSTANTIATE
#define VCL_COMPLEX_INSTANTIATE(FLOAT) \
template class vcl_complex<FLOAT >; \
VCL_INSTANTIATE_INLINE(vcl_ostream& operator<<(vcl_ostream&,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(bool operator==(vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(bool operator==(FLOAT,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(bool operator==(vcl_complex<FLOAT >const&,FLOAT));\
VCL_INSTANTIATE_INLINE(FLOAT vcl_imag(vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(FLOAT vcl_real(vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_sqrt (vcl_complex<FLOAT >const& x));\
VCL_INSTANTIATE_INLINE(vcl_complex<float > operator + (vcl_complex<FLOAT > const &)); \
VCL_INSTANTIATE_INLINE(vcl_complex<float > operator - (vcl_complex<FLOAT > const &)); \
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator + (vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator + (vcl_complex<FLOAT >const&,FLOAT));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator + (FLOAT,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator - (vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator - (vcl_complex<FLOAT >const&,FLOAT));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator - (FLOAT,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator * (vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator * (vcl_complex<FLOAT >const&,FLOAT));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator * (FLOAT,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator / (vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator / (vcl_complex<FLOAT >const&,FLOAT));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator / (FLOAT,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_polar (FLOAT,FLOAT));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_pow (vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_pow (vcl_complex<FLOAT >const&,FLOAT));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_pow (vcl_complex<FLOAT >const&,int));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_pow (FLOAT,vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_exp (vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_log (vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(FLOAT vcl_arg (vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(FLOAT vcl_abs (vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(FLOAT vcl_norm (vcl_complex<FLOAT >const&));\
VCL_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_conj (vcl_complex<FLOAT >const&))

#endif // vcl_emulation_complex_txx_
