#ifndef vcl_complex_h_
#define vcl_complex_h_

#include "vcl_compiler.h"

// File: vcl_complex.h
// 
// The task of this horrible file is to rationalize the complex number
// support in the various compilers.  Basically it promises to give you:
//
// A working vcl_complex<T> template.
//
// Stream >> and << operators.
//
// Instances of the types vcl_complex<float> and vcl_complex<double>
//
// A macro VCL_COMPLEX_INSTANTIATE(T) which allows you to instantiate
// complex over other number types.
//
// If you just want to forward declare the vcl complex types, use 
// vcl_complex_fwd.h instead.
#include "vcl_complex_fwd.h"

// ---------- all emulation
#if !VCL_USE_NATIVE_COMPLEX 
# include "emulation/vcl_complex.h"

// ---------- native WIN32
#elif defined(VCL_VC)
# include "win32/vcl_complex.h"

// ---------- Borland compiler for Win32
#elif defined(VCL_BORLAND)
# include "borland/vcl_complex.h"

// ---------- SunPro compiler
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_complex.h"

// ---------- SGI 730
#elif defined(VCL_SGI_CC_730)
#include  "sgi/vcl_complex.h"

// ---------- ISO
#else
# include "iso/vcl_complex.h"
#endif

//--------------------------------------------------------------------------------

// bogus instantiation macro.
#define VCL_COMPLEX_INSTANTIATE(T) extern "you must include vcl_complex.txx instead"

#endif // vcl_complex_h_
