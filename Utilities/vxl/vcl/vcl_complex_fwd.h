//-*- c++ -*-------------------------------------------------------------------
#ifndef vcl_complex_fwd_h_
#define vcl_complex_fwd_h_

/*
  fsm \and awf@robots.ox.ac.uk
*/

// Purpose: Forward declare the template class known
// as vcl_complex<T> without the cost of including the
// header file. Also declare the two typedefs at the
// bottom.
// *** make sure this file is consistent with vcl_complex.h ***
//
// Note: the standard complex class is just std::complex<T>, so
// on an ISO compiler, no forward declaration header is needed.

#include "vcl_compiler.h"

// ---------- emulation
#if !VCL_USE_NATIVE_COMPLEX
# include "emulation/vcl_complex_fwd.h"

// ---------- Visual Studio 6
#elif defined(VCL_VC60)
# include "win32-vc60/vcl_complex_fwd.h"

// ---------- SunPro compiler
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_complex_fwd.h"

#elif defined(VCL_SGI_CC)
# include "iso/vcl_complex.h"

// ---------- ISO
#else
# include "iso/vcl_complex.h"
# ifndef vcl_complex
# define vcl_complex std::complex
# endif
#endif

#endif // vcl_complex_fwd_h_
