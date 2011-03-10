//-*- c++ -*-------------------------------------------------------------------
#ifndef vcl_complex_fwd_h_
#define vcl_complex_fwd_h_

// This used to forward declare the std::complex classes. However,
// this created a mess on a number of compilers because they
// used internal magic to implement it. Since <complex_fwd> is
// not a C++ standard header file, this forwarding declaration
// file no longer forward declares. It is now equivalent to including
// vcl_complex.h, which is what you should've been doing in the
// first place.
//  -- Amitha Perera

#include "vcl_complex.h"

#endif // vcl_complex_fwd_h_
