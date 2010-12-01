// This is vcl/vcl_iostream.h
#ifndef vcl_iostream_h_
#define vcl_iostream_h_
//:
// \file
// \brief Include compiler's <iostream.h> in a uniform way.
// \author awf@robots.ox.ac.uk
//
//  In particular, define the following
// \code
//   vcl_ostream
//   vcl_ios_X (as in ios::X)
//   operator>>(vcl_ostream &, T &) for T in {signed char, bool}
//   vcl_hex
//   vcl_dec
//   vcl_ws
//   vcl_setprecision
//   vcl_streampos
//   vcl_streambuf
//   vcl_streamsize
//   vcl_cin
//   vcl_cout
//   vcl_cerr
// \endcode
//

#include "vcl_iosfwd.h" // Include this to ensure the two are consistent.

// Notes to maintainers.
//   The purpose of this file is to repair broken iostream
// headers. Thus in conditional logic, the compilers that
// behave in a non-standard way should be treated first, as
// special cases, and the #else arm should contain the
// appropriate action for an ISO compiler.
//
// On win32, <iostream.h> contains old crufty iostreams and
// <iostream> contains new standard ones. There is no iosfwd
// for the old ones and <string> includes the new iostreams.
// So we must avoid the old ones at any price.
//
// ------------------------------------------------------------

#if defined(VCL_SGI_CC_720)
# include "sgi/vcl_iostream.h"

#else // -------------------- ISO
# include "iso/vcl_iostream.h"
#endif

   // -------------------- miscellaneous fixes which can go at the end: -------

// Need std::ios::nocreate to avoid creating an empty file on
// attempts to read a non-existent one. Don't we? -- fsm
#if defined(VCL_VC50)
# undef  vcl_ios_in
# define vcl_ios_in      (std::ios::in | std::ios::nocreate)
#endif

// It seems that VC++ can show strange behaviour without this include:
#if defined(VCL_VC)
# include <vcl_fstream.h>
#endif

#endif // vcl_iostream_h_
