#ifndef vcl_win32_vc60_complex_h_
#define vcl_win32_vc60_complex_h_


// fsm: complex<T> is derived from _Complex_base<T>, which is not
// declared with __declspec(dllimport). So complex<T> shouldn't be either
// (else the compiler will emit an error). Whether or not it is depends on
// the value of _CRTIMP being set, e.g. in <math.h>

// However, modifying _CRTIMP is bad because it leads to inconsistent
// dll linkage issues for other functions. <complex> includes other
// things, like the streams. If _CRTIMP is changes, then the
// declaration of the streams effectively change. This causes all
// kinds of problems.
//
// The compiler issues a warning about the inconsistency in <complex>,
// but it seems that the warning can be safely ignored provided the
// implementors wrote the code correcting. Since this is part of the
// compiler system, we can assume that the implementators have
// addressed the issue, and that we can safely ignore the warning. A
// couple of articles on the web seem to support this position:
// http://support.microsoft.com/default.aspx?scid=http://support.microsoft.com:80/support/kb/articles/Q134/9/80.asp&NoWebContent=1
// http://www.osl.iu.edu/MailArchives/mtl-devel/msg00395.php

// Disable "warning C4275: non dll-interface class
// 'std::_Complex_base<float>' used as base for dll-interface class
// 'std::complex<float>'" for <complex>
//
#pragma warning (push)
#pragma warning (disable: 4275)
#include <complex>
#pragma warning (pop)

// It used to necessary to bring the complex math functions
// from the std namespace into the global namespace to avoid
// conflicts with the (incorrect) cmath and cstdlib headers. It
// is no longer necessary because now vcl versions of those
// headers bring the math functions into the std namespace
// instead. Thus std::complex can live completely in the std
// namespace.
//   -- Amitha Perera

#define vcl_generic_complex_STD std
#include "../generic/vcl_complex.h"

#endif // vcl_win32_vc60_complex_h_
