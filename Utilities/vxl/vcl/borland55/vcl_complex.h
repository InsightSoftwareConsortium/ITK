#ifndef vcl_borland55_complex_h_
#define vcl_borland55_complex_h_

// This header's vcl_abs must be consistent with vcl_cmath.h's version.

// Standard version mostly works.
#include "../iso/vcl_complex.h"

// Replace abs from standard version.
#undef vcl_abs
#define vcl_abs vcl_abs

inline long double vcl_abs(const vcl_complex<long double>& c)
{ return ::std::abs(c); }
inline double vcl_abs(const vcl_complex<double>& c) { return ::std::abs(c); }
inline float vcl_abs(const vcl_complex<float>& c) { return ::std::abs(c); }

#endif // vcl_borland55_complex_h_
