#ifndef vcl_borland_cmath_h_
#define vcl_borland_cmath_h_
// 1. include system header
#include <cmath>
#undef abs
#include <vcl_iostream.h>
inline double abs(long double  x) { return x >= 0 ? x : -x; vcl_cerr << "abs long double\n";}
inline double abs(double  x) { return x >= 0 ? x : -x;  vcl_cerr << "abs double\n";}
inline float  abs(float x) { return x >= 0 ? x : -x; vcl_cerr << "abs float\n";}

// abs
#undef vcl_abs
#define vcl_abs ::abs
//#endif

// rest of cmath can be defined using generic/vcl_complex.h
#define vcl_generic_cmath_STD std
#include "../generic/vcl_cmath.h"


#endif // vcl_cmath_h_
