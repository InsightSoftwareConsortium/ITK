#ifndef test_util_h_
#define test_util_h_

#include <vcl_complex_fwd.h>
#define macro(T) void test_util_fill_random(T *begin, T *end)
macro(float);
macro(double);
macro(long double);
macro(vcl_complex<float>);
macro(vcl_complex<double>);
macro(vcl_complex<long double>);
#undef macro

#endif
