#ifndef test_util_h_
#define test_util_h_

class vnl_random;
#include <vcl_complex.h>
#define macro(T) void test_util_fill_random(T *begin, T *end, vnl_random &rng)
macro(float);
macro(double);
macro(long double);
macro(vcl_complex<float>);
macro(vcl_complex<double>);
macro(vcl_complex<long double>);
#undef macro

#endif
