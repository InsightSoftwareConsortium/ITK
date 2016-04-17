#ifndef test_util_h_
#define test_util_h_

class vnl_random;
#include <complex>
#include <vcl_compiler.h>
#define macro(T) void test_util_fill_random(T *begin, T *end, vnl_random &rng)
macro(float);
macro(std::complex<float>);
macro(double);
macro(std::complex<double>);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
macro(long double);
macro(std::complex<long double>);
#endif
#undef macro

#endif
