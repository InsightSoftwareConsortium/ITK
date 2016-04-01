// This is core/vnl/algo/tests/test_util.cxx
#include <complex>
#include "test_util.h"
#include <vcl_compiler.h>
#include <vnl/vnl_random.h>

#define macro(T) \
void test_util_fill_random(T *b, T *e, vnl_random &rng) \
{ \
  for (T *p=b; p<e; ++p) \
    *p = (T)rng.drand64(-1.0, +1.0); \
} \
void test_util_fill_random(std::complex<T> *b, std::complex<T> *e, vnl_random &rng) \
{ \
  for (std::complex<T> *p=b; p<e; ++p) \
    *p = std::complex<T>((T)rng.drand64(-1.0, +1.0), (T)rng.drand64(-1.0, +1.0)); \
}

macro(float);
macro(double);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
macro(long double);
#endif
#undef macro
