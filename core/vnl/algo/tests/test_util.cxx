// This is core/vnl/algo/tests/test_util.cxx
#include <vcl_complex.h>
#include <vnl/vnl_random.h>

#define macro(T) \
void test_util_fill_random(T *b, T *e, vnl_random &rng) \
{ \
  for (T *p=b; p<e; ++p) \
    *p = (T)rng.drand64(-1.0, +1.0); \
} \
void test_util_fill_random(vcl_complex<T> *b, vcl_complex<T> *e, vnl_random &rng) \
{ \
  for (vcl_complex<T> *p=b; p<e; ++p) \
    *p = vcl_complex<T>((T)rng.drand64(-1.0, +1.0), (T)rng.drand64(-1.0, +1.0)); \
}

macro(float);
macro(double);
macro(long double);
#undef macro
