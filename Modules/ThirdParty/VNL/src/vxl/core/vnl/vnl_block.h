// This is core/vnl/vnl_block.h
#ifndef vnl_block_h_
#define vnl_block_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \author fsm
//
// \verbatim
//  Modifications
//   2009-03-30 Peter Vanroose - Added arg_min() & arg_max() and reimplemented min_value() & max_value()
// \endverbatim
//
#include <vcl_compiler.h>
#include "vnl/vnl_export.h"

VNL_EXPORT void vnl_block_raise_exception(char const *FILE, int LINE, char const *why);

//: return sum of elements
template <class T> inline
T vnl_block_sum(T const x[], unsigned n)
{
  if (n == 0)
    return T(0);
  else {
    T ans = x[0];
    for (unsigned i=1; i<n; ++i)
      ans += x[i];
    return ans;
  }
}

//: return product of elements
template <class T> inline
T vnl_block_product(T const x[], unsigned n)
{
  if (n == 0)
    return T(1);
  else {
    T ans = x[0];
    for (unsigned i=1; i<n; ++i)
      ans *= x[i];
    return ans;
  }
}

//: return smallest value.
template <class T> inline
T vnl_block_min_value(T const *x, unsigned n)
{
  if (n == 0)
    vnl_block_raise_exception(__FILE__, __LINE__, "n is 0");
  T ans = *x;
  while (--n > 0)
    if (ans > *++x)
      ans = *x;
  return ans;
}

//: return largest value.
template <class T> inline
T vnl_block_max_value(T const *x, unsigned n)
{
  if (n == 0)
    vnl_block_raise_exception(__FILE__, __LINE__, "n is 0");
  T ans = *x;
  while (--n > 0)
    if (ans < *++x)
      ans = *x;
  return ans;
}

//: return index of smallest value.
template <class T> inline
unsigned vnl_block_arg_min(T const x[], unsigned n)
{
  if (n == 0)
    vnl_block_raise_exception(__FILE__, __LINE__, "n is 0");
  T tmp = *x;
  unsigned idx = 0;
  for (unsigned i=1; i<n; ++i)
    if (tmp > *++x)
      tmp = *x, idx = i;
  return idx;
}

//: return index of largest value.
template <class T> inline
unsigned vnl_block_arg_max(T const x[], unsigned n)
{
  if (n == 0)
    vnl_block_raise_exception(__FILE__, __LINE__, "n is 0");
  T tmp = *x;
  unsigned idx = 0;
  for (unsigned i=1; i<n; ++i)
    if (tmp < *++x)
      tmp = *x, idx = i;
  return idx;
}

//: y[i] = x[i]
template <class T> inline
void vnl_block_copy(T const x[], T y[], unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    y[i] = x[i];
}

//: reverses sequence
template <class T> inline
void vnl_block_reverse(T x[], unsigned n)
{
  T tmp;
  for (unsigned i=0; 2*i<n; ++i) {
    tmp = x[i];
    x[i] = x[n-1-i];
    x[n-1-i] = tmp;
  }
}

//: x[i] *= a
template <class T> inline
void vnl_block_scale(T a, T x[], unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    x[i] *= a;
}

//: y[i] = a * x[i]
template <class T> inline
void vnl_block_scale(T a, T const x[], T y[], unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    y[i] = a * x[i];
}

//: y[i] += x[i]
template <class T> inline
void vnl_block_add(T const x[], T y[], unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    y[i] += x[i];
}

//: z[i] = x[i] + y[i]
template <class T> inline
void vnl_block_add(T const x[], T const y[], T z[], unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    z[i] = x[i] + y[i];
}

//: z[i] = x[i] - y[i]
template <class T> inline
void vnl_block_sub(T const x[], T const y[], T z[], unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    z[i] = x[i] - y[i];
}

//: y[i] *= x[i]
template <class T> inline
void vnl_block_mul(T const x[], T y[], unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    y[i] *= x[i];
}

//: z[i]  = x[i] * y[i]
template <class T> inline
void vnl_block_mul(T const x[], T const y[], T z[], unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    z[i] = x[i] * y[i];
}

//: z[i]  = x[i] / y[i]
template <class T> inline
void vnl_block_div(T const x[], T const y[], T z[], unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    z[i] = x[i] / y[i];
}

//: y[i]  = -x[i]
template <class T> inline
void vnl_block_negate(T const x[], T y[], unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    y[i] = - x[i];
}

//: y[i]  = 1/x[i]
template <class T> inline
void vnl_block_invert(T const x[], T y[], unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    y[i] = T(1) / x[i];
}

//:  y[i] += a * x[i]
template <class T> inline
void vnl_block_axpy(T a, T const x[], T y[], unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    y[i] += a * x[i];
}

//: x[i]  = v
template <class T> inline
void vnl_block_fill(T x[], unsigned n, T value)
{
  for (unsigned i=0; i<n; ++i)
    x[i] = value;
}

#endif // vnl_block_h_
