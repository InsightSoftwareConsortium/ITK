/*
  fsm@robots.ox.ac.uk
*/
#ifdef __GNUC__
#pragma implementation
#endif
#include "vnl_copy.h"
#include <vcl_cassert.h>
#include <vnl/vnl_complex.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>
#include <vnl/vnl_complex.h>

//--------------------------------------------------------------------------------

template <class S, class T>
void vnl_copy(S const *src, T *dst, unsigned n) {
  for (unsigned i=0; i<n; ++i)
    dst[i] = src[i];
}

template <class S, class T>
void vnl_copy(S const &src, T &dst) { 
  assert(src.size() == dst.size());
  vnl_copy(src.begin(), dst.begin(), src.size());
}

//--------------------------------------------------------------------------------

// C arrays
#define VNL_COPY_INSTANTIATE0(S, T) \
template void vnl_copy(S const *, T *, unsigned );

VNL_COPY_INSTANTIATE0(float, double);
VNL_COPY_INSTANTIATE0(double, float);
VCL_DEFINE_SPECIALIZATION
void vnl_copy(vnl_float_complex const *src, vnl_double_complex *dst, unsigned n) {
  for (unsigned i=0; i<n; ++i)
    dst[i] = vnl_double_complex(src[i].real(), src[i].imag());
}
VCL_DEFINE_SPECIALIZATION
void vnl_copy(vnl_double_complex const *src, vnl_float_complex *dst, unsigned n) {
  for (unsigned i=0; i<n; ++i)
    dst[i] = vnl_float_complex(src[i].real(), src[i].imag());
}

// vnl_* containers
#define VNL_COPY_INSTANTIATE(S, T) \
template void vnl_copy(vnl_vector<S> const &, vnl_vector<T> &); \
template void vnl_copy(vnl_matrix<S> const &, vnl_matrix<T> &); \
template void vnl_copy(vnl_diag_matrix<S> const &, vnl_diag_matrix<T> &);

VNL_COPY_INSTANTIATE(float, double);
VNL_COPY_INSTANTIATE(double, float);
VNL_COPY_INSTANTIATE(vnl_float_complex, vnl_double_complex);
VNL_COPY_INSTANTIATE(vnl_double_complex, vnl_float_complex);
