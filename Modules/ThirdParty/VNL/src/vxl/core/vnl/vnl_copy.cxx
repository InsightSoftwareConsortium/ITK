// This is core/vnl/vnl_copy.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author fsm

#include <complex>
#include "vnl_copy.h"
#include <vcl_cassert.h>
#include <vcl_compiler.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>

//-------------------------------------------------------------------

template <class S, class T>
void vnl_copy(S const * const src, T *const dst, const unsigned n)
{
  for (unsigned int i=0; i<n; ++i)
    dst[i] = T(src[i]);
}

template <class S, class T>
void vnl_copy(S const &src, T &dst)
{
  assert(src.size() == dst.size());
  vnl_copy(src.begin(), dst.begin(), src.size());
}

//------------------------------------------------------------------------

// C arrays
#define VNL_COPY_INSTANTIATE0(S, T) \
template void VNL_EXPORT vnl_copy(S const * const, T * const, const unsigned )

VNL_COPY_INSTANTIATE0(float, double);
VNL_COPY_INSTANTIATE0(double, float);
VNL_COPY_INSTANTIATE0(double, long double);
#ifndef __hppa // assembler bug on HP?
VNL_COPY_INSTANTIATE0(long double, double);
#endif

#define vnl_copy_macro(S, D) \
template <> \
VNL_EXPORT void vnl_copy(std::complex<S> const * const src, std::complex<D> * const dst, const unsigned n) \
{ \
  for (unsigned int i=0; i<n; ++i) \
    dst[i] = std::complex<D>((D)std::real(src[i]), (D)std::imag(src[i])); \
}

vnl_copy_macro(float, double);
vnl_copy_macro(double, float);
vnl_copy_macro(double, long double);
vnl_copy_macro(long double, double);
#undef vnl_copy_macro

#define vnl_copy_dumb(S) \
template <> \
VNL_EXPORT void vnl_copy(S const * const src, S *const dst, const unsigned n) \
{ \
  for (unsigned int i=0; i<n; ++i) \
    dst[i] = src[i]; \
}

vnl_copy_dumb(float);
vnl_copy_dumb(double);
#undef vnl_copy_dumb

// vnl_* containers
#define VNL_COPY_INSTANTIATE(S, T) \
template VNL_EXPORT void vnl_copy(vnl_vector<S > const &, vnl_vector<T > &); \
template VNL_EXPORT void vnl_copy(vnl_matrix<S > const &, vnl_matrix<T > &); \
template VNL_EXPORT void vnl_copy(vnl_diag_matrix<S > const &, vnl_diag_matrix<T > &)

VNL_COPY_INSTANTIATE(float, float);
VNL_COPY_INSTANTIATE(double, double);

#define VNL_COPY_INSTANTIATE_twoway(S, T) \
VNL_COPY_INSTANTIATE(S, T); \
VNL_COPY_INSTANTIATE(T, S)

VNL_COPY_INSTANTIATE_twoway(float, double);
VNL_COPY_INSTANTIATE_twoway(std::complex<float>, std::complex<double>);
#ifndef __hppa // assembler bug on HP?
VNL_COPY_INSTANTIATE_twoway(double, long double);
VNL_COPY_INSTANTIATE_twoway(std::complex<double>, std::complex<long double>);
#endif
