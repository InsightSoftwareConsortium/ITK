#ifndef vnl_matlab_print_scalar_h_
#define vnl_matlab_print_scalar_h_
/*
  fsm
*/

//:
// \file

#include <iosfwd>
#include <complex>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include "vnl_matlab_print_format.h"
#include "vnl/vnl_export.h"
#include "vcl_compiler.h"

//: print real or complex scalar into character buffer.
#define vnl_matlab_print_scalar_declare(T) \
  VNL_EXPORT void vnl_matlab_print_scalar( \
    T v, char * buf, size_t buf_len, vnl_matlab_print_format = vnl_matlab_print_format_default)

// Even with a function template we would have to
// forward declare all the specializations anyway.
vnl_matlab_print_scalar_declare(int);
vnl_matlab_print_scalar_declare(unsigned int);
vnl_matlab_print_scalar_declare(float);
vnl_matlab_print_scalar_declare(double);
vnl_matlab_print_scalar_declare(long double);
vnl_matlab_print_scalar_declare(std::complex<float>);
vnl_matlab_print_scalar_declare(std::complex<double>);
vnl_matlab_print_scalar_declare(std::complex<long double>);

#if !VXL_LEGACY_FUTURE_REMOVE
//: print real or complex scalar into character buffer.
// These variants are deprecated because the buffer size is not provided
#  define vnl_matlab_print_scalar_declare_old(T)            \
    VNL_EXPORT VXL_DEPRECATED void vnl_matlab_print_scalar( \
      T v, char * buf, vnl_matlab_print_format = vnl_matlab_print_format_default)

// Even with a function template we would have to
// forward declare all the specializations anyway.
vnl_matlab_print_scalar_declare_old(int);
vnl_matlab_print_scalar_declare_old(unsigned int);
vnl_matlab_print_scalar_declare_old(float);
vnl_matlab_print_scalar_declare_old(double);
vnl_matlab_print_scalar_declare_old(long double);
vnl_matlab_print_scalar_declare_old(std::complex<float>);
vnl_matlab_print_scalar_declare_old(std::complex<double>);
vnl_matlab_print_scalar_declare_old(std::complex<long double>);

#endif

//: print scalar to std::ostream.
template <class T>
VNL_EXPORT std::ostream &
vnl_matlab_print_scalar(std::ostream &, T value, vnl_matlab_print_format = vnl_matlab_print_format_default);

#endif
