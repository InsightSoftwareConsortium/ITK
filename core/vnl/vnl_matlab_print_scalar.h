#ifndef vnl_matlab_print_scalar_h_
#define vnl_matlab_print_scalar_h_
/*
  fsm
*/

//:
// \file

#include <iosfwd>
#include <complex>
#include <vcl_compiler.h>
#include <vnl/vnl_matlab_print_format.h>
#include "vnl/vnl_export.h"

//: print real or complex scalar into character buffer.
#define vnl_matlab_print_scalar_declare(T) \
VNL_EXPORT void vnl_matlab_print_scalar(T v, \
                             char *buf, \
                             vnl_matlab_print_format =vnl_matlab_print_format_default)

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

//: print scalar to std::ostream.
VCL_TEMPLATE_EXPORT template <class T> VNL_TEMPLATE_EXPORT
std::ostream &vnl_matlab_print_scalar(std::ostream &,
                                     T value,
                                     vnl_matlab_print_format =vnl_matlab_print_format_default);

#endif
