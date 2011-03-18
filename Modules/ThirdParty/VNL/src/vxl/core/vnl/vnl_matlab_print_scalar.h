#ifndef vnl_matlab_print_scalar_h_
#define vnl_matlab_print_scalar_h_
/*
  fsm
*/

//:
// \file

#include <vcl_iosfwd.h>
#include <vcl_complex.h>
#include <vnl/vnl_matlab_print_format.h>

//: print real or complex scalar into character buffer.
#define vnl_matlab_print_scalar_declare(T) \
void vnl_matlab_print_scalar(T v, \
                             char *buf, \
                             vnl_matlab_print_format =vnl_matlab_print_format_default)

// Even with a function template we would have to
// forward declare all the specializations anyway.
vnl_matlab_print_scalar_declare(int);
vnl_matlab_print_scalar_declare(unsigned int);
vnl_matlab_print_scalar_declare(float);
vnl_matlab_print_scalar_declare(double);
vnl_matlab_print_scalar_declare(long double);
vnl_matlab_print_scalar_declare(vcl_complex<float>);
vnl_matlab_print_scalar_declare(vcl_complex<double>);
vnl_matlab_print_scalar_declare(vcl_complex<long double>);

//: print scalar to vcl_ostream.
export template <class T>
vcl_ostream &vnl_matlab_print_scalar(vcl_ostream &,
                                     T value,
                                     vnl_matlab_print_format =vnl_matlab_print_format_default);

#endif
