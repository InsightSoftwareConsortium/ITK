#ifndef vnl_matlab_print_scalar_h_
#define vnl_matlab_print_scalar_h_
#ifdef __GNUC__
#pragma interface
#endif
/*
  fsm@robots.ox.ac.uk
*/

#include <vcl_complex_fwd.h>
#include <vnl/vnl_matlab_print_format.h>

//: print real or complex scalar into character buffer.
#define vnl_matlab_print_scalar_declare(T) \
void vnl_matlab_print_scalar(T v, \
                             char *buf, \
                             vnl_matlab_print_format =vnl_matlab_print_format_default)

// Even with a function template we would have to
// forward declare all the specializations anyway.
vnl_matlab_print_scalar_declare(int);
vnl_matlab_print_scalar_declare(float);
vnl_matlab_print_scalar_declare(double);
vnl_matlab_print_scalar_declare(long double);
vnl_matlab_print_scalar_declare(vcl_complex<float>);
vnl_matlab_print_scalar_declare(vcl_complex<double>);
vnl_matlab_print_scalar_declare(vcl_complex<long double>);

#endif
