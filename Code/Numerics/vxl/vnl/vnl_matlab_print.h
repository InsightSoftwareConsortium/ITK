#ifndef vnl_matlab_print_h_
#define vnl_matlab_print_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_matlab_print.h

//: \file
//  \brief Print matrices and vectors in nice MATLAB format.
//  \author fsm@robots.ox.ac.uk, from awf's MatOps code.


template <class T> class vnl_vector;
template <class T> class vnl_matrix;
template <class T> class vnl_diag_matrix;
#include <vcl_iosfwd.h>

#include <vnl/vnl_matlab_print_format.h>

// If a variable name (e.g. "foo") is given, the raw data will be preceded by
//   "foo = diag([ " for a vnl_diag_matrix
//   "foo = [ ...\n" for a vnl_matrix and
//   "foo = [ "      for a vnl_vector
// and followed by "])\n", "]\n" and "]\n" respectively. If the variable name
// is a null pointer, the data is printed as is.

//: print a 1D array.
template <class T>
vcl_ostream &vnl_matlab_print(vcl_ostream &,
                          T const *array,
                          unsigned length,
                          vnl_matlab_print_format =vnl_matlab_print_format_default);

//: print a 2D array.
template <class T>
vcl_ostream &vnl_matlab_print(vcl_ostream &,
                          T const * const *array,
                          unsigned rows, unsigned cols,
                          vnl_matlab_print_format =vnl_matlab_print_format_default);

//: print a vnl_diagonal_matrix<>.
template <class T>
vcl_ostream &vnl_matlab_print(vcl_ostream &,
                          vnl_diag_matrix<T> const &,
                          char const *variable_name =0,
                          vnl_matlab_print_format =vnl_matlab_print_format_default);

//: print a vnl_matrix<>.
template <class T>
vcl_ostream &vnl_matlab_print(vcl_ostream &,
                          vnl_matrix<T> const &,
                          char const *variable_name =0,
                          vnl_matlab_print_format =vnl_matlab_print_format_default);

//: print a vnl_vector<>.
template <class T>
vcl_ostream &vnl_matlab_print(vcl_ostream &,
                          vnl_vector<T> const &,
                          char const *variable_name =0,
                          vnl_matlab_print_format =vnl_matlab_print_format_default);


//: naughty naming-convention-defying-but-handy macro.
#define MATLABPRINT(X) (vnl_matlab_print(vcl_cerr, X, #X))

#endif // vnl_matlab_print_h_
