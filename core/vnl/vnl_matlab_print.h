// This is core/vnl/vnl_matlab_print.h
#ifndef vnl_matlab_print_h_
#define vnl_matlab_print_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief Print matrices and vectors in nice MATLAB format.
//  \author fsm, from awf's MatOps code.

#include <iosfwd>
#include <vcl_compiler.h>
#include <vnl/vnl_fwd.h>

#include <vnl/vnl_matlab_print_format.h>
#include "vnl/vnl_export.h"

// If a variable name (e.g. "foo") is given, the raw data will be preceded by
//   "foo = diag([ " for a vnl_diag_matrix
//   "foo = [ ...\n" for a vnl_matrix and
//   "foo = [ "      for a vnl_vector
// and followed by "])\n", "]\n" and "]\n" respectively. If the variable name
// is a null pointer, the data is printed as is.

//-------------------- "unnamed" forms.

//: print a 1D array.
template <class T> VNL_TEMPLATE_EXPORT
std::ostream &vnl_matlab_print(std::ostream &,
                              T const *array,
                              unsigned length,
                              vnl_matlab_print_format =vnl_matlab_print_format_default);

//: print a 2D array.
template <class T> VNL_TEMPLATE_EXPORT
std::ostream &vnl_matlab_print(std::ostream &,
                              T const * const *array,
                              unsigned rows, unsigned cols,
                              vnl_matlab_print_format =vnl_matlab_print_format_default);

//-------------------- "named" forms.

//: print a vnl_diagonal_matrix<T>.
//  \relatesalso vnl_diag_matrix
template <class T> VNL_TEMPLATE_EXPORT
std::ostream &vnl_matlab_print(std::ostream &,
                              vnl_diag_matrix<T> const&,
                              char const *variable_name =0,
                              vnl_matlab_print_format =vnl_matlab_print_format_default);

//: print a vnl_matrix<T>.
//  \relatesalso vnl_matrix
template <class T> VNL_TEMPLATE_EXPORT
std::ostream &vnl_matlab_print(std::ostream &,
                              vnl_matrix<T> const&,
                              char const *variable_name =0,
                              vnl_matlab_print_format =vnl_matlab_print_format_default);

//: print a vnl_matrix_fixed<T>.
//  \relatesalso vnl_matrix_fixed
template <class T, unsigned int n, unsigned int m> VNL_TEMPLATE_EXPORT
std::ostream &vnl_matlab_print(std::ostream &,
                              vnl_matrix_fixed<T,n,m> const&,
                              char const *variable_name =0,
                              vnl_matlab_print_format =vnl_matlab_print_format_default);

//: print a vnl_matrix_ref<T>.
//  \relatesalso vnl_matrix_ref
template <class T> VNL_TEMPLATE_EXPORT
std::ostream &vnl_matlab_print(std::ostream &,
                              vnl_matrix_ref<T> const &,
                              char const *variable_name =0,
                              vnl_matlab_print_format =vnl_matlab_print_format_default);

//: print a vnl_vector<T>.
//  \relatesalso vnl_vector
template <class T> VNL_TEMPLATE_EXPORT
std::ostream &vnl_matlab_print(std::ostream &,
                              vnl_vector<T> const &,
                              char const *variable_name =0,
                              vnl_matlab_print_format =vnl_matlab_print_format_default);

//: print a vnl_vector_fixed<T>.
//  \relatesalso vnl_vector_fixed
template <class T, unsigned int n> VNL_TEMPLATE_EXPORT
std::ostream &vnl_matlab_print(std::ostream &,
                              vnl_vector_fixed<T,n> const &,
                              char const *variable_name =0,
                              vnl_matlab_print_format =vnl_matlab_print_format_default);


//: naughty naming-convention-defying-but-handy macro.
#define MATLABPRINT(X) (vnl_matlab_print(std::cerr, (X).as_ref(), #X))

#endif // vnl_matlab_print_h_
