// This is core/vnl/vnl_matlab_print.hxx
// It is different from vnl_matlab_print.cxx
#ifndef vnl_matlab_print_hxx_
#define vnl_matlab_print_hxx_
// \author fsm
// Adapted from awf's MatOps class.

#include <iostream>
#include "vnl_matlab_print.h"

#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_matrix_ref.h>
#include <vnl/vnl_diag_matrix.h>
#include <vnl/vnl_matlab_print_scalar.h>

//--------------------------------------------------------------------------------

template <class T>
std::ostream &vnl_matlab_print(std::ostream& s,
                              T const* array,
                              unsigned length,
                              vnl_matlab_print_format format)
{
  char buf[1024];
  for (unsigned j=0; j<length; j++ ) {
    // Format according to selected style
    // In both cases an exact 0 goes out as such
    vnl_matlab_print_scalar(array[j], buf, format);
    s << buf;
  }

  return s;
}

template <class T>
std::ostream &vnl_matlab_print(std::ostream &s,
                              T const * const *array,
                              unsigned rows, unsigned cols,
                              vnl_matlab_print_format format)
{
  for (unsigned i=0; i<rows; ++i)
    vnl_matlab_print(s, array[i], cols, format) << '\n';
  return s;
}

template <class T>
std::ostream& vnl_matlab_print(std::ostream& s,
                              vnl_diag_matrix<T> const& D,
                              char const* variable_name,
                              vnl_matlab_print_format format)
{
  if (variable_name)
    s << variable_name << " = diag([ ";

  vnl_matlab_print(s, D.begin(), D.size(), format);

  if (variable_name)
    s << " ])\n";

  return s;
}

template <class T>
std::ostream& vnl_matlab_print(std::ostream& s,
                              vnl_matrix<T> const& M,
                              char const* variable_name,
                              vnl_matlab_print_format format)
{
  if (variable_name)
    s << variable_name << " = [ ...\n";

  if (variable_name && M.rows() == 0)
    return s << "];\n";

  for (unsigned int i=0; i<M.rows(); i++ ) {
    vnl_matlab_print(s, M[i], M.cols(), format);

    if (variable_name && (i == M.rows()-1))
      s << " ]";

    s << '\n';
  }

  return s;
}

template <class T>
std::ostream& vnl_matlab_print(std::ostream& s,
                              vnl_vector<T> const & v,
                              char const* variable_name,
                              vnl_matlab_print_format format)
{
  if (variable_name)
    s << variable_name << " = [ ";

  vnl_matlab_print(s, v.begin(), v.size(), format);

  if (variable_name)
    s << " ]\n";

  return s;
}

template <class T, unsigned int n, unsigned int m>
std::ostream& vnl_matlab_print(std::ostream& s,
                              vnl_matrix_fixed<T,n,m> const& M,
                              char const* variable_name,
                              vnl_matlab_print_format format)
{
  if (variable_name)
    s << variable_name << " = [ ...\n";

  if (variable_name && M.rows() == 0)
    return s << "];\n";

  for (unsigned int i=0; i<n; ++i ) {
    vnl_matlab_print(s, M[i], m, format);

    if (variable_name && (i == n-1))
      s << " ]";

    s << '\n';
  }

  return s;
}

template <class T>
std::ostream& vnl_matlab_print(std::ostream& s,
                              vnl_matrix_ref<T> const& M,
                              char const* variable_name,
                              vnl_matlab_print_format format)
{
  if (variable_name)
    s << variable_name << " = [ ...\n";

  if (variable_name && M.rows() == 0)
    return s << "];\n";

  for (unsigned int i=0; i<M.rows(); ++i )
  {
    vnl_matlab_print(s, M[i], M.cols(), format);

    if (variable_name && (i == M.rows()-1))
      s << " ]";

    s << '\n';
  }

  return s;
}

template <class T, unsigned int n>
std::ostream& vnl_matlab_print(std::ostream& s,
                              vnl_vector_fixed<T,n> const & v,
                              char const* variable_name,
                              vnl_matlab_print_format format)
{
  if (variable_name)
    s << variable_name << " = [ ";

  vnl_matlab_print(s, v.begin(), n, format);

  if (variable_name)
    s << " ]\n";

  return s;
}

//--------------------------------------------------------------------------------

#undef  VNL_MATLAB_PRINT_INSTANTIATE
#define VNL_MATLAB_PRINT_INSTANTIATE(T) \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, T const*, unsigned, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, T const* const*, unsigned, unsigned, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_diag_matrix<T > const&, char const *, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix<T > const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_vector<T > const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_ref<T > const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,1,1> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,2,2> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,2,3> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,3,2> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,3,3> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,2,4> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,3,4> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,4,3> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,4,4> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,6,8> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,5,5> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,6,6> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,7,7> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,8,8> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,9,9> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,10,10> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_vector_fixed<T,1> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_vector_fixed<T,2> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_vector_fixed<T,3> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_vector_fixed<T,4> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_vector_fixed<T,5> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_vector_fixed<T,6> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_vector_fixed<T,7> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_vector_fixed<T,8> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_vector_fixed<T,9> const&, char const*, vnl_matlab_print_format); \
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_vector_fixed<T,10> const&, char const*, vnl_matlab_print_format)

#endif // vnl_matlab_print_hxx_
