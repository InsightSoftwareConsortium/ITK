// This is vxl/vnl/vnl_matlab_print.cxx

/*
  fsm@robots.ox.ac.uk
*/
#ifdef __GNUC__
#pragma implementation
#endif

// Adapted from awf's MatOps class.

#include "vnl_matlab_print.h"

#include <vcl_iostream.h>
#include <vcl_complex.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>
#include <vnl/vnl_matlab_print_scalar.h>

//--------------------------------------------------------------------------------

template <class T>
vcl_ostream &vnl_matlab_print(vcl_ostream& s,
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
vcl_ostream &vnl_matlab_print(vcl_ostream &s,
                          T const * const *array,
                          unsigned rows, unsigned cols,
                          vnl_matlab_print_format format)
{
  for (unsigned i=0; i<rows; ++i)
    vnl_matlab_print(s, array[i], cols, format) << vcl_endl;
  return s;
}

template <class T>
vcl_ostream& vnl_matlab_print(vcl_ostream& s,
                          vnl_diag_matrix<T> const& D,
                          char const* variable_name,
                          vnl_matlab_print_format format)
{
  if (variable_name)
    s << variable_name << " = diag([ ";

  vnl_matlab_print(s, D.begin(), D.size(), format);

  if (variable_name) {
    s << " ])";
    s << vcl_endl;
  }

  return s;
}

template <class T>
vcl_ostream& vnl_matlab_print(vcl_ostream& s,
                          vnl_matrix<T> const& M,
                          char const* variable_name,
                          vnl_matlab_print_format format)
{
  if (variable_name)
    s << variable_name << " = [ ... \n";

  if (M.rows() == 0)
    return s << "];" << vcl_endl;

  for (unsigned int i=0; i<M.rows(); i++ ) {
    vnl_matlab_print(s, M[i], M.cols(), format);

    if (variable_name && (i == M.rows()-1))
      s << " ]";

    s << vcl_endl;
  }

  return s;
}

template <class T>
vcl_ostream& vnl_matlab_print(vcl_ostream& s,
                          vnl_vector<T> const & v,
                          char const* variable_name,
                          vnl_matlab_print_format format)
{
  if (variable_name)
    s << variable_name << " = [ ";

  vnl_matlab_print(s, v.begin(), v.size(), format);

  if (variable_name) {
    s << " ]";
    s << vcl_endl;
  }

  return s;
}

//--------------------------------------------------------------------------------

//: Can be used within debugger to print matrix
extern "C"
void vnl_dbprintmx(vnl_matrix<double> const& p)
{
  // why the cast? is it a const_cast?
  vnl_matlab_print(vcl_cerr, p, (char const*)"M", vnl_matlab_print_format_default);
}

//--------------------------------------------------------------------------------

#define inst(T) \
template vcl_ostream &vnl_matlab_print(vcl_ostream &, T const *, unsigned, vnl_matlab_print_format); \
template vcl_ostream &vnl_matlab_print(vcl_ostream &, T const * const *, unsigned, unsigned, vnl_matlab_print_format); \
template vcl_ostream &vnl_matlab_print(vcl_ostream &, vnl_diag_matrix<T > const &, char const *, vnl_matlab_print_format); \
template vcl_ostream &vnl_matlab_print(vcl_ostream &, vnl_matrix<T > const &, char const *, vnl_matlab_print_format); \
template vcl_ostream &vnl_matlab_print(vcl_ostream &, vnl_vector<T > const &, char const *, vnl_matlab_print_format);
inst(int);
inst(float);
inst(double);
inst(vcl_complex<float>);
inst(vcl_complex<double>);
#undef inst
