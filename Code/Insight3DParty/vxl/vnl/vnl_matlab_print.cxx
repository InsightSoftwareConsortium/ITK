/*
  fsm@robots.ox.ac.uk
*/
#ifdef __GNUC__
#pragma implementation
#endif

// Adapted from awf's MatOps class.

#include "vnl_matlab_print.h"

#include <vcl_iostream.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>
#include <vnl/vnl_complex.h>

//--------------------------------------------------------------------------------

template <class T>
vcl_ostream &vnl_matlab_print(vcl_ostream& s, 
			  T const* array, 
			  unsigned length,
			  vnl_matlab_print_format format VCL_DEFAULT_VALUE(vnl_matlab_print_format_default))
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
			  vnl_matlab_print_format format VCL_DEFAULT_VALUE(vnl_matlab_print_format_default))
{
  for (unsigned i=0; i<rows; ++i)
    vnl_matlab_print(s, array[i], cols, format) << vcl_endl;
  return s;
}

template <class T>
vcl_ostream& vnl_matlab_print(vcl_ostream& s, 
			  vnl_diag_matrix<T> const& D, 
			  char const* variable_name,
			  vnl_matlab_print_format format VCL_DEFAULT_VALUE(vnl_matlab_print_format_default))
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
			  vnl_matlab_print_format format VCL_DEFAULT_VALUE(vnl_matlab_print_format_default))
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
			  vnl_matlab_print_format format VCL_DEFAULT_VALUE(vnl_matlab_print_format_default))
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

// -- Can be used within debugger to print matrix
extern "C"
void vnl_dbprintmx(vnl_matrix<double> const& p)
{
#if defined(VCL_GCC_27)
  // a mysterious error :
  //vnl_matlab.cxx: In function `void dbprintmx(const class vnl_matrix<double> &)':
  //336: call of overloaded `vnl_matlab_print' is ambiguous
  //216: candidates are: vnl_matlab_print(ostream &, const vnl_matrix<double> &, const char *)
  //                     vnl_matlab_print(ostream &, const vnl_matrix<double> &, const char *)
  vcl_cerr << "[" << vcl_endl << p << "]" << vcl_endl;
  abort();
#else
  // why the cast? is it a const_cast?
  vnl_matlab_print(vcl_cerr, p, (char const*)"M", vnl_matlab_print_format_default);
#endif
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
inst(vnl_float_complex);
inst(vnl_double_complex);
#undef inst
