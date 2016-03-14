// This is core/vnl/vnl_matlab_filewrite.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include <sstream>
#include <iostream>
#include <complex>
#include "vnl_matlab_filewrite.h"

#include <vcl_compiler.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matlab_write.h>

vnl_matlab_filewrite::vnl_matlab_filewrite(char const *file_name,
                                           char const *basename)
  : basename_(basename ? basename : "targetvar"), variable_int_(0)
{
  out_.open(file_name, std::ios::out | std::ios::binary);
  if (out_.bad())
    std::cerr << __FILE__ << ':' << __LINE__ << ", WARNING : output stream is bad\n";
}

std::string vnl_matlab_filewrite::make_var_name(char const* variable_name)
{
  if (variable_name)
    return std::string(variable_name);
  else {
    std::stringstream ss;
    ss << variable_int_++;
    return basename_ + ss.str();
  }
}

//--------------------------------------------------------------------------------

//: scalar
void vnl_matlab_filewrite::write(double v, char const* variable_name)
{
  vnl_matlab_write(out_, v, make_var_name(variable_name).c_str());
}

//: vector
void vnl_matlab_filewrite::write(vnl_vector<double> const& v, char const* variable_name)
{
  vnl_matlab_write(out_, v.data_block(), v.size(), make_var_name(variable_name).c_str());
}

void vnl_matlab_filewrite::write(vnl_vector<std::complex<double> > const& v, char const* variable_name)
{
  vnl_matlab_write(out_, v.data_block(), v.size(), make_var_name(variable_name).c_str());
}

//: matrix
void vnl_matlab_filewrite::write(vnl_matrix<float> const& M, char const* variable_name)
{
  vnl_matlab_write(out_, M.data_array(), M.rows(), M.cols(), make_var_name(variable_name).c_str());
}

void vnl_matlab_filewrite::write(vnl_matrix<double> const& M, char const* variable_name)
{
  vnl_matlab_write(out_, M.data_array(), M.rows(), M.cols(), make_var_name(variable_name).c_str());
}

void vnl_matlab_filewrite::write(vnl_matrix<std::complex<float> > const& M, char const* variable_name)
{
  vnl_matlab_write(out_, M.data_array(), M.rows(), M.cols(), make_var_name(variable_name).c_str());
}

void vnl_matlab_filewrite::write(vnl_matrix<std::complex<double> > const& M, char const* variable_name)
{
  vnl_matlab_write(out_, M.data_array(), M.rows(), M.cols(), make_var_name(variable_name).c_str());
}

void vnl_matlab_filewrite::write(double const * const *M, int rows, int cols, char const* variable_name)
{
  vnl_matlab_write(out_, M, rows, cols, make_var_name(variable_name).c_str());
}
