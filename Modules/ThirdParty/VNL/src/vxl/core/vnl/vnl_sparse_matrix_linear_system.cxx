// This is core/vnl/vnl_sparse_matrix_linear_system.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif

#include "vnl_sparse_matrix_linear_system.h"
#include <vcl_cassert.h>
#include <vnl/vnl_copy.h>

template <>
void vnl_sparse_matrix_linear_system<double>::get_rhs(vnl_vector<double>& b) const
{
  b = b_;
}

template <>
void vnl_sparse_matrix_linear_system<double>::transpose_multiply(vnl_vector<double> const& b, vnl_vector<double> & x) const
{
  A_.pre_mult(b,x);
}

template <>
void vnl_sparse_matrix_linear_system<float>::get_rhs(vnl_vector<double>& b) const
{
   vnl_copy(b_, b);
}

template <>
void vnl_sparse_matrix_linear_system<float>::transpose_multiply(vnl_vector<double> const& b, vnl_vector<double> & x) const
{
  static vnl_vector<float> x_float;
  static vnl_vector<float> b_float;

  if (x_float.size() != x.size()) x_float = vnl_vector<float> (x.size());
  if (b_float.size() != b.size()) b_float = vnl_vector<float> (b.size());

  vnl_copy(b, b_float);
  A_.pre_mult(b_float,x_float);
  vnl_copy(x_float, x);
}

template <>
void vnl_sparse_matrix_linear_system<double>::multiply(vnl_vector<double> const& x, vnl_vector<double> & b) const
{
  A_.mult(x,b);
}


template <>
void vnl_sparse_matrix_linear_system<float>::multiply(vnl_vector<double> const& x, vnl_vector<double> & b) const
{
  static vnl_vector<float> x_float;
  static vnl_vector<float> b_float;

  if (x_float.size() != x.size()) x_float = vnl_vector<float> (x.size());
  if (b_float.size() != b.size()) b_float = vnl_vector<float> (b.size());

  vnl_copy(x, x_float);
  A_.mult(x_float,b_float);
  vnl_copy(b_float, b);
}


template<class T>
void vnl_sparse_matrix_linear_system<T>::apply_preconditioner(vnl_vector<double> const& x, vnl_vector<double> & px) const
{
  assert(x.size() == px.size());

  if (jacobi_precond_.size() == 0) {
    vnl_vector<T> tmp(get_number_of_unknowns());
    A_.diag_AtA(tmp);
    const_cast<vnl_vector<double> &>(jacobi_precond_) = vnl_vector<double> (tmp.size());
    for (unsigned int i=0; i < tmp.size(); ++i)
      const_cast<vnl_vector<double> &>(jacobi_precond_)[i] = 1.0 / double(tmp[i]);
  }

  px = dot_product(x,jacobi_precond_);
}

template class VNL_EXPORT vnl_sparse_matrix_linear_system<double>;
template class VNL_EXPORT vnl_sparse_matrix_linear_system<float>;

