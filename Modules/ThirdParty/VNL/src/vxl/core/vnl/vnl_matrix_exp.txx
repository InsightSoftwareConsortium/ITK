// This is core/vnl/vnl_matrix_exp.txx
#ifndef vnl_matrix_exp_txx_
#define vnl_matrix_exp_txx_
//:
// \file
// \author fsm
#include "vnl_matrix_exp.h"
#include <vcl_cassert.h>
#ifdef DEBUG
#include <vcl_iostream.h>
#endif

template <class Matrix>
bool vnl_matrix_exp(Matrix const &X, Matrix &expX, double max_err)
{
  assert(X.rows() == X.cols());
  assert(X.rows() == expX.rows());
  assert(X.cols() == expX.cols());
  assert(max_err > 0);

  double norm_X = X.operator_inf_norm();
#ifdef DEBUG
  vcl_cerr << "norm_X = " << norm_X << vcl_endl;
#endif

  // exponential series
  expX.set_identity();
  Matrix acc(X);
  double norm_acc_bound = norm_X;
  for (unsigned n=1; true; ++n) {
    expX += acc;
#ifdef DEBUG
    vcl_cerr << "n=" << n << vcl_endl;
#endif

    if (norm_X < n) {
      double err_bound = norm_acc_bound / (1 - norm_X/n);
#ifdef DEBUG
      vcl_cerr << "err_bound = " << err_bound << vcl_endl;
#endif
      if (err_bound < max_err)
        break;
    }

    acc = acc * X;
    acc /= n+1;

    norm_acc_bound *= norm_X/(n+1);
  }

  return true;
}


template <class Matrix>
Matrix vnl_matrix_exp(Matrix const &X)
{
  Matrix expX(X.rows(), X.cols());
#ifndef NDEBUG
  bool retval =
#endif
  vnl_matrix_exp(X, expX, 1e-10);

  assert(retval);
  return expX;
}

//------------------------------------------------------------------------------

#undef VNL_MATRIX_EXP_INSTANTIATE
#define VNL_MATRIX_EXP_INSTANTIATE(Matrix) \
template bool vnl_matrix_exp(Matrix const&, Matrix &, double); \
template Matrix vnl_matrix_exp(Matrix const&)

#endif // vnl_matrix_exp_txx_
