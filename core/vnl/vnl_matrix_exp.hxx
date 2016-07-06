// This is core/vnl/vnl_matrix_exp.hxx
#ifndef vnl_matrix_exp_hxx_
#define vnl_matrix_exp_hxx_
//:
// \file
// \author fsm
#include <iostream>
#include "vnl_matrix_exp.h"
#include <vcl_cassert.h>
#ifdef DEBUG
#include <vcl_compiler.h>
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
  std::cerr << "norm_X = " << norm_X << std::endl;
#endif

  // exponential series
  expX.set_identity();
  Matrix acc(X);
  double norm_acc_bound = norm_X;
  for (unsigned n=1; true; ++n) {
    expX += acc;
#ifdef DEBUG
    std::cerr << "n=" << n << std::endl;
#endif

    if (norm_X < n) {
      double err_bound = norm_acc_bound / (1 - norm_X/n);
#ifdef DEBUG
      std::cerr << "err_bound = " << err_bound << std::endl;
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
template VNL_EXPORT bool vnl_matrix_exp(Matrix const&, Matrix &, double); \
template VNL_EXPORT Matrix vnl_matrix_exp(Matrix const&)

#endif // vnl_matrix_exp_hxx_
