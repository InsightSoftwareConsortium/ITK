// This is vxl/vnl/vnl_matrix_exp.txx
#ifndef vnl_matrix_exp_txx_
#define vnl_matrix_exp_txx_
/*
  fsm
*/
#include "vnl_matrix_exp.h"
#include <vcl_cassert.h>
#ifdef DEBUG
#include <vcl_iostream.h>
#endif

template <class T>
bool vnl_matrix_exp(vnl_matrix<T> const &X, vnl_matrix<T> &expX, double max_err)
{
  unsigned N = X.rows();
  X.assert_size(N, N);
  expX.assert_size(N, N);

  double norm_X = X.operator_inf_norm();
#ifdef DEBUG
  vcl_cerr << "norm_X = " << norm_X << vcl_endl;
#endif

  // exponential series
  expX.set_identity();
  vnl_matrix<T> acc(X);
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

template <class T>
vnl_matrix<T> vnl_matrix_exp(vnl_matrix<T> const &X)
{
  vnl_matrix<T> expX(X.rows(), X.cols());
#ifndef NDEBUG
  bool retval = 
#endif
  vnl_matrix_exp(X, expX, 1e-10);

  assert(retval);
  return expX;
}

//--------------------------------------------------------------------------------

#undef VNL_MATRIX_EXP_INSTANTIATE
#define VNL_MATRIX_EXP_INSTANTIATE(T) \
template bool vnl_matrix_exp(vnl_matrix<T > const &, vnl_matrix<T > &, double); \
template vnl_matrix<T > vnl_matrix_exp(vnl_matrix<T > const &)

#endif
