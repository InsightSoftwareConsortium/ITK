#ifndef vnl_matrix_exp_txx_
#define vnl_matrix_exp_txx_
// This is vxl/vnl/vnl_matrix_exp.txx

/*
  fsm@robots.ox.ac.uk
*/
#include "vnl_matrix_exp.h"
#include <vcl_cassert.h>
#include <vcl_cmath.h>
//#include <vcl_iostream.h>

template <class T>
bool vnl_matrix_exp(vnl_matrix<T> const &X, vnl_matrix<T> &expX, double max_err)
{
  unsigned N = X.rows();
  X.assert_size(N, N);
  expX.assert_size(N, N);

  double norm_X = X.operator_inf_norm();
  //vcl_cerr << "norm_X = " << norm_X << vcl_endl;

  // exponential series
  expX.set_identity();
  vnl_matrix<T> acc(X);
  double norm_acc_bound = norm_X;
  for (unsigned n=1; ; ++n) {
    expX += acc;
    //vcl_cerr << "n=" << n << vcl_endl;

    if (norm_X < n) {
      double err_bound = norm_acc_bound / (1 - norm_X/n);
      //vcl_cerr << "err_bound = " << err_bound << vcl_endl;
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
  assert(vnl_matrix_exp(X, expX, 1e-10));
  return expX;
}

//--------------------------------------------------------------------------------

#undef VNL_MATRIX_EXP_INSTANTIATE
#define VNL_MATRIX_EXP_INSTANTIATE(T) \
template bool vnl_matrix_exp(vnl_matrix<T > const &, vnl_matrix<T > &, double); \
template vnl_matrix<T > vnl_matrix_exp(vnl_matrix<T > const &)

#endif
