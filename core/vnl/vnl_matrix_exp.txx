// This is core/vnl/vnl_matrix_exp.txx
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

template <class T, unsigned int n>
vnl_matrix_fixed<T,n,n> vnl_matrix_exp(vnl_matrix_fixed<T,n,n> const &X)
{
  vnl_matrix_fixed<T,n,n> expX;
  vnl_matrix_exp(X, expX, 1e-10);
  return expX;
}

template <class T, unsigned int n>
bool vnl_matrix_exp(vnl_matrix_fixed<T,n,n> const &X, vnl_matrix_fixed<T,n,n> &expX, double max_err)
{
  assert(max_err > 0);
  double norm_X = X.operator_inf_norm();
#ifdef DEBUG
  vcl_cerr << "norm_X = " << norm_X << vcl_endl;
#endif

  // exponential series
  expX.set_identity();
  vnl_matrix_fixed<T,n,n> acc = X;
  double norm_acc_bound = norm_X;
  for (unsigned m=1; true; ++m)
  {
    expX += acc;
#ifdef DEBUG
    vcl_cerr << "n=" << m << vcl_endl;
#endif

    if (norm_X < m) {
      double err_bound = norm_acc_bound / (1 - norm_X/m);
#ifdef DEBUG
      vcl_cerr << "err_bound = " << err_bound << vcl_endl;
#endif
      if (err_bound < max_err)
        return true;
    }

    acc = acc * X;
    acc /= m+1;

    norm_acc_bound *= norm_X/(m+1);
  }
  assert(!"This should never happen...");
  return false; // This line is never reached
}

//--------------------------------------------------------------------------------

#undef VNL_MATRIX_EXP_INSTANTIATE
#define VNL_MATRIX_EXP_INSTANTIATE(T) \
template bool vnl_matrix_exp(vnl_matrix<T > const&, vnl_matrix<T >&, double); \
template vnl_matrix<T > vnl_matrix_exp(vnl_matrix<T > const&);\
template bool vnl_matrix_exp(vnl_matrix_fixed<T,1,1> const&, vnl_matrix_fixed<T,1,1>&, double); \
template vnl_matrix_fixed<T,1,1> vnl_matrix_exp(vnl_matrix_fixed<T,1,1> const&);\
template bool vnl_matrix_exp(vnl_matrix_fixed<T,2,2> const&, vnl_matrix_fixed<T,2,2>&, double); \
template vnl_matrix_fixed<T,2,2> vnl_matrix_exp(vnl_matrix_fixed<T,2,2> const&);\
template bool vnl_matrix_exp(vnl_matrix_fixed<T,3,3> const&, vnl_matrix_fixed<T,3,3>&, double); \
template vnl_matrix_fixed<T,3,3> vnl_matrix_exp(vnl_matrix_fixed<T,3,3> const&);\
template bool vnl_matrix_exp(vnl_matrix_fixed<T,4,4> const&, vnl_matrix_fixed<T,4,4>&, double); \
template vnl_matrix_fixed<T,4,4> vnl_matrix_exp(vnl_matrix_fixed<T,4,4> const&)

#endif
