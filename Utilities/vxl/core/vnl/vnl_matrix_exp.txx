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

template <class Matrix>
bool vnl_matrix_exp_helper(Matrix const &X, Matrix &expX, double max_err)
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

template <class T>
bool vnl_matrix_exp(vnl_matrix<T> const &X, vnl_matrix<T> &expX, double max_err)
{
  return vnl_matrix_exp_helper( X, expX, max_err );
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

#ifndef VCL_VC_60

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
  return vnl_matrix_exp_helper( X, expX, max_err );
}

#else // if is VCL_VC_60

template <class T, unsigned int n, unsigned int m>
vnl_matrix_fixed<T,n,m> vnl_matrix_exp(vnl_matrix_fixed<T,n,m> const &X)
{
  vnl_matrix_fixed<T,n,m> expX;
  vnl_matrix_exp(X, expX, 1e-10);
  return expX;
}

template <class T, unsigned int n, unsigned int m>
bool vnl_matrix_exp(vnl_matrix_fixed<T,n,m> const &X, vnl_matrix_fixed<T,n,m> &expX, double max_err)
{
  return vnl_matrix_exp_helper( X, expX, max_err );
}

#endif // VCL_VC_60

//--------------------------------------------------------------------------------

#undef VNL_MATRIX_EXP_INSTANTIATE_MATRIX
#define VNL_MATRIX_EXP_INSTANTIATE_MATRIX(T) \
template bool vnl_matrix_exp(vnl_matrix<T > const&, vnl_matrix<T >&, double); \
template vnl_matrix<T > vnl_matrix_exp(vnl_matrix<T > const&);

#undef VNL_MATRIX_EXP_INSTANTIATE_FIXED
#define VNL_MATRIX_EXP_INSTANTIATE_FIXED(T) \
template bool vnl_matrix_exp(vnl_matrix_fixed<T,1,1> const&, vnl_matrix_fixed<T,1,1>&, double); \
template vnl_matrix_fixed<T,1,1> vnl_matrix_exp(vnl_matrix_fixed<T,1,1> const&);\
template bool vnl_matrix_exp(vnl_matrix_fixed<T,2,2> const&, vnl_matrix_fixed<T,2,2>&, double); \
template vnl_matrix_fixed<T,2,2> vnl_matrix_exp(vnl_matrix_fixed<T,2,2> const&);\
template bool vnl_matrix_exp(vnl_matrix_fixed<T,3,3> const&, vnl_matrix_fixed<T,3,3>&, double); \
template vnl_matrix_fixed<T,3,3> vnl_matrix_exp(vnl_matrix_fixed<T,3,3> const&);\
template bool vnl_matrix_exp(vnl_matrix_fixed<T,4,4> const&, vnl_matrix_fixed<T,4,4>&, double); \
template vnl_matrix_fixed<T,4,4> vnl_matrix_exp(vnl_matrix_fixed<T,4,4> const&)


#undef VNL_MATRIX_EXP_INSTANTIATE
#define VNL_MATRIX_EXP_INSTANTIATE(T) \
  VNL_MATRIX_EXP_INSTANTIATE_MATRIX(T) \
  VNL_MATRIX_EXP_INSTANTIATE_FIXED(T)

#endif
