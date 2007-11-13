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

// Use implicit instatiation of the exp functions, triggered by the
// explicit instantiation of the helper, because explicit
// instantiation of the exp functions causes an internal compiler
// error on VC6.
#undef VNL_MATRIX_EXP_INSTANTIATE
#define VNL_MATRIX_EXP_INSTANTIATE(inpT)                        \
template<class T>                                               \
void vnl_matrix_exp_instantiation_helper( T* )                  \
{                                                               \
  vnl_matrix<T> dT;                                             \
  vnl_matrix_exp( dT, dT, 1.0 );                                \
  vnl_matrix_exp( dT );                                         \
                                                                \
  vnl_matrix_fixed<T,1,1> fT1;                                  \
  vnl_matrix_exp( fT1, fT1, 1.0 );                              \
  vnl_matrix_exp( fT1 );                                        \
                                                                \
  vnl_matrix_fixed<T,2,2> fT2;                                  \
  vnl_matrix_exp( fT2, fT2, 1.0 );                              \
  vnl_matrix_exp( fT2 );                                        \
                                                                \
  vnl_matrix_fixed<T,3,3> fT3;                                  \
  vnl_matrix_exp( fT3, fT3, 1.0 );                              \
  vnl_matrix_exp( fT3 );                                        \
                                                                \
  vnl_matrix_fixed<T,4,4> fT4;                                  \
  vnl_matrix_exp( fT4, fT4, 1.0 );                              \
  vnl_matrix_exp( fT4 );                                        \
}                                                               \
template void vnl_matrix_exp_instantiation_helper( inpT* )

#endif
