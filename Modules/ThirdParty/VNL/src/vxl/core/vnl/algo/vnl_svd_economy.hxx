// This is core/vnl/algo/vnl_svd_economy.hxx
#ifndef vnl_svd_economy_hxx_
#define vnl_svd_economy_hxx_

#include <iostream>
#include <algorithm>
#include <cmath>
#include "vnl_svd_economy.h"

#include <vcl_compiler.h>

#include <vnl/vnl_fortran_copy.h>
#include <vnl/algo/vnl_netlib.h> // dsvdc_()
#include <vnl/vnl_matlab_print.h>

#define macro(p, T) \
inline void vnl_linpack_svdc_economy(vnl_netlib_svd_proto(T)) \
{ v3p_netlib_##p##svdc_(vnl_netlib_svd_params); }
macro(s, float);
macro(d, double);
macro(c, std::complex<float>);
macro(z, std::complex<double>);
#undef macro

template <class real_t>
vnl_svd_economy<real_t>::vnl_svd_economy( vnl_matrix<real_t> const& M ) :
  m_(M.rows()), n_(M.columns()),
  V_(n_,n_),
  sv_(n_)
{
  vnl_fortran_copy<real_t> X(M);

  int mm = std::min(m_+1L,n_);

  // Make workspace vectors.
  vnl_vector<real_t> work(m_, real_t(0));
  vnl_vector<real_t> vspace(n_*n_, real_t(0));
  vnl_vector<real_t> wspace(mm, real_t(0)); // complex fortran routine actually _wants_ complex W!
  vnl_vector<real_t> espace(n_, real_t(0));

  // Call Linpack SVD
  long ldu = 0;
  long info = 0;
  const long job = 01; // no U, n svs in V (i.e. super-economy size)
  vnl_linpack_svdc_economy((real_t*)X, &m_, &m_, &n_,
                           wspace.data_block(),
                           espace.data_block(),
                           0, &ldu,
                           vspace.data_block(), &n_,
                           work.data_block(),
                           &job, &info);

  // Error return?
  if (info != 0)
  {
    // If info is non-zero, it contains the number of singular values
    // for this the SVD algorithm failed to converge. The condition is
    // not bogus. Even if the returned singular values are sensible,
    // the singular vectors can be utterly wrong.

    // It is possible the failure was due to NaNs or infinities in the
    // matrix. Check for that now.
    M.assert_finite();

    // If we get here it might be because
    // 1. The scalar type has such
    // extreme precision that too few iterations were performed to
    // converge to within machine precision (that is the svdc criterion).
    // One solution to that is to increase the maximum number of
    // iterations in the netlib code.
    //
    // 2. The LINPACK dsvdc_ code expects correct IEEE rounding behaviour,
    // which some platforms (notably x86 processors)
    // have trouble doing. For example, gcc can output
    // code in -O2 and static-linked code that causes this problem.
    // One solution to this is to persuade gcc to output slightly different code
    // by adding and -fPIC option to the command line for v3p\netlib\dsvdc.c. If
    // that doesn't work try adding -ffloat-store, which should fix the problem
    // at the expense of being significantly slower for big problems. Note that
    // if this is the cause, core/vnl/tests/test_svd should have failed.
    //
    // You may be able to diagnose the problem here by printing a warning message.
    std::cerr << __FILE__ ": suspicious return value (" << info << ") from SVDC\n"
             << __FILE__ ": M is " << M.rows() << 'x' << M.cols() << std::endl;

    vnl_matlab_print(std::cerr, M, "M", vnl_matlab_print_format_long);
    //    valid_ = false;
  }

  for (int j = 0; j < mm; ++j)
    sv_[j] = std::abs(wspace(j)); // we get rid of complexness here.

  for (int j = mm; j < n_; ++j)
    sv_[j] = 0;

  {
    const real_t *d = vspace.data_block();
    for (int j = 0; j < n_; ++j)
      for (int i = 0; i < n_; ++i)
        V_[i][j] = *(d++);
  }
}

template <class real_t>
vnl_vector<real_t>
vnl_svd_economy<real_t>::nullvector()
{
  return V_.get_column( n_ - 1 );
}


#undef VNL_SVD_ECONOMY_INSTANTIATE
#define VNL_SVD_ECONOMY_INSTANTIATE(T) \
template class VNL_ALGO_EXPORT vnl_svd_economy<T >

#endif // vnl_svd_economy_hxx_
