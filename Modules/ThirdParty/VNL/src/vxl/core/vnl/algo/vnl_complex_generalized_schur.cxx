// This is core/vnl/algo/vnl_complex_generalized_schur.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author fsm

#include <iostream>
#include "vnl_complex_generalized_schur.h"

#include <vcl_compiler.h>
#include <vcl_cassert.h>

#include <vnl/vnl_vector.h>

#include <vnl/algo/vnl_netlib.h> // zgges_()

template <>
bool vnl_generalized_schur(vnl_matrix<std::complex<double> > *A,
                           vnl_matrix<std::complex<double> > *B,
                           vnl_vector<std::complex<double> > *alpha,
                           vnl_vector<std::complex<double> > *beta,
                           vnl_matrix<std::complex<double> > *L,
                           vnl_matrix<std::complex<double> > *R)
{
  // Both input matrices should be square and of the same size:
  assert(A->rows() == A->cols());
  assert(A->cols() == B->rows());
  assert(B->rows() == B->cols());

  long n = A->rows();
  assert(alpha!=0); alpha->set_size(n);    alpha->fill(0);
  assert(beta!=0);  beta ->set_size(n);    beta ->fill(0);
  assert(L!=0);     L    ->set_size(n, n); L    ->fill(0);
  assert(R!=0);     R    ->set_size(n, n); R    ->fill(0);

  long sdim = 0;
  long lwork = 1000 + (8*n + 16);
  std::complex<double> *work = new std::complex<double>[lwork];
  double *rwork = new double[2*n + 1];
  v3p_netlib_logical *bwork = new v3p_netlib_logical[n + 1];
  long info = 0;
  A->inplace_transpose();
  B->inplace_transpose();
  v3p_netlib_zgges_ ("V", "V",
                     "N",
                     VXL_NULLPTR,
                     &n,
                     A->data_block(), &n,
                     B->data_block(), &n,
                     &sdim,
                     alpha->data_block(),
                     beta->data_block(),
                     L->data_block(), &n,
                     R->data_block(), &n,
                     &work[0], &lwork,
                     &rwork[0], &bwork[0],
                     &info, 1, 1, 1);
  A->inplace_transpose();
  B->inplace_transpose();
  L->inplace_transpose();
  R->inplace_transpose();
  delete [] work;
  delete [] bwork;
  delete [] rwork;

  if (info == 0) {
    // ok
    return true;
  }
  else
  {
    // These return codes are taken from zgges.f:
    //*          = 0:  successful exit
    //*          < 0:  if INFO = -i, the i-th argument had an illegal value.
    //*          =1,...,N:
    //*                The QZ iteration failed.  (A,B) are not in Schur
    //*                form, but ALPHA(j) and BETA(j) should be correct for
    //*                j=INFO+1,...,N.
    //*          > N:  =N+1: other than QZ iteration failed in ZHGEQZ
    //*                =N+2: after reordering, roundoff changed values of
    //*                      some complex eigenvalues so that leading
    //*                      eigenvalues in the Generalized Schur form no
    //*                      longer satisfy SELCTG=.TRUE.  This could also
    //*                      be caused due to scaling.
    //*                =N+3: reordering falied in ZTGSEN.
    std::cerr << __FILE__ ": info = " << info << ", something went wrong:\n";
    if (info < 0) {
      std::cerr << __FILE__ ": (internal error) the " << (-info) << "th argument had an illegal value\n";
    }
    else if (1 <= info && info <= n) {
      std::cerr << __FILE__ ": the QZ iteration failed, but the last " << (n - info) << " eigenvalues may be correct\n";
    }
    else if (info == n+1) {
      std::cerr << __FILE__ ": something went wrong in ZHGEQZ\n";
    }
    else if (info == n+2) {
      std::cerr << __FILE__ ": roundoff error -- maybe due to poor scaling\n";
    }
    else if (info == n+3) {
      std::cerr << __FILE__ ": reordering failed in ZTGSEN\n";
    }
    else {
      std::cerr << __FILE__ ": unknown error\n";
    }
    return false;
  }
}
