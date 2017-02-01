// This is core/vnl/algo/vnl_generalized_schur.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author fsm

#include <iostream>
#include "vnl_generalized_schur.h"

#include <vcl_compiler.h>
#include <vcl_cassert.h>

#include <vnl/vnl_vector.h>

#include <vnl/algo/vnl_netlib.h> // dgges_()

template <>
bool vnl_generalized_schur(vnl_matrix<double> *A,
                           vnl_matrix<double> *B,
                           vnl_vector<double> *alphar,
                           vnl_vector<double> *alphai,
                           vnl_vector<double> *beta,
                           vnl_matrix<double> *L,
                           vnl_matrix<double> *R)
{
  assert(A->cols() == A->cols());
  assert(A->cols() == B->rows());
  assert(A->cols() == B->cols());

  long n = A->rows();
  assert(alphar!=0); alphar->set_size(n);    alphar->fill(0);
  assert(alphai!=0); alphai->set_size(n);    alphai->fill(0);
  assert(beta!=0);   beta  ->set_size(n);    beta  ->fill(0);
  assert(L!=0);      L     ->set_size(n, n); L     ->fill(0);
  assert(R!=0);      R     ->set_size(n, n); R     ->fill(0);

  long sdim = 0;
  long lwork = 1000 + (8*n + 16);
  double *work = new double[lwork];
  long info = 0;
  A->inplace_transpose();
  B->inplace_transpose();
  v3p_netlib_dgges_ ("V", "V",
                     "N",
                     VXL_NULLPTR,
                     &n,
                     A->data_block(), &n,
                     B->data_block(), &n,
                     &sdim,
                     alphar->data_block(),
                     alphai->data_block(),
                     beta->data_block(),
                     L->data_block(), &n,
                     R->data_block(), &n,
                     &work[0], &lwork,
                     VXL_NULLPTR,
                     &info, 1, 1, 1);
  A->inplace_transpose();
  B->inplace_transpose();
  L->inplace_transpose();
  R->inplace_transpose();
  delete [] work;

  if (info == 0) {
    // ok
    return true;
  }
  else
  {
    // These return codes are taken from dgges.f:
    //*          = 0:  successful exit
    //*          < 0:  if INFO = -i, the i-th argument had an illegal value.
    //*          = 1,...,N:
    //*                The QZ iteration failed.  (A,B) are not in Schur
    //*                form, but ALPHAR(j), ALPHAI(j), and BETA(j) should
    //*                be correct for j=INFO+1,...,N.
    //*          > N:  =N+1: other than QZ iteration failed in DHGEQZ.
    //*                =N+2: after reordering, roundoff changed values of
    //*                      some complex eigenvalues so that leading
    //*                      eigenvalues in the Generalized Schur form no
    //*                      longer satisfy DELZTG=.TRUE.  This could also
    //*                      be caused due to scaling.
    //*                =N+3: reordering failed in DTGSEN.
    std::cerr << __FILE__ ": info = " << info << ", something went wrong:\n";
    if (info < 0) {
      std::cerr << __FILE__ ": (internal error) the " << (-info) << "th argument had an illegal value\n";
    }
    else if (1 <= info && info <= n) {
      std::cerr << __FILE__ ": the QZ iteration failed, but the last " << (n - info) << " eigenvalues may be correct\n";
    }
    else if (info == n+1) {
      std::cerr << __FILE__ ": something went wrong in DHGEQZ\n";
    }
    else if (info == n+2) {
      std::cerr << __FILE__ ": roundoff error -- maybe due to poor scaling\n";
    }
    else if (info == n+3) {
      std::cerr << __FILE__ ": reordering failed in DTGSEN\n";
    }
    else {
      std::cerr << __FILE__ ": unknown error\n";
    }
    return false;
  }
}
