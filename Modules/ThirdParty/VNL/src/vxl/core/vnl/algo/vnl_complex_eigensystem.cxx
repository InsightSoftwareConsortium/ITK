// This is core/vnl/algo/vnl_complex_eigensystem.cxx
#include <iostream>
#include "vnl_complex_eigensystem.h"
// \author fsm

#include <vcl_cassert.h>
#include <vcl_compiler.h>

#include <vnl/vnl_matlab_print.h>
#include <vnl/vnl_complexify.h>
#include <vnl/algo/vnl_netlib.h> // zgeev_()

void vnl_complex_eigensystem::compute(vnl_matrix<std::complex<double> > const & A,
                                      bool right,
                                      bool left)
{
  A.assert_size(N, N);

  A.assert_finite();
  assert(! A.is_zero());

  if (right)
    R.set_size(N, N);
  if (left)
    L.set_size(N, N);

  //
  // Remember that fortran matrices and C matrices are transposed
  // relative to each other. Moreover, the documentation for zgeev
  // says that left eigenvectors u satisfy u^h A = lambda u^h,
  // where ^h denotes adjoint (conjugate transpose).
  // So we pass our left eigenvector storage as their right
  // eigenvector storage and vice versa.
  // But then we also have to conjugate our R after calling the routine.
  //
  vnl_matrix<std::complex<double> > tmp(A);

  long work_space=10*N;
  vnl_vector<std::complex<double> > work(work_space);

  long rwork_space=2*N;
  vnl_vector<double> rwork(rwork_space);

  long info;
  long tmpN = N;
  v3p_netlib_zgeev_(
         right ? "V" : "N",          // jobvl
         left  ? "V" : "N",          // jobvr
         &tmpN,                      // n
         tmp.data_block(),           // a
         &tmpN,                      // lda
         W.data_block(),             // w
         right ? R.data_block() : VXL_NULLPTR, // vl
         &tmpN,                      // ldvl
         left  ? L.data_block() : VXL_NULLPTR, // vr
         &tmpN,                      // ldvr
         work.data_block(),          // work
         &work_space,                // lwork
         rwork.data_block(),         // rwork
         &info,                      // info
         1, 1);
  assert(tmpN == int(N));

  if (right) {
    // conjugate all elements of R :
    for (unsigned int i=0;i<N;i++)
      for (unsigned int j=0;j<N;j++)
        R(i,j) = std::conj( R(i,j) );
  }

  if (info == 0) {
    // success
  }
  else if (info < 0) {
    std::cerr << __FILE__ ": info = " << info << std::endl
             << __FILE__ ": " << (-info) << "th argument has illegal value\n";
    assert(false);
  }
  else /* if (info > 0) */ {
    std::cerr << __FILE__ ": info = " << info << std::endl
             << __FILE__ ": QR algorithm failed to compute all eigenvalues.\n";
    vnl_matlab_print(std::cerr, A, "A", vnl_matlab_print_format_long);
    assert(false);
  }
}

//--------------------------------------------------------------------------------

//
vnl_complex_eigensystem::vnl_complex_eigensystem(vnl_matrix<std::complex<double> > const &A,
                                                 bool right,
                                                 bool left)
  : N(A.rows())
  // L and R are intentionally not initialized.
  , W(N)
{
  compute(A, right, left);
}

//
vnl_complex_eigensystem::vnl_complex_eigensystem(vnl_matrix<double> const &A_real,
                                                 vnl_matrix<double> const &A_imag,
                                                 bool right,
                                                 bool left)
  : N(A_real.rows())
  // L and R are intentionally not initialized.
  , W(N)
{
  A_real.assert_size(N,N);
  A_imag.assert_size(N,N);

  vnl_matrix<std::complex<double> > A(N,N);
  vnl_complexify(A_real.begin(), A_imag.begin(), A.begin(), A.size());

  compute(A, right, left);
}
