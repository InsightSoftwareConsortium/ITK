// This is core/vnl/algo/vnl_generalized_eigensystem.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//
// vnl_generalized_eigensystem
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 29 Aug 96
//

#include <iostream>
#include "vnl_generalized_eigensystem.h"

#include <vcl_compiler.h>

#include <vnl/vnl_fortran_copy.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/algo/vnl_symmetric_eigensystem.h>
#include <vnl/algo/vnl_svd.h>
#include <vnl/algo/vnl_netlib.h> // rsg_()

vnl_generalized_eigensystem::vnl_generalized_eigensystem(const vnl_matrix<double>& A,
                                                         const vnl_matrix<double>& B)
  :
  n(A.rows()), V(n,n), D(n)
{
  // Copy source matrices into fortran storage
  vnl_fortran_copy<double> a(A);
  vnl_fortran_copy<double> b(B);

  // Make workspace and storage for V transpose
  vnl_vector<double> work1(n);
  vnl_vector<double> work2(n);
  vnl_vector<double> V1(n*n);

  long want_eigenvectors = 1;
  long ierr = -1;

  // Call EISPACK rsg.
  v3p_netlib_rsg_ (&n, &n, a, b, D.data_block(),
                   &want_eigenvectors,
                   V1.begin(),
                   work1.begin(),
                   work2.begin(), &ierr);

  // If b was not pos-def, retry with projection onto nullspace
  if (ierr == 7*n+1) {
    const double THRESH = 1e-8;
    vnl_symmetric_eigensystem<double> eig(B);
    if (eig.D(0,0) < -THRESH) {
      std::cerr << "**** vnl_generalized_eigensystem: ERROR\n"
               << "Matrix B is not nonneg-definite\n";
      vnl_matlab_print(std::cerr, B, "B");
      std::cerr << "**** eigenvalues(B) = " << eig.D << std::endl;
      return;
    }
    return;
  }

  // transpose-copy V1 to V
  {
    double *vptr = &V1[0];
    for (int c = 0; c < n; ++c)
      for (int r = 0; r < n; ++r)
        V(r,c) = *vptr++;
  }

  // Diagnose errors
  if (ierr) {
    if (ierr == 10*n)
      std::cerr << "vnl_generalized_eigensystem: N is greater than NM.  Bug in interface to rsg.f\n";
    else {
      std::cerr << "vnl_generalized_eigensystem: The "
               << ierr << "-th eigenvalue has not been determined after 30 iterations.\n"
               << "The eigenvalues should be correct for indices 1.." << ierr-1
               << ", but no eigenvectors are computed.\n"
               << "A = " << A
               << "\nsingular values(A) = " << vnl_svd<double>(A).W() << '\n'
               << "B = " << B
               << "\nsingular values(B) = " << vnl_svd<double>(B).W() << '\n';
    }
  }
}
