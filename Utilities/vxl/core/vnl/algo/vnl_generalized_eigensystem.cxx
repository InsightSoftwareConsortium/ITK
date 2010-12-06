// This is core/vnl/algo/vnl_generalized_eigensystem.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//
// vnl_generalized_eigensystem
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 29 Aug 96
//

#include "vnl_generalized_eigensystem.h"

#include <vcl_iostream.h>
#ifdef VCL_SGI_CC_730
# include <vcl_cmath.h>
#endif

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
      vcl_cerr << "**** vnl_generalized_eigensystem: ERROR\n"
               << "Matrix B is not nonneg-definite\n";
      vnl_matlab_print(vcl_cerr, B, "B");
      vcl_cerr << "**** eigenvalues(B) = " << eig.D << vcl_endl;
      return;
    }
    // hmmmmm - all this crap below is worse than whatever the default is...
    return;
#if 0 // so don't compile it then...
    int rank_deficiency = 0;
    while (eig.D(rank_deficiency,rank_deficiency) < THRESH)
      ++rank_deficiency;
    int rank = B.columns() - rank_deficiency;

    vcl_cerr << "vnl_generalized_eigensystem: B rank def by " << rank_deficiency << vcl_endl;
    // M is basis for non-nullspace of B
    vnl_matrix<double> M = eig.V.get_n_columns(rank_deficiency, rank);
    vnl_matrix<double> N = eig.V.get_n_columns(0, rank_deficiency);

    vnl_svd<double> svd(vnl_transpose(M)*A*N);

    vnl_generalized_eigensystem reduced(vnl_transpose(M) * A * M,
                                        vnl_transpose(M) * B * M);

    vcl_cerr << "AN: " << reduced.D << vcl_endl;

    vnl_matrix<double> V05 = M * vnl_transpose(reduced.V);
    vnl_svd<double> sv6(V05.transpose());
    V.update(V05, 0, 0);
    V.update(sv6.nullspace(), 0, rank - 1);
    for (int i = 0; i < rank; ++i)
      D(i,i) = reduced.D(i,i);
    for (unsigned i = rank; i < B.columns(); ++i)
      D(i,i) = 0;
    vcl_cerr << "AN: " << D << vcl_endl;

    return;
#endif
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
      vcl_cerr << "vnl_generalized_eigensystem: N is greater than NM.  Bug in interface to rsg.f\n";
    else {
      vcl_cerr << "vnl_generalized_eigensystem: The "
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
