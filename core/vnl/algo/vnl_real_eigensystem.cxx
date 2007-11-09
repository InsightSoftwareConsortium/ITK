// This is core/vnl/algo/vnl_real_eigensystem.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
//
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   23 Jan 97

//-----------------------------------------------------------------------------

#include "vnl_real_eigensystem.h"
#include <vcl_cassert.h>
#include <vcl_iostream.h>
#include <vnl/vnl_fortran_copy.h>
#include <vnl/algo/vnl_netlib.h> // rg_()

//: Extract eigensystem of unsymmetric matrix M, using the EISPACK routine rg.
//  Should probably switch to using LAPACK's dgeev to avoid transposing.
vnl_real_eigensystem::vnl_real_eigensystem(vnl_matrix<double> const & M):
  Vreal(M.rows(), M.columns()),
  V(M.rows(), M.columns()),
  D(M.rows())
{
  long n = M.rows();
  assert(n == (int)(M.columns()));

  vnl_fortran_copy<double> mf(M);

  vnl_vector<double> wr(n);
  vnl_vector<double> wi(n);
  vnl_vector<long> iv1(n);
  vnl_vector<double> fv1(n);
  vnl_matrix<double> devout(n, n);

  long ierr = 0;
  long matz = 1;
  v3p_netlib_rg_(&n, &n, mf,
                 wr.data_block(), wi.data_block(),
                 &matz, devout.data_block(),
                 iv1.data_block(), fv1.data_block(),
                 &ierr);

  if (ierr != 0) {
    vcl_cerr << " *** vnl_real_eigensystem: Failed on " << ierr << "th eigenvalue\n"
             << M << vcl_endl;
  }

  // Copy out eigenvalues and eigenvectors
  for (int c = 0; c < n; ++c) {
    D(c,c) = vcl_complex<double>(wr[c], wi[c]);
    if (wi[c] != 0) {
      // Complex - copy conjugates and inc c.
      D(c+1, c+1) = vcl_complex<double>(wr[c], -wi[c]);
      for (int r = 0; r < n; ++r) {
        V(r, c) = vcl_complex<double>(devout(c,r), devout(c+1,r));
        V(r, c+1) = vcl_complex<double>(devout(c,r), -devout(c+1,r));
      }

      ++c;
    } else
      for (int r = 0; r < n; ++r) {
        V(r, c) = vcl_complex<double>(devout(c,r), 0);
        Vreal(r,c) = devout(c,r);
      }
  }
}
