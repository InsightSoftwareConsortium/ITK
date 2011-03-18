// This is core/vnl/algo/vnl_sparse_symmetric_eigensystem.h
#ifndef vnl_sparse_symmetric_eigensystem_h_
#define vnl_sparse_symmetric_eigensystem_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Find the eigenvalues of a sparse symmetric matrix
// \author Rupert W. Curwen, GE CR&D
// \date   20 Oct 98
//
// \verbatim
// Modifications
//  dac (Manchester) 28/03/2001: tidied up documentation
// \endverbatim

#include <vnl/vnl_sparse_matrix.h>
#include <vcl_vector.h>

//: Find the eigenvalues of a sparse symmetric matrix
//  Solve the eigenproblem $A x = \lambda x$, with $A$ symmetric and
//  sparse.  The block Lanczos algorithm is used to allow the
//  recovery of a number of eigenvalue/eigenvector pairs from either
//  end of the spectrum, to a required accuracy.
//
//  Uses the dnlaso routine from the LASO package of netlib.

class vnl_sparse_symmetric_eigensystem
{
 public:
  vnl_sparse_symmetric_eigensystem();
 ~vnl_sparse_symmetric_eigensystem();

  // Find n eigenvalue/eigenvectors.  If smallest is true, will
  // calculate the n smallest eigenpairs, else the n largest.
  int CalculateNPairs(vnl_sparse_matrix<double>& M, int n,
                      bool smallest = true, long nfigures = 10);

  // Recover specified eigenvector after computation.  The argument
  // must be less than the requested number of eigenvectors.
  vnl_vector<double> get_eigenvector(int i) const;
  double get_eigenvalue(int i) const;

  // Used as a callback in solving.
  int CalculateProduct(int n, int m, const double* p, double* q);
  int SaveVectors(int n, int m, const double* q, int base);
  int RestoreVectors(int n, int m, double* q, int base);

 protected:
  int nvalues;  // this is the size of the next two arrays.
  vnl_vector<double> * vectors; // eigenvectors
  double * values;              // eigenvalues

  vnl_sparse_matrix<double> * mat;

  vcl_vector<double*> temp_store;
};

#endif // vnl_sparse_symmetric_eigensystem_h_
