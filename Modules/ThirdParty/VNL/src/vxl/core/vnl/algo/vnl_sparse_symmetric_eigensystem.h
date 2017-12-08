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
//  28 Mar 2001: dac (Manchester) - tidied up documentation
//  17 Dec 2010: Michael Bowers - added generalized sparse symmetric eigensystem
//                                solver (see 2nd CalculateNPairs() method)
// \endverbatim

#include <vector>
#include <vnl/vnl_sparse_matrix.h>
#include <vcl_compiler.h>
#include <vnl/algo/vnl_algo_export.h>

//: Find the eigenvalues of a sparse symmetric matrix
//  Solve the standard eigenproblem $A x = \lambda x$, or the
//  generalized eigenproblem of $A x = \lambda B x$, where
//  $A$ symmetric and sparse and, optionally, B sparse, symmetric,
//  and positive definite.  The block Lanczos algorithm is used to allow the
//  recovery of a number of eigenvalue/eigenvector pairs from either
//  end of the spectrum, to a required accuracy.
//
//  Uses the dnlaso routine from the LASO package of netlib for
//  solving the standard case.
//  Uses the dsaupd routine from the ARPACK package of netlib for
//  solving the generalized case.

class VNL_ALGO_EXPORT vnl_sparse_symmetric_eigensystem
{
 public:
  vnl_sparse_symmetric_eigensystem();
 ~vnl_sparse_symmetric_eigensystem();

  // Find n eigenvalue/eigenvectors of the eigenproblem A * x = lambda * x.
  // If smallest is true, will calculate the n smallest eigenpairs,
  // else the n largest.
  int CalculateNPairs(vnl_sparse_matrix<double>& M, int n,
                      bool smallest = true, long nfigures = 10);

  // Find n eigenvalue/eigenvectors of the eigenproblem A * x = lambda * B * x.
  // !smallest and !magnitude - compute the N largest (algebraic) eigenvalues
  //  smallest and !magnitude - compute the N smallest (algebraic) eigenvalues
  // !smallest and  magnitude - compute the N largest (magnitude) eigenvalues
  //  smallest and  magnitude - compute the nev smallest (magnitude) eigenvalues
  // set sigma for shift/invert method
  int CalculateNPairs(vnl_sparse_matrix<double>& A, vnl_sparse_matrix<double>& B, int nEV,
                      double tolerance = 0, int numberLanczosVecs = 0,
                      bool smallest = false, bool magnitude = true,
                      int maxIterations = 0,
                      double sigma = 0.0);

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

  // Matrix A of A*x = lambda*x (or lambda*B*x)
  vnl_sparse_matrix<double> * mat;
  // Matrix B of A*x = lambda*B*x
  vnl_sparse_matrix<double> * Bmat;

  std::vector<double*> temp_store;
};

#endif // vnl_sparse_symmetric_eigensystem_h_
