// This is core/vnl/algo/vnl_generalized_eigensystem.h
#ifndef vnl_generalized_eigensystem_h_
#define vnl_generalized_eigensystem_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief  Solves the generalized eigenproblem Ax=La
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   29 Aug 96
//
// \verbatim
// Modifications
//  dac (Manchester) 28/03/2001: tidied up documentation
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim

#include <vnl/vnl_diag_matrix.h>

//: Solves the generalized eigenproblem Ax=Bx
//  Solves the generalized eigenproblem of $A x = \lambda B x$,
//  with $A$ symmetric and $B$ positive definite. \n
//  See Golub and van Loan, Section 8.7.

class vnl_generalized_eigensystem
{
 public:
// Public data members because they're unique.
  long n;

//: Solves the generalized eigenproblem Ax=Bx
//  Solve real generalized eigensystem $A x = \lambda B x$ for
//  $\lambda$ and $x$, where $A$ symmetric, $B$ positive definite.
//  Initializes storage for the matrix $V = [ x_0 x_1 .. x_n ]$ and
//  the vnl_diag_matrix $D = [ \lambda_0 \lambda_1 ... \lambda_n ]$.
//  The eigenvalues are sorted into increasing order (of value, not
//  absolute value).
//
//  Uses vnl_cholesky decomposition $C^\top C = B$, to convert to
//  $C^{-\top} A C^{-1} x = \lambda x$ and then uses the
//  symmetric eigensystem code.   It will print a verbose warning
//  if $B$ is not positive definite.

  vnl_generalized_eigensystem(const vnl_matrix<double>& A,
                              const vnl_matrix<double>& B);

//: Public eigenvectors.
//  After construction, this contains the matrix of eigenvectors.
  vnl_matrix<double> V;

//: Public eigenvalues.
//  After construction, this contains the diagonal matrix of eigenvalues, stored as a vector.
  vnl_diag_matrix<double> D;
};

#endif // vnl_generalized_eigensystem_h_
