// This is core/vnl/algo/vnl_adjugate.h
#ifndef vnl_adjugate_h_
#define vnl_adjugate_h_
//:
// \file
// \author fsm and Peter Vanroose
//  The adjoint matrix of a square matrix, sometimes also called the adjugate
//  matrix (although that name is also used for the transpose of the complex
//  conjugate), is defined as the matrix whose (i,k)-th entry is the cofactor
//  of the (k,i)-th entry of the given matrix.  The cofactor of entry (k,i)
//  is the determinant of the matrix obtained by deleting row k and column i
//  from the given matrix.
//
//  The adjugate matrix is useful in finding the inverse of a square matrix
//  since det(A) * A_inverse = A_adj.
//
//  In contrast to the inverse matrix, however, calculating the adjoint matrix
//  does not involve divisions, so the adjoint of an integer matrix is integer.

#include <vnl/algo/vnl_algo_export.h>

template <class T> class vnl_matrix;

template <class T>
void vnl_adjugate(vnl_matrix<T> const &A, vnl_matrix<T> *out);

template <class T>
vnl_matrix<T> vnl_adjugate(vnl_matrix<T> const &A);

#endif // vnl_adjugate_h_
