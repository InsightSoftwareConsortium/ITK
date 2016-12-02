/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSymmetricEigenAnalysis_h
#define itkSymmetricEigenAnalysis_h

#include "itkMacro.h"

namespace itk
{
/** \class SymmetricEigenAnalysis
 * \brief Find Eigen values of a real 2D symmetric matrix. It
 * serves as a thread-safe alternative to the class:
 * vnl_symmetric_eigensystem, which uses netlib routines.
 *
 * The class is templated over the input matrix (which is expected to provide
 * access to its elements with the [][] operator), matrix to store eigen
 * values (must provide write operations on its elements with the [] operator), and
 * EigenMatrix to store eigen vectors (must provide write access to its elements
 * with the [][] operator).
 *
 * The SetOrderEigenValues() method can be used to order eigen values (and their
 * corresponding eigen vectors if computed) in ascending order. This is the
 * default ordering scheme. Eigen vectors and values can be obtained without
 * ordering by calling SetOrderEigenValues(false).
 *
 * The SetOrderEigenMagnitudes() method can be used to order eigen values (and
 * their corresponding eigen vectors if computed) by magnitude in ascending order.
 *
 * The user of this class is explicitly supposed to set the dimension of the
 * 2D matrix using the SetDimension() method.
 *
 * The class contains routines taken from netlib sources (www.netlib.org).
 * netlib/tql1.c
 * netlib/tql2.c
 * netlib/tred1.c
 * netlib/tred2.c
 *
 * Reference:
 *     num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and
 *     wilkinson.
 *     handbook for auto. comp., vol.ii-linear algebra, 227-240(1971).
 * \ingroup ITKCommon
 */

template< typename TMatrix, typename TVector, typename TEigenMatrix = TMatrix >
class ITK_TEMPLATE_EXPORT SymmetricEigenAnalysis
{
public:
  typedef enum
  {
    OrderByValue = 1,
    OrderByMagnitude,
    DoNotOrder
  }
  EigenValueOrderType;

  SymmetricEigenAnalysis():
  m_Dimension(0),
  m_Order(0),
  m_OrderEigenValues(OrderByValue) {}

  SymmetricEigenAnalysis(const unsigned int dimension):
  m_Dimension(dimension),
  m_Order(dimension),
  m_OrderEigenValues(OrderByValue) {}

  ~SymmetricEigenAnalysis() {}

  typedef TMatrix      MatrixType;
  typedef TEigenMatrix EigenMatrixType;
  typedef TVector      VectorType;

  /** Compute Eigen values of A
   * A is any type that overloads the [][] operator and contains the
   * symmetric matrix. In practice only the upper triangle of the
   * matrix will be accessed. (Both itk::Matrix and vnl_matrix
   * overload [][] operator.)
   *
   * 'EigenValues' is any type that overloads the [][] operator and will contain
   * the eigen values.
   *
   * No size checking is performed. A is expected to be a square matrix of size
   * m_Dimension.  'EigenValues' is expected to be of length m_Dimension.
   * The matrix is not checked to see if it is symmetric.
   */
  unsigned int ComputeEigenValues(
    const TMatrix  & A,
    TVector        & EigenValues) const;

  /** Compute Eigen values and vectors of A
   * A is any type that overloads the [][] operator and contains the
   * symmetric matrix. In practice only the upper triangle of the
   * matrix will be accessed. (Both itk::Matrix and vnl_matrix
   * overload [][] operator.)
   *
   * 'EigenValues' is any type that overloads the [][] operator and will contain
   * the eigen values.
   *
   * 'EigenVectors' is any type that provides access to its elements with the
   * [][] operator. It is expected be of size m_Dimension * m_Dimension.
   *
   * No size checking is performed. A is expected to be a square matrix of size
   * m_Dimension.  'EigenValues' is expected to be of length m_Dimension.
   * The matrix is not checked to see if it is symmetric.
   *
   * Each row of the matrix 'EigenVectors' represents an eigen vector. (unlike MATLAB
   * where the columns of the [EigenVectors, EigenValues] = eig(A) contains the
   * eigenvectors).
   */
  unsigned int ComputeEigenValuesAndVectors(
    const TMatrix  & A,
    TVector        & EigenValues,
    TEigenMatrix   & EigenVectors) const;

  /** Matrix order. Defaults to matrix dimension if not set */
  void SetOrder(const unsigned int n)
  {
    m_Order = n;
  }

  /** Get the Matrix order. Will be 0 unless explicitly set, or unless a
   * call to SetDimension has been made in which case it will be the
   * matrix dimension. */
  unsigned int GetOrder() const { return m_Order; }

  /** Set/Get methods to order the eigen values in ascending order.
   * This is the default. ie lambda_1 < lambda_2 < ....
   */
  void SetOrderEigenValues(const bool b)
  {
    if ( b ) { m_OrderEigenValues = OrderByValue;     }
    else   { m_OrderEigenValues = DoNotOrder;       }
  }

  bool GetOrderEigenValues() const { return ( m_OrderEigenValues == OrderByValue ); }

  /** Set/Get methods to order the eigen value magnitudes in ascending order.
   * In other words, |lambda_1| < |lambda_2| < .....
   */
  void SetOrderEigenMagnitudes(const bool b)
  {
    if ( b ) { m_OrderEigenValues = OrderByMagnitude; }
    else   { m_OrderEigenValues = DoNotOrder;       }
  }

  bool GetOrderEigenMagnitudes() const { return ( m_OrderEigenValues == OrderByMagnitude ); }

  /** Set the dimension of the input matrix A. A is a square matrix of
   * size m_Dimension. */
  void SetDimension(const unsigned int n)
  {
    m_Dimension = n;
    if ( m_Order == 0 )
      {
      m_Order = m_Dimension;
      }
  }

  /** Get Matrix dimension, Will be 0 unless explicitly set by a
   * call to SetDimension. */
  unsigned int GetDimension() const { return m_Dimension; }

private:
  unsigned int        m_Dimension;
  unsigned int        m_Order;
  EigenValueOrderType m_OrderEigenValues;

  /** Reduces a real symmetric matrix to a symmetric tridiagonal matrix using
   *  orthogonal similarity transformations.
   *  'inputMatrix' contains the real symmetric input matrix. Only the lower
   *  triangle of the matrix need be supplied. The upper triangle is unaltered.
   *  'd' contains the diagonal elements of the tridiagonal matrix.
   *  'e' contains the subdiagonal elements of the tridiagonal matrix in its
   *  last n-1 positions.  e(1) is set to zero.
   *  'e2' contains the squares of the corresponding elements of e.
   *  'e2' may coincide with e if the squares are not needed.
   *  questions and comments should be directed to burton s. garbow.
   *  mathematics and computer science div, argonne national laboratory
   *     this version dated august 1983.
   *
   *  Function adapted from netlib/tred1.c.
   *  [Changed: remove static vars, enforce const correctness.
   *            Use vnl routines as necessary].
   *  Reference:
   *  num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson.
   *    handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).    */
  void ReduceToTridiagonalMatrix(double *inputMatrix, double *d,
                                 double *e, double *e2) const;

  /** Reduces a real symmetric matrix to a symmetric tridiagonal matrix using
   *  and accumulating orthogonal similarity transformations.
   *  'inputMatrix' contains the real symmetric input matrix. Only the lower
   *  triangle of the matrix need be supplied. The upper triangle is unaltered.
   *  'diagonalElements' will contains the diagonal elements of the tridiagonal
   *  matrix.
   *  'subDiagonalElements' will contain the subdiagonal elements of the tridiagonal
   *  matrix in its last n-1 positions.  subDiagonalElements(1) is set to zero.
   *  'transformMatrix' contains the orthogonal transformation matrix produced
   *  in the reduction.
   *
   *  Questions and comments should be directed to Burton s. Garbow,
   *  Mathematics and Computer Science Div., Argonne National Laboratory.
   *  This version dated august 1983.
   *
   *  Function adapted from netlib/tred2.c.
   *  [Changed: remove static vars, enforce const correctness.
   *            Use vnl routines as necessary].
   *  Reference:
   *  num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson.
   *    handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).    */
  void ReduceToTridiagonalMatrixAndGetTransformation(
    double *inputMatrix, double *diagonalElements,
    double *subDiagonalElements, double *transformMatrix) const;

  /** Finds the eigenvalues of a symmetric tridiagonal matrix by the ql method.
   *
   * On input:
   * 'd' contains the diagonal elements of the input matrix.
   * 'e' contains the subdiagonal elements of the input matrix
   * in its last n-1 positions.  e(1) is arbitrary.
   * On Output:
   * 'd' contains the eigenvalues.
   * 'e' has been destroyed.
   *
   * Returns:
   *          zero       for normal return,
   *          j          if the j-th eigenvalue has not been
   *                     determined after 30 iterations.
   *
   *
   * Reference
   *  This subroutine is a translation of the algol procedure tql1,
   *  num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and
   *  wilkinson.
   *  handbook for auto. comp., vol.ii-linear algebra, 227-240(1971).
   *
   *  Questions and comments should be directed to Burton s. Garbow,
   *  Mathematics and Computer Science Div., Argonne National Laboratory.
   *  This version dated august 1983.
   *
   *  Function Adapted from netlib/tql1.c.
   *  [Changed: remove static vars, enforce const correctness.
   *            Use vnl routines as necessary]                      */
  unsigned int ComputeEigenValuesUsingQL(double *d, double *e) const;

  /** Finds the eigenvalues and eigenvectors of a symmetric tridiagonal matrix
   * by the ql method.
   *
   * On input:
   * 'd' contains the diagonal elements of the input matrix.
   * 'e' contains the subdiagonal elements of the input matrix
   * in its last n-1 positions.  e(1) is arbitrary.
   * 'z' contains the transformation matrix produced in the reduction by
   * ReduceToTridiagonalMatrixAndGetTransformation(), if performed. If the
   * eigenvectors of the tridiagonal matrix are desired, z must contain
   * the identity matrix.

   * On Output:
   * 'd' contains the eigenvalues.
   * 'e' has been destroyed.
   * 'z' contains orthonormal eigenvectors of the symmetric tridiagonal
   * (or full) matrix.
   *
   * Returns:
   *          zero       for normal return,
   *          j          if the j-th eigenvalue has not been
   *                     determined after 1000 iterations.
   *
   * Reference
   *  This subroutine is a translation of the algol procedure tql1,
   *  num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and
   *  wilkinson.
   *  handbook for auto. comp., vol.ii-linear algebra, 227-240(1971).
   *
   *  Questions and comments should be directed to Burton s. Garbow,
   *  Mathematics and Computer Science Div., Argonne National Laboratory.
   *  This version dated august 1983.
   *
   *  Function Adapted from netlib/tql2.c.
   *  [Changed: remove static vars, enforce const correctness.
   *            Use vnl routines as necessary]
   */
  unsigned int ComputeEigenValuesAndVectorsUsingQL(double *d, double *e, double *z) const;
};

template< typename TMatrix, typename TVector, typename TEigenMatrix >
std::ostream & operator<<(std::ostream & os,
                          const SymmetricEigenAnalysis< TMatrix, TVector, TEigenMatrix > & s)
{
  os << "[ClassType: SymmetricEigenAnalysis]" << std::endl;
  os << "  Dimension : " << s.GetDimension() << std::endl;
  os << "  Order : " << s.GetOrder() << std::endl;
  os << "  OrderEigenValues: " << s.GetOrderEigenValues() << std::endl;
  os << "  OrderEigenMagnitudes: " << s.GetOrderEigenMagnitudes() << std::endl;
  return os;
}
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSymmetricEigenAnalysis.hxx"
#endif

#endif
