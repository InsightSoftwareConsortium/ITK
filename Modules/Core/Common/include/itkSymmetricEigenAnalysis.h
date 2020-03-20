/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itk_eigen.h"
#include ITK_EIGEN(Eigenvalues)
#include <numeric>
#include <vector>
// For GetPointerToMatrixData
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkMatrix.h"

namespace itk
{
namespace detail
{
/* Helper functions returning pointer to matrix data for different types.  */
template <typename TValueType, unsigned int NRows, unsigned int NCols>
const TValueType *
GetPointerToMatrixData(const vnl_matrix_fixed<TValueType, NRows, NCols> & inputMatrix)
{
  return inputMatrix.data_block();
};
template <typename TValueType>
const TValueType *
GetPointerToMatrixData(const vnl_matrix<TValueType> & inputMatrix)
{
  return inputMatrix.data_block();
};

template <typename TValueType, unsigned int NRows, unsigned int NCols>
const TValueType *
GetPointerToMatrixData(const itk::Matrix<TValueType, NRows, NCols> & inputMatrix)
{
  return inputMatrix.GetVnlMatrix().data_block();
};

/** Sort input to be ordered by magnitude, and returns container with the
 * permutations required for the sorting.
 *
 * For example, if input eigenValues = {10, 0, 40}, the output would be: {2,0,1}
 * and the eigenValues would be modified in-place: {40, 10, 0}.
 *
 * The permutations indices is used to order the matrix of eigenVectors.
 * \sa permuteEigenVectorsWithSortPermutations
 *
 * @tparam TArray  array type with operator []
 * @param eigenValues input array, requires operator []
 * @param numberOfElements size of array
 *
 * @return the permutations needed to sort the input array
 */
template <typename TArray>
std::vector<int>
sortEigenValuesByMagnitude(TArray & eigenValues, const unsigned int numberOfElements)
{
  std::vector<int> indicesSortPermutations(numberOfElements, 0);
  std::iota(std::begin(indicesSortPermutations), std::end(indicesSortPermutations), 0);

  std::sort(
    std::begin(indicesSortPermutations),
    std::end(indicesSortPermutations),
    [&eigenValues](unsigned int a, unsigned int b) { return std::abs(eigenValues[a]) < std::abs(eigenValues[b]); });
  auto tmpCopy = eigenValues;
  for (unsigned int i = 0; i < numberOfElements; ++i)
  {
    eigenValues[i] = tmpCopy[indicesSortPermutations[i]];
  }
  return indicesSortPermutations;
}

/** Permute a eigenVectors matrix according to the permutation indices
 * computed from the output of a sort function like \sa detail::sortEigenValuesByMagnitude
 *
 * @tparam QMatrix a Eigen3 matrix
 * @param eigenVectors stored in columns
 * @param indicesSortPermutations container with the permutations from the output of
 * a sort function.
 */
template <typename QMatrix>
void
permuteColumnsWithSortIndices(QMatrix & eigenVectors, const std::vector<int> & indicesSortPermutations)
{
  using EigenLibPermutationMatrix = Eigen::PermutationMatrix<Eigen::Dynamic, Eigen::Dynamic>;
  auto numberOfElements = indicesSortPermutations.size();
  // Creates a NxN permutation matrix copying our permutation to the matrix indices.
  // Which holds the 1D array representation of a permutation.
  EigenLibPermutationMatrix perm(numberOfElements);
  perm.setIdentity();
  std::copy(indicesSortPermutations.begin(), indicesSortPermutations.end(), perm.indices().data());
  // Apply it
  eigenVectors = eigenVectors * perm;
}
} // end namespace detail

/**\class SymmetricEigenAnalysisEnums
 * \brief This class contains all enum classes used by SymmetricEigenAnalysis class.
 * \ingroup ITKCommon
 */
class SymmetricEigenAnalysisEnums
{
public:
  /** \class EigenValueOrder
   * \ingroup ITKCommon
   * Order of eigen values
   * OrderByValue:      lambda_1 < lambda_2 < ....
   * OrderByMagnitude:  |lambda_1| < |lambda_2| < .....
   * DoNotOrder:        Default order of eigen values obtained after QL method
   */
  enum class EigenValueOrder : uint8_t
  {
    OrderByValue = 1,
    OrderByMagnitude = 2,
    DoNotOrder = 3
  };
};
// Define how to print enumeration
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const SymmetricEigenAnalysisEnums::EigenValueOrder value);

using EigenValueOrderEnum = SymmetricEigenAnalysisEnums::EigenValueOrder;

inline EigenValueOrderEnum
Int2EigenValueOrderEnum(const uint8_t value)
{
  switch (value)
  {
    case 1:
      return EigenValueOrderEnum::OrderByValue;
    case 2:
      return EigenValueOrderEnum::OrderByMagnitude;
    case 3:
      return EigenValueOrderEnum::DoNotOrder;
    default:
      break;
  }
  itkGenericExceptionMacro(<< "Error: Invalid value for conversion.");
}

#if !defined(ITK_LEGACY_REMOVE)
/** Enables reverse compatibility for enumeration values */
static constexpr EigenValueOrderEnum OrderByValue = EigenValueOrderEnum::OrderByValue;
static constexpr EigenValueOrderEnum OrderByMagnitude = EigenValueOrderEnum::OrderByMagnitude;
static constexpr EigenValueOrderEnum DoNotOrder = EigenValueOrderEnum::DoNotOrder;
#endif

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

template <typename TMatrix, typename TVector, typename TEigenMatrix = TMatrix>
class ITK_TEMPLATE_EXPORT SymmetricEigenAnalysis
{
public:
  using EigenValueOrderEnum = itk::EigenValueOrderEnum;
#if !defined(ITK_LEGACY_REMOVE)
  /** Enables reverse compatibility for enumeration values */
  using EigenValueOrderType = EigenValueOrderEnum;
#endif

  SymmetricEigenAnalysis() = default;

  SymmetricEigenAnalysis(const unsigned int dimension)
    : m_Dimension(dimension)
    , m_Order(dimension)
  {}

  ~SymmetricEigenAnalysis() = default;

  using MatrixType = TMatrix;
  using EigenMatrixType = TEigenMatrix;
  using VectorType = TVector;

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
  unsigned int
  ComputeEigenValues(const TMatrix & A, TVector & EigenValues) const;

  /** Compute Eigen values and vectors of A
   * A is any type that overloads the [][] operator and contains the
   * symmetric matrix. In practice only the upper triangle of the
   * matrix will be accessed. (Both itk::Matrix and vnl_matrix
   * overload [][] operator.)
   *
   * 'EigenValues' is any type that overloads the [] operator and will contain
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
  unsigned int
  ComputeEigenValuesAndVectors(const TMatrix & A, TVector & EigenValues, TEigenMatrix & EigenVectors) const;


  /** Matrix order. Defaults to matrix dimension if not set */
  void
  SetOrder(const unsigned int n)
  {
    m_Order = n;
  }

  /** Get the Matrix order. Will be 0 unless explicitly set, or unless a
   * call to SetDimension has been made in which case it will be the
   * matrix dimension. */
  unsigned int
  GetOrder() const
  {
    return m_Order;
  }

  /** Set/Get methods to order the eigen values in ascending order.
   * This is the default. ie lambda_1 < lambda_2 < ....
   */
  void
  SetOrderEigenValues(const bool b)
  {
    if (b)
    {
      m_OrderEigenValues = EigenValueOrderEnum::OrderByValue;
    }
    else
    {
      m_OrderEigenValues = EigenValueOrderEnum::DoNotOrder;
    }
  }

  bool
  GetOrderEigenValues() const
  {
    return (m_OrderEigenValues == EigenValueOrderEnum::OrderByValue);
  }

  /** Set/Get methods to order the eigen value magnitudes in ascending order.
   * In other words, |lambda_1| < |lambda_2| < .....
   */
  void
  SetOrderEigenMagnitudes(const bool b)
  {
    if (b)
    {
      m_OrderEigenValues = EigenValueOrderEnum::OrderByMagnitude;
    }
    else
    {
      m_OrderEigenValues = EigenValueOrderEnum::DoNotOrder;
    }
  }

  bool
  GetOrderEigenMagnitudes() const
  {
    return (m_OrderEigenValues == EigenValueOrderEnum::OrderByMagnitude);
  }

  /** Set the dimension of the input matrix A. A is a square matrix of
   * size m_Dimension. */
  void
  SetDimension(const unsigned int n)
  {
    m_Dimension = n;
    if (m_Order == 0)
    {
      m_Order = m_Dimension;
    }
  }

  /** Get Matrix dimension, Will be 0 unless explicitly set by a
   * call to SetDimension. */
  unsigned int
  GetDimension() const
  {
    return m_Dimension;
  }

  /** Set/Get to use Eigen library instead of vnl/netlib. */
  void
  SetUseEigenLibrary(const bool input)
  {
    m_UseEigenLibrary = input;
  }
  void
  SetUseEigenLibraryOn()
  {
    m_UseEigenLibrary = true;
  }
  void
  SetUseEigenLibraryOff()
  {
    m_UseEigenLibrary = false;
  }
  bool
  GetUseEigenLibrary() const
  {
    return m_UseEigenLibrary;
  }

private:
  bool                m_UseEigenLibrary{ false };
  unsigned int        m_Dimension{ 0 };
  unsigned int        m_Order{ 0 };
  EigenValueOrderEnum m_OrderEigenValues{ EigenValueOrderEnum::OrderByValue };

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
  void
  ReduceToTridiagonalMatrix(double * inputMatrix, double * d, double * e, double * e2) const;

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
  void
  ReduceToTridiagonalMatrixAndGetTransformation(double * inputMatrix,
                                                double * diagonalElements,
                                                double * subDiagonalElements,
                                                double * transformMatrix) const;

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
  unsigned int
  ComputeEigenValuesUsingQL(double * d, double * e) const;

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
  unsigned int
  ComputeEigenValuesAndVectorsUsingQL(double * d, double * e, double * z) const;

  /* Legacy algorithms using thread-safe netlib.
   * \sa ComputeEigenValues and \sa ComputeEigenValuesAndVectors
   */
  unsigned int
  ComputeEigenValuesLegacy(const TMatrix & A, TVector & EigenValues) const;

  unsigned int
  ComputeEigenValuesAndVectorsLegacy(const TMatrix & A, TVector & EigenValues, TEigenMatrix & EigenVectors) const;

  /* Helper to get the matrix value type for EigenLibMatrix typename.
   *
   * If the TMatrix is vnl, the type is in element_type.
   * In TMatrix is itk::Matrix, or any itk::FixedArray is in ValueType.
   *
   * To use this function:
   * using ValueType = decltype(this->GetMatrixType(true));
   *
   * \note The two `GetMatrixValueType` overloads have different
   * parameter declarations (`bool` and `...`), to avoid that both
   * functions are equally good candidates during overload resolution,
   * in case `element_type` and `ValueType` are both nested types of
   * `TMatrix` (which is the case when `TMatrix` = `itk::Array2D`).
   */
  template <typename QMatrix = TMatrix>
  auto
  GetMatrixValueType(bool) const -> typename QMatrix::element_type
  {
    return QMatrix::element_type();
  }
  template <typename QMatrix = TMatrix>
  auto
  GetMatrixValueType(...) const -> typename QMatrix::ValueType
  {
    return QMatrix::ValueType();
  }

  /* Wrapper that call the right implementation for the type of matrix.  */
  unsigned int
  ComputeEigenValuesAndVectorsWithEigenLibrary(const TMatrix & A,
                                               TVector &       EigenValues,
                                               TEigenMatrix &  EigenVectors) const
  {
    return ComputeEigenValuesAndVectorsWithEigenLibraryImpl(A, EigenValues, EigenVectors, true);
  }

  /* Implementation detail using EigenLib that performs a copy of the input matrix.
   *
   * @param (long) implementation detail argument making this implementation less favourable
   *   to be chosen if alternatives are available.
   *
   * @return an unsigned int with no information value (no error code in EigenLib) */
  template <typename QMatrix>
  auto
  ComputeEigenValuesAndVectorsWithEigenLibraryImpl(const QMatrix & A,
                                                   TVector &       EigenValues,
                                                   TEigenMatrix &  EigenVectors,
                                                   long) const -> decltype(static_cast<unsigned int>(1))
  {
    using ValueType = decltype(GetMatrixValueType(true));
    using EigenLibMatrixType = Eigen::Matrix<ValueType, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
    EigenLibMatrixType inputMatrix(m_Dimension, m_Dimension);
    for (unsigned int row = 0; row < m_Dimension; ++row)
    {
      for (unsigned int col = 0; col < m_Dimension; ++col)
      {
        inputMatrix(row, col) = A(row, col);
      }
    }
    using EigenSolverType = Eigen::SelfAdjointEigenSolver<EigenLibMatrixType>;
    EigenSolverType solver(inputMatrix); // Computes EigenValues and EigenVectors
    const auto &    eigenValues = solver.eigenvalues();
    /* Column  k  of the returned matrix is an eigenvector corresponding to
     * eigenvalue number $ k $ as returned by eigenvalues().
     * The eigenvectors are normalized to have (Euclidean) norm equal to one. */
    const auto & eigenVectors = solver.eigenvectors();

    if (m_OrderEigenValues == EigenValueOrderEnum::OrderByMagnitude)
    {
      auto copyEigenValues = eigenValues;
      auto copyEigenVectors = eigenVectors;
      auto indicesSortPermutations = detail::sortEigenValuesByMagnitude(copyEigenValues, m_Dimension);
      detail::permuteColumnsWithSortIndices(copyEigenVectors, indicesSortPermutations);

      for (unsigned int row = 0; row < m_Dimension; ++row)
      {
        EigenValues[row] = copyEigenValues[row];
        for (unsigned int col = 0; col < m_Dimension; ++col)
        {
          EigenVectors[row][col] = copyEigenVectors(col, row);
        }
      }
    }
    else
    {
      for (unsigned int row = 0; row < m_Dimension; ++row)
      {
        EigenValues[row] = eigenValues[row];
        for (unsigned int col = 0; col < m_Dimension; ++col)
        {
          EigenVectors[row][col] = eigenVectors(col, row);
        }
      }
    }
    // No error code
    return 1;
  }


  /* Implementation detail using EigenLib that do not peform a copy.
   * It needs the existence of a pointer to matrix data. \sa GetPointerToMatrixData
   * If new types want to use this method, an appropriate overload of GetPointerToMatrixData
   * should be included.
   *
   * @param (bool) implementation detail argument making this implementation the most favourable
   *   to be chosen from all the alternative implementations.
   *
   * @return an unsigned int with no information value (no error code in EigenLib) */
  template <typename QMatrix>
  auto
  ComputeEigenValuesAndVectorsWithEigenLibraryImpl(const QMatrix & A,
                                                   TVector &       EigenValues,
                                                   TEigenMatrix &  EigenVectors,
                                                   bool) const
    -> decltype(GetPointerToMatrixData(A), static_cast<unsigned int>(1))
  {
    auto pointerToData = GetPointerToMatrixData(A);
    using PointerType = decltype(pointerToData);
    using ValueTypeCV = typename std::remove_pointer<PointerType>::type;
    using ValueType = typename std::remove_cv<ValueTypeCV>::type;
    using EigenLibMatrixType = Eigen::Matrix<ValueType, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
    using EigenConstMatrixMap = Eigen::Map<const EigenLibMatrixType>;
    EigenConstMatrixMap inputMatrix(pointerToData, m_Dimension, m_Dimension);
    using EigenSolverType = Eigen::SelfAdjointEigenSolver<EigenLibMatrixType>;
    EigenSolverType solver(inputMatrix); // Computes EigenValues and EigenVectors
    const auto &    eigenValues = solver.eigenvalues();
    /* Column  k  of the returned matrix is an eigenvector corresponding to
     * eigenvalue number $ k $ as returned by eigenvalues().
     * The eigenvectors are normalized to have (Euclidean) norm equal to one. */
    const auto & eigenVectors = solver.eigenvectors();
    if (m_OrderEigenValues == EigenValueOrderEnum::OrderByMagnitude)
    {
      auto copyEigenValues = eigenValues;
      auto copyEigenVectors = eigenVectors;
      auto indicesSortPermutations = detail::sortEigenValuesByMagnitude(copyEigenValues, m_Dimension);
      detail::permuteColumnsWithSortIndices(copyEigenVectors, indicesSortPermutations);
      for (unsigned int row = 0; row < m_Dimension; ++row)
      {
        EigenValues[row] = copyEigenValues[row];
        for (unsigned int col = 0; col < m_Dimension; ++col)
        {
          EigenVectors[row][col] = copyEigenVectors(col, row);
        }
      }
    }
    else
    {
      for (unsigned int row = 0; row < m_Dimension; ++row)
      {
        EigenValues[row] = eigenValues[row];
        for (unsigned int col = 0; col < m_Dimension; ++col)
        {
          EigenVectors[row][col] = eigenVectors(col, row);
        }
      }
    }
    // No error code
    return 1;
  }

  /* Wrapper that call the right implementation for the type of matrix.  */
  unsigned int
  ComputeEigenValuesWithEigenLibrary(const TMatrix & A, TVector & EigenValues) const
  {
    return ComputeEigenValuesWithEigenLibraryImpl(A, EigenValues, true);
  }

  /* Implementation detail using EigenLib that performs a copy of the input matrix.
   *
   * @param (long) implementation detail argument making this implementation less favourable
   *   to be chosen if alternatives are available.
   *
   * @return an unsigned int with no information value (no error code in EigenLib) */
  template <typename QMatrix>
  auto
  ComputeEigenValuesWithEigenLibraryImpl(const QMatrix & A, TVector & EigenValues, long) const
    -> decltype(static_cast<unsigned int>(1))
  {
    using ValueType = decltype(GetMatrixValueType(true));
    using EigenLibMatrixType = Eigen::Matrix<ValueType, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
    EigenLibMatrixType inputMatrix(m_Dimension, m_Dimension);
    for (unsigned int row = 0; row < m_Dimension; ++row)
    {
      for (unsigned int col = 0; col < m_Dimension; ++col)
      {
        inputMatrix(row, col) = A(row, col);
      }
    }
    using EigenSolverType = Eigen::SelfAdjointEigenSolver<EigenLibMatrixType>;
    EigenSolverType solver(inputMatrix, Eigen::EigenvaluesOnly);
    auto            eigenValues = solver.eigenvalues();
    if (m_OrderEigenValues == EigenValueOrderEnum::OrderByMagnitude)
    {
      detail::sortEigenValuesByMagnitude(eigenValues, m_Dimension);
    }
    for (unsigned int i = 0; i < m_Dimension; ++i)
    {
      EigenValues[i] = eigenValues[i];
    }

    // No error code
    return 1;
  }

  /* Implementation detail using EigenLib that do not peform a copy.
   * It needs the existence of a pointer to matrix data. \sa GetPointerToMatrixData
   * If new types want to use this method, an appropriate overload of GetPointerToMatrixData
   * should be included.
   *
   * @param (bool) implementation detail argument making this implementation the most favourable
   *   to be chosen from all the alternative implementations.
   *
   * @return an unsigned int with no information value (no error code in EigenLib) */
  template <typename QMatrix>
  auto
  ComputeEigenValuesWithEigenLibraryImpl(const QMatrix & A, TVector & EigenValues, bool) const
    -> decltype(GetPointerToMatrixData(A), static_cast<unsigned int>(1))
  {
    auto pointerToData = GetPointerToMatrixData(A);
    using PointerType = decltype(pointerToData);
    using ValueTypeCV = typename std::remove_pointer<PointerType>::type;
    using ValueType = typename std::remove_cv<ValueTypeCV>::type;
    using EigenLibMatrixType = Eigen::Matrix<ValueType, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
    using EigenConstMatrixMap = Eigen::Map<const EigenLibMatrixType>;
    EigenConstMatrixMap inputMatrix(pointerToData, m_Dimension, m_Dimension);
    using EigenSolverType = Eigen::SelfAdjointEigenSolver<EigenLibMatrixType>;
    EigenSolverType solver(inputMatrix, Eigen::EigenvaluesOnly);
    auto            eigenValues = solver.eigenvalues();
    if (m_OrderEigenValues == EigenValueOrderEnum::OrderByMagnitude)
    {
      detail::sortEigenValuesByMagnitude(eigenValues, m_Dimension);
    }
    for (unsigned int i = 0; i < m_Dimension; ++i)
    {
      EigenValues[i] = eigenValues[i];
    }
    // No error code
    return 1;
  }
};

template <typename TMatrix, typename TVector, typename TEigenMatrix>
std::ostream &
operator<<(std::ostream & os, const SymmetricEigenAnalysis<TMatrix, TVector, TEigenMatrix> & s)
{
  os << "[ClassType: SymmetricEigenAnalysis]" << std::endl;
  os << "  Dimension : " << s.GetDimension() << std::endl;
  os << "  Order : " << s.GetOrder() << std::endl;
  os << "  OrderEigenValues: " << s.GetOrderEigenValues() << std::endl;
  os << "  OrderEigenMagnitudes: " << s.GetOrderEigenMagnitudes() << std::endl;
  os << "  UseEigenLibrary: " << s.GetUseEigenLibrary() << std::endl;
  return os;
}

template <unsigned int VDimension, typename TMatrix, typename TVector, typename TEigenMatrix = TMatrix>
class ITK_TEMPLATE_EXPORT SymmetricEigenAnalysisFixedDimension
{
public:
#if !defined(ITK_LEGACY_REMOVE)
  /** Enables reverse compatibility for enumeration values */
  using EigenValueOrderType = EigenValueOrderEnum;
#endif
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr EigenValueOrderEnum OrderByValue = EigenValueOrderEnum::OrderByValue;
  static constexpr EigenValueOrderEnum OrderByMagnitude = EigenValueOrderEnum::OrderByMagnitude;
  static constexpr EigenValueOrderEnum DoNotOrder = EigenValueOrderEnum::DoNotOrder;
#endif

  SymmetricEigenAnalysisFixedDimension() = default;
  ~SymmetricEigenAnalysisFixedDimension() = default;

  using MatrixType = TMatrix;
  using EigenMatrixType = TEigenMatrix;
  using VectorType = TVector;

  /** Compute Eigen values of A
   * A is any type that overloads the [][] operator and contains the
   * symmetric matrix. In practice only the upper triangle of the
   * matrix will be accessed. (Both itk::Matrix and vnl_matrix
   * overload [][] operator.)
   *
   * 'EigenValues' is any type that overloads the [] operator and will contain
   * the eigen values.
   *
   * No size checking is performed. A is expected to be a square matrix of size
   * VDimension.  'EigenValues' is expected to be of length VDimension.
   * The matrix is not checked to see if it is symmetric.
   */
  unsigned int
  ComputeEigenValues(const TMatrix & A, TVector & EigenValues) const
  {
    return ComputeEigenValuesWithEigenLibraryImpl(A, EigenValues, true);
  }

  /** Compute Eigen values and vectors of A
   * A is any type that overloads the [][] operator and contains the
   * symmetric matrix. In practice only the upper triangle of the
   * matrix will be accessed. (Both itk::Matrix and vnl_matrix
   * overload [][] operator.)
   *
   * 'EigenValues' is any type that overloads the [] operator and will contain
   * the eigen values.
   *
   * 'EigenVectors' is any type that provides access to its elements with the
   * [][] operator. It is expected be of size VDimension * VDimension.
   *
   * No size checking is performed. A is expected to be a square matrix of size
   * VDimension.  'EigenValues' is expected to be of length VDimension.
   * The matrix is not checked to see if it is symmetric.
   *
   * Each row of the matrix 'EigenVectors' represents an eigen vector. (unlike MATLAB
   * where the columns of the [EigenVectors, EigenValues] = eig(A) contains the
   * eigenvectors).
   */
  unsigned int
  ComputeEigenValuesAndVectors(const TMatrix & A, TVector & EigenValues, TEigenMatrix & EigenVectors) const
  {
    return ComputeEigenValuesAndVectorsWithEigenLibraryImpl(A, EigenValues, EigenVectors, true);
  }

  void
  SetOrderEigenValues(const bool b)
  {
    if (b)
    {
      m_OrderEigenValues = EigenValueOrderEnum::OrderByValue;
    }
    else
    {
      m_OrderEigenValues = EigenValueOrderEnum::DoNotOrder;
    }
  }
  bool
  GetOrderEigenValues() const
  {
    return (m_OrderEigenValues == EigenValueOrderEnum::OrderByValue);
  }
  void
  SetOrderEigenMagnitudes(const bool b)
  {
    if (b)
    {
      m_OrderEigenValues = EigenValueOrderEnum::OrderByMagnitude;
    }
    else
    {
      m_OrderEigenValues = EigenValueOrderEnum::DoNotOrder;
    }
  }
  bool
  GetOrderEigenMagnitudes() const
  {
    return (m_OrderEigenValues == EigenValueOrderEnum::OrderByMagnitude);
  }
  constexpr unsigned int
  GetOrder() const
  {
    return VDimension;
  }
  constexpr unsigned int
  GetDimension() const
  {
    return VDimension;
  }
  constexpr bool
  GetUseEigenLibrary() const
  {
    return true;
  }

private:
  EigenValueOrderEnum m_OrderEigenValues{ EigenValueOrderEnum::OrderByValue };

  /* Helper to get the matrix value type for EigenLibMatrix typename.
   *
   * If the TMatrix is vnl, the type is in element_type.
   * In TMatrix is itk::Matrix, or any itk::FixedArray is in ValueType.
   *
   * To use this function:
   * using ValueType = decltype(this->GetMatrixType(true));
   */
  template <typename QMatrix = TMatrix>
  auto
  GetMatrixValueType(bool) const -> typename QMatrix::element_type
  {
    return QMatrix::element_type();
  }
  template <typename QMatrix = TMatrix>
  auto
  GetMatrixValueType(bool) const -> typename QMatrix::ValueType
  {
    return QMatrix::ValueType();
  }

  /* Implementation detail using EigenLib that do not peform a copy.
   * It needs the existence of a pointer to matrix data. \sa GetPointerToMatrixData
   * If new types want to use this method, an appropriate overload of GetPointerToMatrixData
   * should be included.
   *
   * @param (bool) implementation detail argument making this implementation the most favourable
   *   to be chosen from all the alternative implementations.
   *
   * @return an unsigned int with no information value (no error code in EigenLib) */
  template <typename QMatrix>
  auto
  ComputeEigenValuesAndVectorsWithEigenLibraryImpl(const QMatrix & A,
                                                   TVector &       EigenValues,
                                                   TEigenMatrix &  EigenVectors,
                                                   bool) const
    -> decltype(GetPointerToMatrixData(A), static_cast<unsigned int>(1))
  {
    auto pointerToData = GetPointerToMatrixData(A);
    using PointerType = decltype(pointerToData);
    using ValueTypeCV = typename std::remove_pointer<PointerType>::type;
    using ValueType = typename std::remove_cv<ValueTypeCV>::type;
    using EigenLibMatrixType = Eigen::Matrix<ValueType, VDimension, VDimension, Eigen::RowMajor>;
    using EigenConstMatrixMap = Eigen::Map<const EigenLibMatrixType>;
    EigenConstMatrixMap inputMatrix(pointerToData);
    using EigenSolverType = Eigen::SelfAdjointEigenSolver<EigenLibMatrixType>;
    EigenSolverType solver(inputMatrix); // Computes EigenValues and EigenVectors
    const auto &    eigenValues = solver.eigenvalues();
    /* Column  k  of the returned matrix is an eigenvector corresponding to
     * eigenvalue number $ k $ as returned by eigenvalues().
     * The eigenvectors are normalized to have (Euclidean) norm equal to one. */
    const auto & eigenVectors = solver.eigenvectors();
    if (m_OrderEigenValues == EigenValueOrderEnum::OrderByMagnitude)
    {
      auto copyEigenValues = eigenValues;
      auto copyEigenVectors = eigenVectors;
      auto indicesSortPermutations = detail::sortEigenValuesByMagnitude(copyEigenValues, VDimension);
      detail::permuteColumnsWithSortIndices(copyEigenVectors, indicesSortPermutations);
      for (unsigned int row = 0; row < VDimension; ++row)
      {
        EigenValues[row] = copyEigenValues[row];
        for (unsigned int col = 0; col < VDimension; ++col)
        {
          EigenVectors[row][col] = copyEigenVectors(col, row);
        }
      }
    }
    else
    {
      for (unsigned int row = 0; row < VDimension; ++row)
      {
        EigenValues[row] = eigenValues[row];
        for (unsigned int col = 0; col < VDimension; ++col)
        {
          EigenVectors[row][col] = eigenVectors(col, row);
        }
      }
    }
    // No error code
    return 1;
  }

  /* Implementation detail using EigenLib that performs a copy of the input matrix.
   *
   * @param (long) implementation detail argument making this implementation less favourable
   *   to be chosen if alternatives are available.
   *
   * @return an unsigned int with no information value (no error code in EigenLib) */
  template <typename QMatrix>
  auto
  ComputeEigenValuesAndVectorsWithEigenLibraryImpl(const QMatrix & A,
                                                   TVector &       EigenValues,
                                                   TEigenMatrix &  EigenVectors,
                                                   long) const -> decltype(static_cast<unsigned int>(1))
  {
    using ValueType = decltype(GetMatrixValueType(true));
    using EigenLibMatrixType = Eigen::Matrix<ValueType, VDimension, VDimension, Eigen::RowMajor>;
    EigenLibMatrixType inputMatrix;
    for (unsigned int row = 0; row < VDimension; ++row)
    {
      for (unsigned int col = 0; col < VDimension; ++col)
      {
        inputMatrix(row, col) = A(row, col);
      }
    }
    using EigenSolverType = Eigen::SelfAdjointEigenSolver<EigenLibMatrixType>;
    EigenSolverType solver(inputMatrix); // Computes EigenValues and EigenVectors
    const auto &    eigenValues = solver.eigenvalues();
    /* Column  k  of the returned matrix is an eigenvector corresponding to
     * eigenvalue number $ k $ as returned by eigenvalues().
     * The eigenvectors are normalized to have (Euclidean) norm equal to one. */
    const auto & eigenVectors = solver.eigenvectors();

    if (m_OrderEigenValues == EigenValueOrderEnum::OrderByMagnitude)
    {
      auto copyEigenValues = eigenValues;
      auto copyEigenVectors = eigenVectors;
      auto indicesSortPermutations = detail::sortEigenValuesByMagnitude(copyEigenValues, VDimension);
      detail::permuteColumnsWithSortIndices(copyEigenVectors, indicesSortPermutations);

      for (unsigned int row = 0; row < VDimension; ++row)
      {
        EigenValues[row] = copyEigenValues[row];
        for (unsigned int col = 0; col < VDimension; ++col)
        {
          EigenVectors[row][col] = copyEigenVectors(col, row);
        }
      }
    }
    else
    {
      for (unsigned int row = 0; row < VDimension; ++row)
      {
        EigenValues[row] = eigenValues[row];
        for (unsigned int col = 0; col < VDimension; ++col)
        {
          EigenVectors[row][col] = eigenVectors(col, row);
        }
      }
    }
    // No error code
    return 1;
  }

  /* Implementation detail using EigenLib that performs a copy of the input matrix.
   *
   * @param (long) implementation detail argument making this implementation less favourable
   *   to be chosen if alternatives are available.
   *
   * @return an unsigned int with no information value (no error code in EigenLib) */
  template <typename QMatrix>
  auto
  ComputeEigenValuesWithEigenLibraryImpl(const QMatrix & A, TVector & EigenValues, long) const
    -> decltype(static_cast<unsigned int>(1))
  {
    using ValueType = decltype(GetMatrixValueType(true));
    using EigenLibMatrixType = Eigen::Matrix<ValueType, VDimension, VDimension, Eigen::RowMajor>;
    EigenLibMatrixType inputMatrix;
    for (unsigned int row = 0; row < VDimension; ++row)
    {
      for (unsigned int col = 0; col < VDimension; ++col)
      {
        inputMatrix(row, col) = A(row, col);
      }
    }
    using EigenSolverType = Eigen::SelfAdjointEigenSolver<EigenLibMatrixType>;
    EigenSolverType solver(inputMatrix, Eigen::EigenvaluesOnly);
    auto            eigenValues = solver.eigenvalues();
    if (m_OrderEigenValues == EigenValueOrderEnum::OrderByMagnitude)
    {
      detail::sortEigenValuesByMagnitude(eigenValues, VDimension);
    }
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      EigenValues[i] = eigenValues[i];
    }

    // No error code
    return 1;
  }

  /* Implementation detail using EigenLib that do not peform a copy.
   * It needs the existence of a pointer to matrix data. \sa GetPointerToMatrixData
   * If new types want to use this method, an appropriate overload of GetPointerToMatrixData
   * should be included.
   *
   * @param (bool) implementation detail argument making this implementation the most favourable
   *   to be chosen from all the alternative implementations.
   *
   * @return an unsigned int with no information value (no error code in EigenLib) */
  template <typename QMatrix>
  auto
  ComputeEigenValuesWithEigenLibraryImpl(const QMatrix & A, TVector & EigenValues, bool) const
    -> decltype(GetPointerToMatrixData(A), static_cast<unsigned int>(1))
  {
    auto pointerToData = GetPointerToMatrixData(A);
    using PointerType = decltype(pointerToData);
    using ValueTypeCV = typename std::remove_pointer<PointerType>::type;
    using ValueType = typename std::remove_cv<ValueTypeCV>::type;
    using EigenLibMatrixType = Eigen::Matrix<ValueType, VDimension, VDimension, Eigen::RowMajor>;
    using EigenConstMatrixMap = Eigen::Map<const EigenLibMatrixType>;
    EigenConstMatrixMap inputMatrix(pointerToData);
    using EigenSolverType = Eigen::SelfAdjointEigenSolver<EigenLibMatrixType>;
    EigenSolverType solver(inputMatrix, Eigen::EigenvaluesOnly);
    auto            eigenValues = solver.eigenvalues();
    if (m_OrderEigenValues == EigenValueOrderEnum::OrderByMagnitude)
    {
      detail::sortEigenValuesByMagnitude(eigenValues, VDimension);
    }
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      EigenValues[i] = eigenValues[i];
    }
    // No error code
    return 1;
  }
};

template <unsigned int VDimension, typename TMatrix, typename TVector, typename TEigenMatrix>
std::ostream &
operator<<(std::ostream &                                                                           os,
           const SymmetricEigenAnalysisFixedDimension<VDimension, TMatrix, TVector, TEigenMatrix> & s)
{
  os << "[ClassType: SymmetricEigenAnalysisFixedDimension]" << std::endl;
  os << "  Dimension : " << s.GetDimension() << std::endl;
  os << "  Order : " << s.GetOrder() << std::endl;
  os << "  OrderEigenValues: " << s.GetOrderEigenValues() << std::endl;
  os << "  OrderEigenMagnitudes: " << s.GetOrderEigenMagnitudes() << std::endl;
  os << "  UseEigenLibrary: " << s.GetUseEigenLibrary() << std::endl;
  return os;
}
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSymmetricEigenAnalysis.hxx"
#endif

#endif
