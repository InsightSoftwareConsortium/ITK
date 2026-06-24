/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkQRDecomposition_h
#define itkQRDecomposition_h

#include "itkMacro.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "itk_eigen.h"
#include ITK_EIGEN(Dense)

namespace itk
{

/** \class QRDecomposition
 * \brief QR decomposition A = Q R of a real or complex matrix, backed by Eigen.
 *
 * Eigen-backed replacement for vnl_qr. The factors Q (orthonormal/unitary) and
 * R (upper triangular) are computed once in the constructor via Eigen's
 * HouseholderQR and stored as vnl matrices, so no Eigen type appears in the
 * public interface. Q/R follow Eigen's sign convention; rely on the QR
 * identities Q R == A and Q^* Q == I rather than on specific factor signs.
 *
 * \ingroup ITKCommon
 */
template <typename T>
class QRDecomposition
{
public:
  using MatrixType = vnl_matrix<T>;
  using VectorType = vnl_vector<T>;

  explicit QRDecomposition(const MatrixType & A)
  {
    itkAssertOrThrowMacro(A.rows() > 0 && A.cols() > 0, "QRDecomposition requires a non-empty matrix");
    using RowMajor = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
    using ColMajor = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>;
    const unsigned int rows = A.rows();
    const unsigned int cols = A.cols();

    Eigen::Map<const RowMajor>           aMap(A.data_block(), rows, cols);
    const Eigen::HouseholderQR<ColMajor> qr(aMap);

    const ColMajor qFull = qr.householderQ();
    const ColMajor rFull = qr.matrixQR().template triangularView<Eigen::Upper>();

    m_Q.set_size(rows, rows);
    Eigen::Map<RowMajor>(m_Q.data_block(), rows, rows) = qFull;
    m_R.set_size(rows, cols);
    Eigen::Map<RowMajor>(m_R.data_block(), rows, cols) = rFull;

    m_Square = (rows == cols);
    m_Determinant = m_Square ? static_cast<T>(aMap.determinant()) : T{};
  }

  /** Orthonormal/unitary factor Q (rows x rows). */
  const MatrixType &
  GetQ() const
  {
    return m_Q;
  }

  /** Upper-triangular factor R (rows x cols). */
  const MatrixType &
  GetR() const
  {
    return m_R;
  }

  /** Solve A x = b (least-squares for an overdetermined A) using x = R^-1 Q^* b.
   *  A must have full column rank; a rank-deficient R yields a non-finite x. */
  VectorType
  Solve(const VectorType & b) const
  {
    using RowMajor = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
    using Vector = Eigen::Matrix<T, Eigen::Dynamic, 1>;
    const unsigned int rows = m_Q.rows();
    const unsigned int cols = m_R.cols();
    itkAssertOrThrowMacro(b.size() == rows, "QRDecomposition::Solve requires b length to match the row count");
    if (cols > rows)
    {
      itkGenericExceptionMacro("QRDecomposition::Solve supports square or overdetermined systems only (rows >= cols)");
    }

    Eigen::Map<const RowMajor> qMap(m_Q.data_block(), rows, rows);
    Eigen::Map<const RowMajor> rMap(m_R.data_block(), rows, cols);
    Eigen::Map<const Vector>   bMap(b.data_block(), rows);

    const Vector qtb = qMap.adjoint() * bMap;
    const Vector x = rMap.topLeftCorner(cols, cols).template triangularView<Eigen::Upper>().solve(qtb.head(cols));

    VectorType out(cols);
    Eigen::Map<Vector>(out.data_block(), cols) = x;
    return out;
  }

  /** Solve A X = B for a multi-column right-hand side. Each column of B is an
   *  independent system sharing the single stored factorization, so the result
   *  X has one solution column per column of B (X.cols() == B.cols()); column j
   *  equals Solve(B.get_column(j)). B.rows() must match the row count and A must
   *  have full column rank, as in the vector overload. */
  MatrixType
  Solve(const MatrixType & B) const
  {
    using RowMajor = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
    const unsigned int rows = m_Q.rows();
    const unsigned int cols = m_R.cols();
    const unsigned int nrhs = B.cols();
    itkAssertOrThrowMacro(B.rows() == rows, "QRDecomposition::Solve requires B row count to match the row count");
    if (cols > rows)
    {
      itkGenericExceptionMacro("QRDecomposition::Solve supports square or overdetermined systems only (rows >= cols)");
    }

    Eigen::Map<const RowMajor> qMap(m_Q.data_block(), rows, rows);
    Eigen::Map<const RowMajor> rMap(m_R.data_block(), rows, cols);
    Eigen::Map<const RowMajor> bMap(B.data_block(), rows, nrhs);

    const RowMajor qtb = qMap.adjoint() * bMap;
    const RowMajor x = rMap.topLeftCorner(cols, cols).template triangularView<Eigen::Upper>().solve(qtb.topRows(cols));

    MatrixType out(cols, nrhs);
    Eigen::Map<RowMajor>(out.data_block(), cols, nrhs) = x;
    return out;
  }

  /** Inverse of a square A, computed as the solution of A X = I. Requires A to
   *  be square and full rank; a rank-deficient A yields a non-finite result. */
  MatrixType
  Inverse() const
  {
    const unsigned int rows = m_Q.rows();
    itkAssertOrThrowMacro(rows == m_R.cols(), "QRDecomposition::Inverse requires a square matrix");
    MatrixType identity(rows, rows);
    identity.set_identity();
    return this->Solve(identity);
  }

  /** Determinant of a square A (zero-initialized value for non-square A). */
  T
  GetDeterminant() const
  {
    return m_Determinant;
  }

private:
  MatrixType m_Q;
  MatrixType m_R;
  T          m_Determinant{};
  bool       m_Square{ false };
};

} // namespace itk

#endif // itkQRDecomposition_h
