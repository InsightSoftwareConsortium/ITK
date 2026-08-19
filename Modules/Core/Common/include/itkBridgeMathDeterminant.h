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
#ifndef itkBridgeMathDeterminant_h
#define itkBridgeMathDeterminant_h

#include "itkMacro.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_matrix_fixed.h"

#include "itk_eigen.h"
#include ITK_EIGEN(Dense)

namespace itk
{
// Forward declaration lets the Matrix overload parse under the circular include
// with itkMatrix.h; its body instantiates only where itk::Matrix is complete.
template <typename T, unsigned int VRows, unsigned int VColumns>
class Matrix;
} // namespace itk

namespace itk::bridge
{
namespace Math
{
namespace detail
{
// Eigen chooses direct cofactor formulas for VDim <= 4 and PartialPivLU beyond;
// the determinant is transpose-invariant, so the row-major map matches ITK
// storage without affecting the value.
template <unsigned int VDim, typename TReal>
TReal
DeterminantEigen(const TReal * inData)
{
  using RowMajor = Eigen::Matrix<TReal, VDim, VDim, Eigen::RowMajor>;
  return Eigen::Map<const RowMajor>(inData).determinant();
}

template <typename TReal>
TReal
DynamicDeterminantEigen(const TReal * inData, unsigned int n)
{
  // Eigen's Dynamic-sized determinant always uses LU; dispatch small sizes to
  // the compile-time direct formulas so runtime small matrices stay fast.
  switch (n)
  {
    case 1:
      return inData[0];
    case 2:
      return DeterminantEigen<2, TReal>(inData);
    case 3:
      return DeterminantEigen<3, TReal>(inData);
    case 4:
      return DeterminantEigen<4, TReal>(inData);
    default:
    {
      using RowMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
      return Eigen::Map<const RowMajor>(inData, n, n).determinant();
    }
  }
}
} // namespace detail

/** \brief Determinant of a square matrix, backed by Eigen.
 *
 * Eigen-backed replacement for vnl_determinant. Overloads accept an itk::Matrix,
 * a vnl_matrix_fixed, or a runtime vnl_matrix over a zero-copy Eigen::Map; no
 * Eigen type appears in the interface. Fixed sizes <= 4x4 -- which dominate ITK
 * usage -- use Eigen's direct cofactor formulas; larger sizes use PartialPivLU.
 *
 * \ingroup ITKCommon
 */
template <typename TReal, unsigned int VDim>
TReal
Determinant(const vnl_matrix_fixed<TReal, VDim, VDim> & A)
{
  return detail::DeterminantEigen<VDim, TReal>(A.data_block());
}

/** Determinant of a fixed-size square itk::Matrix. */
template <typename TReal, unsigned int VDim>
TReal
Determinant(const Matrix<TReal, VDim, VDim> & A)
{
  return Determinant<TReal, VDim>(A.GetVnlMatrix());
}

/** Determinant of a runtime-sized square vnl_matrix. */
template <typename TReal>
TReal
Determinant(const vnl_matrix<TReal> & A)
{
  const unsigned int rows = A.rows();
  if (rows != A.cols())
  {
    itkGenericExceptionMacro("itk::bridge::Math::Determinant requires a square matrix.");
  }
  return detail::DynamicDeterminantEigen<TReal>(A.data_block(), rows);
}

/** Non-square-typed fixed matrices (e.g. the 3 x PointDimension matrices in mesh
 * cells) forward to the runtime path, which requires the actual matrix be
 * square. The square overload above is more specialized and wins when the type
 * is square. */
template <typename TReal, unsigned int VRows, unsigned int VColumns>
TReal
Determinant(const vnl_matrix_fixed<TReal, VRows, VColumns> & A)
{
  return Determinant(A.as_ref());
}

} // namespace Math
} // namespace itk::bridge

#endif // itkBridgeMathDeterminant_h
