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
#ifndef itkRieszRotationMatrix_h
#define itkRieszRotationMatrix_h
#include "itkVariableSizeMatrix.h"
#include <vector>
#include "itkMatrix.h"
#include "itkRieszUtilities.h"

namespace itk
{
/** \class RieszRotationMatrix
 * Get a steerable matrix for the Riesz transform: S_r
 * from a rotation matrix R in the spatial domain,
 * and the order of the Riesz transform (T)
 * T f( R*x ) = T_{S_r} f(x)
 *
 * S_r is a MxM matrix, where M = p(N,d) is the number of
 * components of a riesz transform of order N and dimension d.
 * M := p(N,d) = \frac{(N+d-1)!}{(d-1)! N!}
 *
 * The rotation matrix is a dxd matrix.
 *
 * \sa RieszFrequencyFunction
 * \sa RieszFrequencyFilterBankGenerator
 *
 * \ingroup IsotropicWavelets
 */

template <typename T = double, unsigned int VImageDimension = 3>
class RieszRotationMatrix : public itk::VariableSizeMatrix<T>
{
public:
  /** Standard typedefs */
  typedef RieszRotationMatrix        Self;
  typedef itk::VariableSizeMatrix<T> Superclass;

  /** Component value type */
  typedef typename Superclass::ValueType                   ValueType;
  typedef typename Superclass::InternalMatrixType          InternalMatrixType;
  typedef itk::Matrix<T, VImageDimension, VImageDimension> SpatialRotationMatrixType;

  /** Matrix by std::vector multiplication.  */
  std::vector<T>
  operator*(const std::vector<T> & vector) const;

  /**
   * Multi-index notation
   * S[n = (n1,...,nd)][m = (m1,...,md)]
   *
   * 2D: order:1, components:2
   * S_{|n| = 1,|m| = 1} = S_{2x2} =:
         (1,0),(1,0)  (1,0),(0,1)
         (0,1),(1,0)  (0,1),(0,1)

   * 3D: order:1, components:3
   * S_{|n| = 1,|m| = 1} = S_{3x3} =:
         (1,0,0),(1,0,0)  (1,0,0),(0,1,0)  (1,0,0),(0,0,1)
         (0,1,0),(1,0,0)  (0,1,0),(0,1,0)  (0,1,0),(0,0,1)
         (0,0,1),(1,0,0)  (0,0,1),(0,1,0)  (0,0,1),(0,0,1)

   * S[n = (n1,...,nd)][m = (m1,...,md)] =
   *  sqrt(\frac{m!}{n!}) \sum_{|k1| = n1} \cdots \sum_{|kd| = nd}
   *  \delta_{k1 + k2 + k3, m} x
   *  \\frac{n!}{k1! \cdots kd!} r_1^{k_1} \cdots r_d^{k_d}
   *
   * The indices are ordered in descending order:
   * For example, for order = 2:
   * ( 2, 0, 0, )( 1, 1, 0, )( 1, 0, 1, )( 0, 2, 0, )( 0, 1, 1, )( 0, 0, 2, )

   */
  const InternalMatrixType &
  ComputeSteerableMatrix();

  /** Default constructor. */
  RieszRotationMatrix();
  /** Copy constructor. */
  RieszRotationMatrix(const Self & matrix);
  /** Compute constructor. */
  RieszRotationMatrix(const SpatialRotationMatrixType & spatialRotationMatrix, const unsigned int & order);

  inline const unsigned int &
  GetOrder() const
  {
    return m_Order;
  }
  inline void
  SetOrder(const unsigned int & order)
  {
    this->m_Order = order;
    this->m_Components = itk::utils::ComputeNumberOfComponents(this->m_Order, VImageDimension);
    this->SetSize(this->m_Components, this->m_Components);
  }

  inline const unsigned int &
  GetComponents() const
  {
    return m_Components;
  }

  inline const SpatialRotationMatrixType &
  GetSpatialRotationMatrix() const
  {
    return m_SpatialRotationMatrix;
  }

  inline void
  SetSpatialRotationMatrix(const SpatialRotationMatrixType & spatialRotationMatrix)
  {
    this->m_SpatialRotationMatrix = spatialRotationMatrix;
  }

private:
  SpatialRotationMatrixType m_SpatialRotationMatrix;
  unsigned int              m_Order;
  unsigned int              m_Components;

}; // end of class
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRieszRotationMatrix.hxx"
#endif

#endif
