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

  /**
   * Get/Set the order of the Riesz transform.
   * The order modifies the number of components and the size of the steerable matrix.
   *
   * \sa GetComponents
   * \sa RieszFrequencyFunction
   */
  inline const unsigned int &
  GetOrder() const
  {
    return this->m_Order;
  }
  inline void
  SetOrder(const unsigned int & order)
  {
    this->m_Order = order;
    this->m_Components = itk::utils::ComputeNumberOfComponents(this->m_Order, VImageDimension);
    this->SetSize(this->m_Components, this->m_Components);
  }

  /**
   * Get the number of componets M of the steerable matrix.
   * The size of the steerable matrix is MxM.
   * The number of components is based on the m_Order of the Riesz
   * transform and the dimension.
   *
   * \sa RieszFrequencyFunction
   */
  inline const unsigned int &
  GetComponents() const
  {
    return this->m_Components;
  }

  /// Get/Set the spatial rotation matrix from which the steerable matrix is composed.
  inline const SpatialRotationMatrixType &
  GetSpatialRotationMatrix() const
  {
    return this->m_SpatialRotationMatrix;
  }

  inline void
  SetSpatialRotationMatrix(const SpatialRotationMatrixType & spatialRotationMatrix)
  {
    this->m_SpatialRotationMatrix = spatialRotationMatrix;
  }

  /**
   * Round the computed result S[i][j] to zero if it is close enough to zero. How close is controlled by this value.
   *
   * Computation requires a quite a few trivial multiplications, generating float errors.
   * It is specially noticiable because the domain of rotations is in a small interval around zero.
   * This tries to fix them, but there is no fit for all solution.
   *
   * The default in this class: itk::NumericTraits<ValueType>::epsilon()
   * The default in the function FloatAlmostEqual is : 0.1 * itk::NumericTraits<ValueType>::epsilon()
   *
   * \sa itk::Math::FloatAlmostEqual
   */
  inline const ValueType &
  GetMaxAbsoluteDifferenceCloseToZero() const
  {
    return this->m_MaxAbsoluteDifferenceCloseToZero;
  }
  inline void
  SetMaxAbsoluteDifferenceCloseToZero(const ValueType & maxAbsoluteDifference)
  {
    this->m_MaxAbsoluteDifferenceCloseToZero = maxAbsoluteDifference;
  }
  // ------- Debug Macro ------
  /// Get/Set Debug flag to print extra information.
  inline const bool &
  GetDebug() const
  {
    return this->m_Debug;
  }
  inline void
  SetDebug(const bool & boolean)
  {
    this->m_Debug = boolean;
  }
  inline void
  SetDebugOn()
  {
    this->m_Debug = true;
  }
  inline void
  SetDebugOff()
  {
    this->m_Debug = false;
  }

#ifdef ITK_USE_STRICT_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(ValueTypeIsFloatCheck, (Concept::IsFloatingPoint<ValueType>));
  // End concept checking
#endif

private:
  SpatialRotationMatrixType m_SpatialRotationMatrix;
  unsigned int              m_Order;
  unsigned int              m_Components;
  ValueType                 m_MaxAbsoluteDifferenceCloseToZero;
  bool                      m_Debug;

}; // end of class
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRieszRotationMatrix.hxx"
#endif

#endif
