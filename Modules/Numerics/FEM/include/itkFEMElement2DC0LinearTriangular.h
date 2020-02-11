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

#ifndef itkFEMElement2DC0LinearTriangular_h
#define itkFEMElement2DC0LinearTriangular_h

#include "itkFEMElementStd.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element2DC0LinearTriangular
 * \brief 3-noded, linear, C0 continuous finite element in 2D space.
 * \ingroup ITKFEM
 *
 * The ordering of the nodes is counter clockwise. That is the nodes
 * should be defined in the following order:
 *
 *  (0,1)
 *  2
 *  *
 *  |\
 *  | \
 *  |  \
 *  |   \
 *  |    \
 *  |     \
 *  *------*
 *  0      1
 *  (0,0)  (0,1)
 *
 * This is an abstract class. Specific concrete implementations of this
 * It must be combined with the physics component of the problem.
 * This has already been done in the following classes:
 *
 * \sa Element2DC0LinearTriangular
 * \sa Element2DC0LinearTriangularStrain
 * \sa Element2DC0LinearTriangularStress
 */
class ITKFEM_EXPORT Element2DC0LinearTriangular : public ElementStd<3, 2>
{
public:
  /** Standard class type aliases. */
  using Self = Element2DC0LinearTriangular;
  using TemplatedParentClass = ElementStd<3, 2>;
  using Superclass = TemplatedParentClass;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element2DC0LinearTriangular, TemplatedParentClass);

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to numeric integration
   */

  enum
  {
    DefaultIntegrationOrder = 1
  };

  /** Get the Integration point and weight */
  void
  GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order) const override;

  /** Get the number of integration points */
  unsigned int
  GetNumberOfIntegrationPoints(unsigned int order) const override;

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to the geometry of an element
   */

  /** Return the shape functions used to interpolate across the element */
  VectorType
  ShapeFunctions(const VectorType & pt) const override;

  /** Return the shape functions derivatives in the shapeD matrix */
  void
  ShapeFunctionDerivatives(const VectorType & pt, MatrixType & shapeD) const override;

  /** Convert from global to local coordinates */
  bool
  GetLocalFromGlobalCoordinates(const VectorType & globalPt, VectorType & localPt) const override;

  // Since the Jacobian is not quadratic, we need to provide our
  // own implementation of calculating the determinant and inverse.
  Float
  JacobianDeterminant(const VectorType & pt, const MatrixType * pJ = nullptr) const override;

  /** Return the inverse of the Jacobian */
  void
  JacobianInverse(const VectorType & pt, MatrixType & invJ, const MatrixType * pJ = nullptr) const override;

  /**
   * Constants for integration rules.
   */
  static const Float trigGaussRuleInfo[6][7][4];

  /**
   * Array that holds number of integration point for each order
   * of numerical integration.
   */
  static const unsigned int Nip[6];

protected:
  void
  PopulateEdgeIds() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace fem
} // end namespace itk

#endif // itkFEMElement2DC0LinearTriangular_h
