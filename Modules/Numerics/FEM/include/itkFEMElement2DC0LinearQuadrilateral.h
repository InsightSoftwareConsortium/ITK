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

#ifndef itkFEMElement2DC0LinearQuadrilateral_h
#define itkFEMElement2DC0LinearQuadrilateral_h

#include "itkFEMElementStd.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element2DC0LinearQuadrilateral
 * \brief 4-noded, linear, C0 continuous finite element in 2D space.
 * \ingroup ITKFEM
 *
 * The ordering of the nodes is counter clockwise. That is the nodes
 * should be defined in the following order:
 *
 *  3 (0,1)                  2 (1,1)
 *  *------------------------*
 *  |                        |
 *  |                        |
 *  |                        |
 *  |                        |
 *  |                        |
 *  |                        |
 *  *------------------------*
 *  0 (0,0)                  1 (0,1)
 *
 * This is an abstract class. Specific concrete implementations of this
 * It must be combined with the physics component of the problem.
 * This has already been done in the following classes:
 *
 * \sa Element2DC0LinearQuadrilateralMembrane
 * \sa Element2DC0LinearQuadrilateralStrain
 * \sa Element2DC0LinearQuadrilateralStress
 */

class ITKFEM_EXPORT Element2DC0LinearQuadrilateral : public ElementStd<4, 2>
{
public:
  /** Standard class type aliases. */
  using Self = Element2DC0LinearQuadrilateral;
  using TemplatedParentClass = ElementStd<4, 2>;
  using Superclass = TemplatedParentClass;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element2DC0LinearQuadrilateral, TemplatedParentClass);

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to numeric integration
   */

  enum
  {
    DefaultIntegrationOrder = 2
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

  /** Interpolation Functions */
  void
  InterpolationFunctions(const VectorType & pcoords, VectorType & sf) const;

  /** Interpolation Derivatives */
  void
  InterpolationDerivs(const VectorType & pcoords, VectorType & derivs) const;

  /** Return the determinate of a 2x2 matrix */
  Float
  Determinant2x2(const VectorType & c1, const VectorType & c2) const;

protected:
  void
  PopulateEdgeIds() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
};
} // end namespace fem
} // end namespace itk

#endif // itkFEMElement2DC0LinearQuadrilateral_h
