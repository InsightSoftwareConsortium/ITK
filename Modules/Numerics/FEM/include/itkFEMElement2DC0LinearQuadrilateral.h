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
  /** Standard class typedefs. */
  typedef Element2DC0LinearQuadrilateral Self;
  typedef ElementStd<4, 2>               TemplatedParentClass;
  typedef TemplatedParentClass           Superclass;
  typedef SmartPointer<Self>             Pointer;
  typedef SmartPointer<const Self>       ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element2DC0LinearQuadrilateral, TemplatedParentClass);

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to numeric integration
   */

  enum { DefaultIntegrationOrder = 2 };

  /** Get the Integration point and weight */
  virtual void GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order) const ITK_OVERRIDE;

  /** Get the number of integration points */
  virtual unsigned int GetNumberOfIntegrationPoints(unsigned int order) const ITK_OVERRIDE;

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to the geometry of an element
   */

  /** Return the shape functions used to interpolate across the element */
  virtual VectorType ShapeFunctions(const VectorType & pt) const ITK_OVERRIDE;

  /** Return the shape functions derivatives in the shapeD matrix */
  virtual void ShapeFunctionDerivatives(const VectorType & pt, MatrixType & shapeD) const ITK_OVERRIDE;

  /** Convert from global to local coordinates */
  virtual bool GetLocalFromGlobalCoordinates(const VectorType & globalPt, VectorType & localPt) const ITK_OVERRIDE;

  /** Interpolation Functions */
  void InterpolationFunctions( const VectorType & pcoords, VectorType & sf) const;

  /** Interpolation Derivatives */
  void InterpolationDerivs(const VectorType & pcoords, VectorType & derivs) const;

  /** Return the determinate of a 2x2 matrix */
  Float Determinant2x2(const VectorType & c1, const VectorType & c2) const;

protected:
  virtual void PopulateEdgeIds(void) ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

private:

};
}
}  // end namespace itk::fem

#endif  // #ifndef itkFEMElement2DC0LinearQuadrilateral_h
