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
  /** Standard class typedefs. */
  typedef Element2DC0LinearTriangular Self;
  typedef ElementStd<3, 2>            TemplatedParentClass;
  typedef TemplatedParentClass        Superclass;
  typedef SmartPointer<Self>          Pointer;
  typedef SmartPointer<const Self>    ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element2DC0LinearTriangular, TemplatedParentClass);

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to numeric integration
   */

  enum { DefaultIntegrationOrder = 1 };

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

  // Since the Jacobian is not quadratic, we need to provide our
  // own implementation of calculating the determinant and inverse.
  virtual Float JacobianDeterminant(const VectorType & pt, const MatrixType *pJ = ITK_NULLPTR) const ITK_OVERRIDE;

  /** Return the inverse of the Jacobian */
  virtual void JacobianInverse(const VectorType & pt, MatrixType & invJ, const MatrixType *pJ = ITK_NULLPTR) const ITK_OVERRIDE;

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
  virtual void PopulateEdgeIds(void) ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

};
}
}  // end namespace itk::fem

#endif  // #ifndef itkFEMElement2DC0LinearTriangular_h
