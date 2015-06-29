/*=========================================================================
*
* Copyright Insight Software Consortium
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0.txt
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
*=========================================================================*/

#ifndef itkFEMElement3DC0LinearTriangular_h
#define itkFEMElement3DC0LinearTriangular_h

#include "itkFEMElementStd.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element3DC0LinearTriangular
 * \brief 3-noded, linear, C0 continuous finite element in 2D space.
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
 * \ingroup ITKFEM
 */

class ITKFEM_EXPORT Element3DC0LinearTriangular : public ElementStd<3, 3>
{
public:
  /** Standard class typedefs. */
  typedef Element3DC0LinearTriangular Self;
  typedef ElementStd<3, 3>            TemplatedParentClass;
  typedef TemplatedParentClass        Superclass;
  typedef SmartPointer<Self>          Pointer;
  typedef SmartPointer<const Self>    ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element3DC0LinearTriangular, TemplatedParentClass);

// ////////////////////////////////////////////////////////////////////////
/*
 * Methods related to numeric integration
 */

  enum { DefaultIntegrationOrder = 1 };

  /** Get the Integration point and weight */
  virtual void GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order) const ITK_OVERRIDE;

  /** Get the number of integration points */
  virtual unsigned int GetNumberOfIntegrationPoints(unsigned int order) const ITK_OVERRIDE;

  // ////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to the geometry of an element
   */

  /** Return the shape functions used to interpolate across the element */
  virtual VectorType ShapeFunctions(const VectorType & pt) const ITK_OVERRIDE;

  /** Return the shape functions derivatives in the shapeD matrix */
  virtual void ShapeFunctionDerivatives(const VectorType & pt, MatrixType & shapeD) const ITK_OVERRIDE;

  /** Convert from global to local coordinates */
  virtual bool GetLocalFromGlobalCoordinates(const VectorType & globalPt, VectorType & localPt) const ITK_OVERRIDE;

  /** Compute the determinate of the Jacobian matrix */
  virtual Float JacobianDeterminant(const VectorType & pt, const MatrixType *pJ = ITK_NULLPTR) const ITK_OVERRIDE;

  /** Compute the inverse of the Jacobian matrix */
  virtual void JacobianInverse(const VectorType & pt, MatrixType & invJ, const MatrixType *pJ = ITK_NULLPTR) const ITK_OVERRIDE;

  /** Define the edges and nodes that correspond to the edges */
  virtual void PopulateEdgeIds() ITK_OVERRIDE;

  /**
  * Normal of the triangle element
  */
  void ComputeNormalDirection(const VectorType & v1, const VectorType & v2, const VectorType & v3,
                              VectorType & n) const;

  /**
  * Project the point x onto the plane containing the triangle element
  */
  void GeneralizedProjectPoint(const VectorType & x, const VectorType & origin, const VectorType & normal,
                               VectorType & xproj) const;

  /** Return the determinate of a 2x2 matrix */
  itk::fem::Element::Float Determinant2x2(const VectorType & c1, const VectorType & c2) const;

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
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

};
}
}  // end namespace itk::fem

#endif  // #ifndef itkFEMElement3DC0LinearTriangular_h
