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

#ifndef itkFEMElement2DC0LinearLine_h
#define itkFEMElement2DC0LinearLine_h

#include "itkFEMElementStd.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element2DC0LinearLine
 * \brief 2-noded, linear, C0 continuous line element in 2D space.
 *  takes loads only along the length of the axis
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Element2DC0LinearLine : public ElementStd<2, 2>
{
public:
  /** Standard class typedefs. */
  typedef Element2DC0LinearLine    Self;
  typedef ElementStd<2, 2>         TemplatedParentClass;
  typedef TemplatedParentClass     Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element2DC0LinearLine, TemplatedParentClass);

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to numeric integration
   */

  enum { DefaultIntegrationOrder = 1 };

  /** Get the Integration point and weight */
  virtual void GetIntegrationPointAndWeight(unsigned int i,
                                            VectorType & pt,
                                            Float & w,
                                            unsigned int order) const ITK_OVERRIDE;

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

  /**
   * Get parametric/local coordinates given global coordinates. The function returns true if the
   * global coordinate is within the element else returns false.
   * For a line, line length*1e-4 is used as the tolerance
   */
  virtual bool GetLocalFromGlobalCoordinates(const VectorType & globalPt,
                                             VectorType & localPt) const ITK_OVERRIDE;

  /**
   * We need to provide our own implementation of calculating Jacobian,
   * because the element lives in 2D space and has only one dimension.
   * The default implementation of Jacobian in the Element base class
   * is not correct since it assumes that the number of element dimensions
   * is equal to the number of spatial dimensions.
   *
   * Jacobian is a scalar for this element.
   */
  virtual void Jacobian(const VectorType & pt, MatrixType & J, const MatrixType *pshapeD = ITK_NULLPTR) const ITK_OVERRIDE;

  /**
   * Distance of a point to a line.(Used in GetLocalFromGlobalCoordinates ).
   */
  Float DistanceToLine(const VectorType & x, const VectorType & p1, const VectorType & p2, Float & t,
                       VectorType & closestPoint) const;

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  virtual void PopulateEdgeIds(void) ITK_OVERRIDE; // HACK:  Should PopulateEdgeIds
                                      // be const or not in this
                                      // hierarchy. Sometimes it is,
                                      // sometimes it is not.

};
}
}  // end namespace itk::fem

#endif  // #ifndef itkFEMElement2DC0LinearLine_h
