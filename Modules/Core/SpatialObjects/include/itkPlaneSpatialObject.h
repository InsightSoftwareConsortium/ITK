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
#ifndef itkPlaneSpatialObject_h
#define itkPlaneSpatialObject_h

#include "itkSpatialObject.h"
#include "itkAffineTransform.h"

namespace itk
{
/** \class PlaneSpatialObject
 * A plane spatial object is defined by two points
 * \brief
 * \ingroup ITKSpatialObjects
 *
 * \wiki
 * \wikiexample{SpatialObjects/PlaneSpatialObject,Plane spatial object}
 * \endwiki
 */

template< unsigned int TDimension = 3  >
class ITK_TEMPLATE_EXPORT PlaneSpatialObject:
  public SpatialObject< TDimension >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PlaneSpatialObject);

  /** Standard type alias */
  using Self = PlaneSpatialObject;
  using ScalarType = double;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;
  using Superclass = SpatialObject< TDimension >;
  using SuperclassPointer = SmartPointer< Superclass >;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using PointContainerType = VectorContainer< IdentifierType, PointType >;
  using PointContainerPointer = SmartPointer< PointContainerType >;
  using BoundingBoxType = typename Superclass::BoundingBoxType;

  static constexpr unsigned int NumberOfDimension = TDimension;

  itkNewMacro(Self);
  itkTypeMacro(PlaneSpatialObject, SpatialObject);

  /** Returns a degree of membership to the object.
   *  That's useful for fuzzy objects. */
  bool ValueAt(const PointType & point, double & value,
                       unsigned int depth = 0, char *name = nullptr) const override;

  /** return ture if the object provides a method to evaluate the value
   * at the specified point, else otherwise. */
  bool IsEvaluableAt(const PointType & point,
                             unsigned int depth = 0, char *name = nullptr) const override;

  /** Test whether a point is inside or outside the object */
  bool IsInside(const PointType & point,
                        unsigned int depth, char *name) const override;

  /** Test whether a point is inside or outside the object
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */
  virtual bool IsInside(const PointType & point) const;

  /** provide a method to get the boundaries of
  *  a specific object. Basically, this function need to be called
  *  every time one of the object component is changed. */
  bool ComputeLocalBoundingBox() const override;

  itkSetMacro(LowerPoint, PointType);
  itkSetMacro(UpperPoint, PointType);
  itkGetMacro(LowerPoint, PointType);
  itkGetMacro(UpperPoint, PointType);

protected:
  PlaneSpatialObject();
  ~PlaneSpatialObject() override = default;

  PointType m_LowerPoint;
  PointType m_UpperPoint;

  /** Print the object informations in a stream. */
  void PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPlaneSpatialObject.hxx"
#endif

#endif // itkPlaneSpatialObject_h
