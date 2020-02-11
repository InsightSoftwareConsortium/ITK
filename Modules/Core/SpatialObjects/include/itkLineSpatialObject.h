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
#ifndef itkLineSpatialObject_h
#define itkLineSpatialObject_h

#include <list>

#include "itkPointBasedSpatialObject.h"
#include "itkLineSpatialObjectPoint.h"

namespace itk
{
/**
 * \class LineSpatialObject
 * \brief Representation of a Line based on the spatial object classes.
 *
 * The Line is basically defined by a set of points.
 *
 * \sa LineSpatialObjectPoint
 * \ingroup ITKSpatialObjects
 *
 */

template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT LineSpatialObject
  : public PointBasedSpatialObject<TDimension, LineSpatialObjectPoint<TDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LineSpatialObject);

  using Self = LineSpatialObject;
  using Superclass = PointBasedSpatialObject<TDimension, LineSpatialObjectPoint<TDimension>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using ScalarType = double;

  using LinePointType = LineSpatialObjectPoint<TDimension>;
  using LinePointListType = std::vector<LinePointType>;

  using SpatialObjectPointType = typename Superclass::SpatialObjectPointType;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using PointContainerType = VectorContainer<IdentifierType, PointType>;
  using PointContainerPointer = SmartPointer<PointContainerType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(LineSpatialObject, PointBasedSpatialObject);

  /** Returns true if the line is evaluable at the requested point,
   *  false otherwise. */
  bool
  IsInsideInObjectSpace(const PointType & point) const override;

  /* Avoid hiding the overload that supports depth and name arguments */
  using Superclass::IsInsideInObjectSpace;

protected:
  LineSpatialObject();
  ~LineSpatialObject() override = default;

  /** Method to print the object. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer
  InternalClone() const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLineSpatialObject.hxx"
#endif

#endif // itkLineSpatialObject_h
