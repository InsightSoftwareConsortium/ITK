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
#ifndef itkLandmarkSpatialObject_h
#define itkLandmarkSpatialObject_h

#include <list>

#include "itkPointBasedSpatialObject.h"

namespace itk
{
/**
 * \class LandmarkSpatialObject
 * \brief Representation of a Landmark based on the spatial object classes.
 *
 * The Landmark is basically defined by a set of points with spatial locations.
 *
 * \sa SpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */

template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT LandmarkSpatialObject : public PointBasedSpatialObject<TDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LandmarkSpatialObject);

  using Self = LandmarkSpatialObject;
  using Superclass = PointBasedSpatialObject<TDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using ScalarType = double;

  using LandmarkPointType = SpatialObjectPoint<TDimension>;
  using LandmarkPointListType = std::vector<LandmarkPointType>;

  using SpatialObjectPointType = typename Superclass::SpatialObjectPointType;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using PointContainerType = VectorContainer<IdentifierType, PointType>;
  using PointContainerPointer = SmartPointer<PointContainerType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(LandmarkSpatialObject, PointBasedSpatialObject);

protected:
  LandmarkSpatialObject();
  ~LandmarkSpatialObject() override = default;

  /** Method to print the object. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer
  InternalClone() const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLandmarkSpatialObject.hxx"
#endif

#endif // itkLandmarkSpatialObject_h
