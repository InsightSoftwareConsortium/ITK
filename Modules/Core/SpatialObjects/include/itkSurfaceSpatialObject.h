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
#ifndef itkSurfaceSpatialObject_h
#define itkSurfaceSpatialObject_h

#include <list>

#include "itkPointBasedSpatialObject.h"
#include "itkSurfaceSpatialObjectPoint.h"

namespace itk
{
/**
 * \class SurfaceSpatialObject
 * \brief Representation of a Surface based on the spatial object classes.
 *
 * The Surface is basically defined by a set of points.
 *
 * \sa SurfaceSpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */

template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT SurfaceSpatialObject
  : public PointBasedSpatialObject<TDimension, SurfaceSpatialObjectPoint<TDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SurfaceSpatialObject);

  using Self = SurfaceSpatialObject;
  using Superclass = PointBasedSpatialObject<TDimension, SurfaceSpatialObjectPoint<TDimension>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using ScalarType = double;

  using SurfacePointType = SurfaceSpatialObjectPoint<TDimension>;
  using SurfacePointListType = std::vector<SurfacePointType>;

  using SpatialObjectPointType = typename Superclass::SpatialObjectPointType;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using PointContainerType = VectorContainer<IdentifierType, PointType>;
  using PointContainerPointer = SmartPointer<PointContainerType>;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using CovariantVectorType = typename Superclass::CovariantVectorType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(SurfaceSpatialObject, PointBasedSpatialObject);

  /** Restore a spatial object to its initial state, yet preserves Id as well as
   *   parent and children relationships */
  void
  Clear() override;

  /** Compute the normals to the surface from neighboring points */
  bool
  Approximate3DNormals();

protected:
  SurfaceSpatialObject();
  ~SurfaceSpatialObject() override = default;

  /** Method to print the object.*/
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer
  InternalClone() const override;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSurfaceSpatialObject.hxx"
#endif

#endif // itkSurfaceSpatialObject_h
