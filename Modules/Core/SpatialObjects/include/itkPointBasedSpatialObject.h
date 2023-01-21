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
#ifndef itkPointBasedSpatialObject_h
#define itkPointBasedSpatialObject_h

#include "itkSpatialObject.h"
#include "itkSpatialObjectPoint.h"

namespace itk
{
/**
 * \class PointBasedSpatialObject
 * \brief This class serves as the base class for point-based spatial objects
 *
 * A PointBasedSpatialObject is an abstract class to support
 * PointBasedSpatialObject filters and algorithms.
 *
 * \ingroup ITKSpatialObjects
 */

template <unsigned int TDimension = 3, class TSpatialObjectPointType = SpatialObjectPoint<TDimension>>
class ITK_TEMPLATE_EXPORT PointBasedSpatialObject : public SpatialObject<TDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PointBasedSpatialObject);

  using Self = PointBasedSpatialObject;
  using Superclass = SpatialObject<TDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using ScalarType = double;

  using SpatialObjectPointType = TSpatialObjectPointType;
  using SpatialObjectPointListType = std::vector<SpatialObjectPointType>;

  using typename Superclass::PointType;
  using typename Superclass::TransformType;
  using typename Superclass::VectorType;
  using typename Superclass::CovariantVectorType;
  using typename Superclass::BoundingBoxType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(PointBasedSpatialObject, SpatialObject);

  /** Reset the spatial object to its initial condition, yet preserves
   *   Id, Parent, and Child information */
  void
  Clear() override;

  /** Assign points to this object, and assigned this object to
   * each point (for computing world coordinates) */
  virtual void
  AddPoint(const SpatialObjectPointType & newPoint);

  /** Removes the indicated point from this object. */
  virtual void
  RemovePoint(IdentifierType id);

  /** Assign points to this object, and assigned this object to
   * each point (for computing world coordinates). */
  virtual void
  SetPoints(const SpatialObjectPointListType & newPoints);

  /** Get the list of points assigned to this object. */
  virtual SpatialObjectPointListType &
  GetPoints()
  {
    return m_Points;
  }

  /** Get a const list of the points assigned to this object */
  virtual const SpatialObjectPointListType &
  GetPoints() const
  {
    return m_Points;
  }

  /** Return a SpatialObjectPoint given its position in the list */
  virtual const SpatialObjectPointType *
  GetPoint(IdentifierType id) const
  {
    return &(m_Points[id]);
  }

  virtual SpatialObjectPointType *
  GetPoint(IdentifierType id)
  {
    return &(m_Points[id]);
  }

  /** Return the number of points in the list */
  virtual SizeValueType
  GetNumberOfPoints() const
  {
    return static_cast<SizeValueType>(m_Points.size());
  }

  /** Get the closest point in world space. */
  TSpatialObjectPointType
  ClosestPointInWorldSpace(const PointType & point) const;

  /** Get the closest point in object space. */
  TSpatialObjectPointType
  ClosestPointInObjectSpace(const PointType & point) const;

  /** Test if a world-coordinate point is inside of this object or its children.
   *
   * Returns true if the point is inside the blob, false otherwise.
   */
  bool
  IsInsideInObjectSpace(const PointType & point) const override;

  /* Avoid hiding the overload that supports depth and name arguments */
  using Superclass::IsInsideInObjectSpace;

protected:
  /** Compute the boundaries of the Blob. */
  void
  ComputeMyBoundingBox() override;

  PointBasedSpatialObject();
  ~PointBasedSpatialObject() override = default;

  SpatialObjectPointListType m_Points{};

  /** Method to print the object.*/
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer
  InternalClone() const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPointBasedSpatialObject.hxx"
#endif

#endif // itkPointBasedSpatialObject_h
