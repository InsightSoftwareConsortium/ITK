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

template< unsigned int TDimension = 3,
  class TSpatialObjectPointType = SpatialObjectPoint< TDimension > >
class ITK_TEMPLATE_EXPORT PointBasedSpatialObject:
  public SpatialObject< TDimension >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PointBasedSpatialObject);

  using Self = PointBasedSpatialObject;
  using Superclass = SpatialObject< TDimension >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  using ScalarType = double;

  using SpatialObjectPointType = TSpatialObjectPointType;
  using SpatialObjectPointListType = std::vector< SpatialObjectPointType >;

  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using VectorType = typename Superclass::VectorType;
  using CovariantVectorType = typename Superclass::CovariantVectorType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(PointBasedSpatialObject, SpatialObject);

  /** Assign points to this object, and assigned this object to
   * each point (for computing world coordinates) */
  virtual void AddPoint( SpatialObjectPointType & newPoints );

  /** Assign points to this object, and assigned this object to
   * each point (for computing world coordinates) */
  virtual void SetPoints( SpatialObjectPointListType & newPoints );

  /** Get the list of points assigned to this object */
  virtual SpatialObjectPointListType & GetPoints()
  { return m_Points; }

  /** Get a const list of the points assigned to this object */
  virtual const SpatialObjectPointListType & GetPoints() const
  { return m_Points; }

  /** Return a SpatialObjectPoint given its position in the list */
  virtual const SpatialObjectPointType *
  GetPoint( IdentifierType id ) const
  { return &( m_Points[id] ); }

  virtual SpatialObjectPointType *
  GetPoint( IdentifierType id )
  { return &( m_Points[id] ); }

  /** Return the number of points in the list */
  virtual SizeValueType GetNumberOfPoints() const
  { return static_cast<SizeValueType>( m_Points.size() ); }

  /** Method returns the Point closest to the given point */
  TSpatialObjectPointType & ClosestPoint(const PointType & worldPoint) const;

  /** Returns true if the point is inside the Blob, false otherwise. */
  bool IsInside(const PointType & worldPoint, unsigned int depth,
    const std::string & name) const override;

  /** Compute the boundaries of the Blob. */
  bool ComputeMyBoundingBox() const override;

protected:
  PointBasedSpatialObject();
  ~PointBasedSpatialObject() override = default;

  SpatialObjectPointListType m_Points;

  /** Method to print the object.*/
  void PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointBasedSpatialObject.hxx"
#endif

#endif // itkPointBasedSpatialObject_h
