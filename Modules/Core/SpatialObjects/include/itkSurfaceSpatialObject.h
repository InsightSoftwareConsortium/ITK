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

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT SurfaceSpatialObject:
  public PointBasedSpatialObject<  TDimension >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SurfaceSpatialObject);

  using Self = SurfaceSpatialObject;
  using Superclass = PointBasedSpatialObject< TDimension >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;
  using ScalarType = double;
  using SurfacePointType = SurfaceSpatialObjectPoint< TDimension >;
  using PointListType = std::vector< SurfacePointType >;
  using SpatialObjectPointType = typename Superclass::SpatialObjectPointType;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using PointContainerType = VectorContainer< IdentifierType, PointType >;
  using PointContainerPointer = SmartPointer< PointContainerType >;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using CovariantVectorType = typename Superclass::CovariantVectorType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(SurfaceSpatialObject, PointBasedSpatialObject);

  /** Set the list of Surface points. */
  void SetPoints(PointListType & newPoints);

  /** Returns a reference to the list of the Surface points. */
  PointListType & GetPoints()
  { return m_Points; }

  /** Returns a const reference to the list of the Surface points. */
  const PointListType & GetPoints() const
  { return m_Points; }

  /** Return a point in the list given the index */
  const SpatialObjectPointType * GetPoint(IdentifierType id) const override
  { return &( m_Points[id] ); }

  /** Return a point in the list given the index */
  SpatialObjectPointType * GetPoint(IdentifierType id) override
  { return &( m_Points[id] ); }

  /** Return the number of points in the list */
  SizeValueType GetNumberOfPoints() const override
  { return static_cast<SizeValueType>( m_Points.size() ); }

  /** Method returns the Point closest to the given point */
  IdentifierType ClosestPoint( const PointType & curPoint) const;

  /** Returns true if the point is inside the Surface, false otherwise. */
  bool IsInside(const PointType & point, unsigned int depth=0,
    const std::string & name) const override;

  /** Compute the boundaries of the Surface. */
  bool ComputeObjectBoundingBox() const override;

  /** Compute the normals to the surface from neighboring points */
  bool Approximate3DNormals();

protected:
  PointListType m_Points;

  SurfaceSpatialObject();
  ~SurfaceSpatialObject() override = default;

  /** Method to print the object.*/
  void PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSurfaceSpatialObject.hxx"
#endif

#endif // itkSurfaceSpatialObject_h
