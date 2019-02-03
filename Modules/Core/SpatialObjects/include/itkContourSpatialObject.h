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
#ifndef itkContourSpatialObject_h
#define itkContourSpatialObject_h

#include <list>

#include "itkPointBasedSpatialObject.h"
#include "itkContourSpatialObjectPoint.h"

namespace itk
{
/**
 * \class ContourSpatialObject
 * \brief Representation of a Contour based on the spatial object classes.
 *
 * The Contour is basically defined by a set of points which are inside this blob
 *
 * \sa SpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT ContourSpatialObject:
  public PointBasedSpatialObject<  TDimension >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ContourSpatialObject);

  using Self = ContourSpatialObject;
  using Superclass = PointBasedSpatialObject< TDimension >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;
  using ScalarType = double;
  using ControlPointType = ContourSpatialObjectPoint< TDimension >;
  using InterpolatedPointType = SpatialObjectPoint< TDimension >;
  using ControlPointListType = std::vector< ControlPointType >;
  using InterpolatedPointListType = std::vector< InterpolatedPointType >;

  using SpatialObjectPointType = typename Superclass::SpatialObjectPointType;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using PointContainerType = VectorContainer< IdentifierType, PointType >;
  using PointContainerPointer = SmartPointer< PointContainerType >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(ContourSpatialObject, PointBasedSpatialObject);

  /** Returns a reference to the list of the control points. */
  ControlPointListType & GetControlPoints();

  /** Returns a reference to the list of the control points. */
  const ControlPointListType & GetControlPoints() const;

  /** Set the list of control points. */
  void SetControlPoints(ControlPointListType & newPoints);

  /** Return a control point in the list given the index */
  const ControlPointType * GetControlPoint(IdentifierType id) const
  { return &( m_ControlPoints[id] ); }

  /** Return a control point in the list given the index */
  ControlPointType * GetControlPoint(IdentifierType id)
  { return &( m_ControlPoints[id] ); }

  /** Return the number of control points in the list */
  SizeValueType GetNumberOfControlPoints() const
  { return static_cast<SizeValueType>( m_ControlPoints.size() ); }

  /** Returns a reference to the list of the interpolated points. */
  InterpolatedPointListType & GetInterpolatedPoints();

  /** Returns a reference to the list of the interpolated points. */
  const InterpolatedPointListType & GetInterpolatedPoints() const;

  /** Set the list of interpolated points. */
  void SetInterpolatedPoints(InterpolatedPointListType & newPoints);

  /** Return a interpolated point in the list given the index */
  const InterpolatedPointType * GetInterpolatedPoint(IdentifierType id) const
  { return &( m_InterpolatedPoints[id] ); }

  /** Return a interpolated point in the list given the index */
  InterpolatedPointType * GetInterpolatedPoint(IdentifierType id)
  { return &( m_InterpolatedPoints[id] ); }

  /** Return the number of interpolated points in the list */
  SizeValueType GetNumberOfInterpolatedPoints() const
  { return static_cast<SizeValueType>( m_InterpolatedPoints.size() ); }

  enum InterpolationType { NO_INTERPOLATION = 0,
                           EXPLICIT_INTERPOLATION, BEZIER_INTERPOLATION,
                           LINEAR_INTERPOLATION };

  /** Set/Get the interpolation type */
  InterpolationType GetInterpolationType() const
  { return m_InterpolationType; }
  void SetInterpolationType(InterpolationType interpolation)
  { m_InterpolationType = interpolation; }

  /** Set/Get if the contour is closed */
  itkSetMacro(Closed, bool);
  itkGetConstMacro(Closed, bool);

  /** Set/Get the display orientation of the contour */
  itkSetMacro(DisplayOrientation, int);
  itkGetConstMacro(DisplayOrientation, int);

  /** Set/Get the slice attached to the contour if any
   *  -1 is returned if no contour attached. */
  itkSetMacro(AttachedToSlice, int);
  itkGetConstMacro(AttachedToSlice, int);

  /** Returns true if the point is inside the Contour, false otherwise. */
  bool IsInside(const PointType & point, unsigned int depth=0,
    const std::string & name="") const override;

  /** Compute the boundaries of the Contour. */
  bool ComputeObjectBoundingBox( void ) const override;

protected:
  ControlPointListType      m_ControlPoints;
  InterpolatedPointListType m_InterpolatedPoints;
  InterpolationType         m_InterpolationType;
  bool                      m_Closed;
  int                       m_DisplayOrientation;
  int                       m_AttachedToSlice;

  ContourSpatialObject();
  ~ContourSpatialObject() override = default;

  /** Method to print the object. */
  void PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkContourSpatialObject.hxx"
#endif

#endif // itkContourSpatialObject_h
