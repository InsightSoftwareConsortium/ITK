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
#ifndef itkContourSpatialObject_hxx
#define itkContourSpatialObject_hxx

#include "itkContourSpatialObject.h"
#include "itkNumericTraits.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
ContourSpatialObject< TDimension >
::ContourSpatialObject()
{
  this->SetTypeName("ContourSpatialObject");
  this->GetProperty()->SetRed(1);
  this->GetProperty()->SetGreen(0);
  this->GetProperty()->SetBlue(0);
  this->GetProperty()->SetAlpha(1);
  m_InterpolationType = NO_INTERPOLATION;
  m_Closed = false;
  m_DisplayOrientation = -1;
  m_AttachedToSlice = -1;
}

/** Destructor */
template< unsigned int TDimension >
ContourSpatialObject< TDimension >
::~ContourSpatialObject() = default;

/** Get the list of control points */
template< unsigned int TDimension >
typename ContourSpatialObject< TDimension >::ControlPointListType &
ContourSpatialObject< TDimension >
::GetControlPoints()
{
  itkDebugMacro("Getting control Point list");
  return m_ControlPoints;
}

/** Get the list of control points */
template< unsigned int TDimension >
const typename ContourSpatialObject< TDimension >::ControlPointListType &
ContourSpatialObject< TDimension >
::GetControlPoints() const
{
  itkDebugMacro("Getting ContourPoint list");
  return m_ControlPoints;
}

/** Set the control points which are defining the contour */
template< unsigned int TDimension >
void
ContourSpatialObject< TDimension >
::SetControlPoints(ControlPointListType & points)
{
  m_ControlPoints.clear();

  typename ControlPointListType::iterator it, end;
  it = points.begin();
  end = points.end();
  while ( it != end )
    {
    m_ControlPoints.push_back(*it);
    it++;
    }
  this->Modified();
}

/** Get the list of interpolated points */
template< unsigned int TDimension >
typename ContourSpatialObject< TDimension >::InterpolatedPointListType &
ContourSpatialObject< TDimension >
::GetInterpolatedPoints()
{
  itkDebugMacro("Getting interpolated Point list");
  return m_InterpolatedPoints;
}

/** Get the list of interpolated points */
template< unsigned int TDimension >
const typename ContourSpatialObject< TDimension >::InterpolatedPointListType &
ContourSpatialObject< TDimension >
::GetInterpolatedPoints() const
{
  itkDebugMacro("Getting interpolated list");
  return m_InterpolatedPoints;
}

/** Set the interpolated points which are defining the contour */
template< unsigned int TDimension >
void
ContourSpatialObject< TDimension >
::SetInterpolatedPoints(InterpolatedPointListType & points)
{
  m_InterpolatedPoints.clear();

  typename InterpolatedPointListType::iterator it, end;
  it = points.begin();
  end = points.end();
  while ( it != end )
    {
    m_InterpolatedPoints.push_back(*it);
    it++;
    }
  this->Modified();
}

/** Print the contour spatial object */
template< unsigned int TDimension >
void
ContourSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "ContourSpatialObject(" << this << ")" << std::endl;
  os << indent << "#Control Points: "
     << static_cast< SizeValueType >( m_ControlPoints.size() ) << std::endl;
  os << indent << "Interpolation type: " << m_InterpolationType << std::endl;
  os << indent << "Contour closed: " << m_Closed << std::endl;
  os << indent << "Display Orientation : " << m_DisplayOrientation << std::endl;
  os << indent << "Pin to slice : " << m_AttachedToSlice << std::endl;
  Superclass::PrintSelf(os, indent);
}

/** Compute the bounds of the blob */
template< unsigned int TDimension >
bool
ContourSpatialObject< TDimension >
::ComputeObjectBoundingBox() const
{
  itkDebugMacro("Computing blob bounding box");

  auto it  = m_ControlPoints.begin();
  auto end = m_ControlPoints.end();

  if ( it == end )
    {
    return false;
    }
  else
    {
    PointType pt = this->GetObjectToWorldTransform()->TransformPoint(
      ( *it ).GetPosition() );
    const_cast< BoundingBoxType * >( this->GetObjectBounds() )->SetMinimum(pt);
    const_cast< BoundingBoxType * >( this->GetObjectBounds() )->SetMaximum(pt);
    const_cast< BoundingBoxType * >( this->GetObjectBounds() )->
      ConsiderPoint(pt);
    it++;

    while ( it != end )
      {
      pt = this->GetObjectToWorldTransform()->TransformPoint(
        ( *it ).GetPosition() );
      const_cast< BoundingBoxType * >( this->GetObjectBounds() )->
        ConsiderPoint(pt);
      it++;
      }

    // Add the interpolated points (if any)
    auto itI = m_InterpolatedPoints.begin();
    while ( itI != m_InterpolatedPoints.end() )
      {
      pt = this->GetObjectToWorldTransform()->TransformPoint(
        ( *itI ).GetPosition() );
      const_cast< BoundingBoxType * >( this->GetObjectBounds() )->
        ConsiderPoint(pt);
      itI++;
      }
    }

  return true;
}

/** Test if the given point is inside the blob. Since a contour is
 *  considered to be a 1D object, IsInside will always return false.
 *  Note: removed names of arguments since they are no longer used. */
template< unsigned int TDimension >
bool
ContourSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  itkDebugMacro("Computing blob IsInside");

  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    PointType tPoint = this->GetObjectToWorldTransform()->GetInverse()->
      TransformPoint( point );

    auto it = m_ControlPoints.begin();

    bool match = false;
    while ( it != m_ControlPoints.end() )
      {
      match = true;
      PointType pt = ( *it ).GetPosition();
      for( unsigned int d=0; d<ObjectDimension; ++d )
        {
        if( pt[d] != tPoint[d] )
          {
          match = false;
          break;
          }
        }
      if( match )
        {
        return true;
        }
      it++;
      }

    // Add the interpolated points (if any)
    auto itI = m_InterpolatedPoints.begin();
    while ( itI != m_InterpolatedPoints.end() )
      {
      match = true;
      PointType pt = ( *itI ).GetPosition();
      for( unsigned int d=0; d<ObjectDimension; ++d )
        {
        if( pt[d] != tPoint[d] )
          {
          match = false;
          break;
          }
        }
      if( match )
        {
        return true;
        }
      itI++;
      }
    }

  if( depth > 0 )
    {
    return Superclass::IsInsideChildren( point, depth-1, name );
    }
  else
    {
    return false;
    }
}

} // end namespace itk

#endif
