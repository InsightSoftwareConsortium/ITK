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
  this->SetDimension(TDimension);
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
::~ContourSpatialObject()
{}

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
  os << indent << "ID: " << this->GetId() << std::endl;
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
::ComputeLocalBoundingBox() const
{
  itkDebugMacro("Computing blob bounding box");

  if ( this->GetBoundingBoxChildrenName().empty()
       || strstr( typeid( Self ).name(),
                  this->GetBoundingBoxChildrenName().c_str() ) )
    {
    typename ControlPointListType::const_iterator it  = m_ControlPoints.begin();
    typename ControlPointListType::const_iterator end = m_ControlPoints.end();

    if ( it == end )
      {
      return false;
      }
    else
      {
      PointType pt =
        this->GetIndexToWorldTransform()->TransformPoint( ( *it ).GetPosition() );
      const_cast< BoundingBoxType * >( this->GetBounds() )->SetMinimum(pt);
      const_cast< BoundingBoxType * >( this->GetBounds() )->SetMaximum(pt);
      it++;

      while ( it != end )
        {
        pt =
          this->GetIndexToWorldTransform()->TransformPoint( ( *it ).GetPosition() );
        const_cast< BoundingBoxType * >( this->GetBounds() )->ConsiderPoint(pt);
        it++;
        }

      // Add the interpolated points (if any)
      typename InterpolatedPointListType::const_iterator itI =
        m_InterpolatedPoints.begin();
      while ( itI != m_InterpolatedPoints.end() )
        {
        pt = this->GetIndexToWorldTransform()->TransformPoint(
          ( *itI ).GetPosition() );
        const_cast< BoundingBoxType * >( this->GetBounds() )->ConsiderPoint(pt);
        itI++;
        }
      }
    }

  return true;
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth. Since a contour is
 *  considered to be a 1D object, IsInside will always return false. */
template< unsigned int TDimension >
bool
ContourSpatialObject< TDimension >
::IsInside( const PointType & itkNotUsed(point) ) const
{
  return false;
}

/** Test if the given point is inside the blob. Since a contour is
 *  considered to be a 1D object, IsInside will always return false.
 *  Note: removed names of arguments since they are no longer used. */
template< unsigned int TDimension >
bool
ContourSpatialObject< TDimension >
::IsInside(const PointType &, unsigned int, char *) const
{
  return false;
}

/** Return true if the blob is evaluable at a given point
 *  i.e if the point is defined in the points list        */
template< unsigned int TDimension >
bool
ContourSpatialObject< TDimension >
::IsEvaluableAt(const PointType & point,
                unsigned int depth, char *name) const
{
  itkDebugMacro("Checking if the blob is evaluable at " << point);
  return IsInside(point, depth, name);
}

/** Return 1 if the point is in the points list
 *  Note: removed names of third parameter since it is no
 *  longer used.  It was "depth" */
template< unsigned int TDimension >
bool
ContourSpatialObject< TDimension >
::ValueAt(const PointType & point, double & value, unsigned int,
          char *name) const
{
  itkDebugMacro("Getting the value of the blob at " << point);

  value = this->GetDefaultOutsideValue(); // cannot be inside of a 1d contour
  return IsInside(point, 0, name);        // so will always return false
}
} // end namespace itk

#endif
