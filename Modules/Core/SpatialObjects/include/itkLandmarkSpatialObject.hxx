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
#ifndef itkLandmarkSpatialObject_hxx
#define itkLandmarkSpatialObject_hxx


#include "itkLandmarkSpatialObject.h"
#include "itkNumericTraits.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
LandmarkSpatialObject< TDimension >
::LandmarkSpatialObject()
{
  this->SetDimension(TDimension);
  this->SetTypeName("LandmarkSpatialObject");
  this->GetProperty()->SetRed(1);
  this->GetProperty()->SetGreen(0);
  this->GetProperty()->SetBlue(0);
  this->GetProperty()->SetAlpha(1);
}

/** Destructor */
template< unsigned int TDimension >
LandmarkSpatialObject< TDimension >
::~LandmarkSpatialObject()
{}

/** Get the list of points which are defining the blob */
template< unsigned int TDimension >
typename LandmarkSpatialObject< TDimension >::PointListType &
LandmarkSpatialObject< TDimension >
::GetPoints()
{
  itkDebugMacro("Getting LandmarkPoint list");
  return m_Points;
}

/** Get the list of points which are defining the blob */
template< unsigned int TDimension >
const typename LandmarkSpatialObject< TDimension >::PointListType &
LandmarkSpatialObject< TDimension >
::GetPoints() const
{
  itkDebugMacro("Getting LandmarkPoint list");
  return m_Points;
}

/** Set the points which are defining the Landmark structure */
template< unsigned int TDimension >
void
LandmarkSpatialObject< TDimension >
::SetPoints(PointListType & points)
{
  // in this function, passing a null pointer as argument will
  // just clear the list...
  m_Points.clear();

  typename PointListType::iterator it, end;
  it = points.begin();
  end = points.end();
  while ( it != end )
    {
    m_Points.push_back(*it);
    it++;
    }

  this->Modified();
}

/** Print the blob spatial object */
template< unsigned int TDimension >
void
LandmarkSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "LandmarkSpatialObject(" << this << ")" << std::endl;
  os << indent << "ID: " << this->GetId() << std::endl;
  os << indent << "nb of points: "
     << static_cast< SizeValueType >( m_Points.size() ) << std::endl;
  Superclass::PrintSelf(os, indent);
}

/** Compute the bounds of the blob */
template< unsigned int TDimension >
bool
LandmarkSpatialObject< TDimension >
::ComputeLocalBoundingBox() const
{
  itkDebugMacro("Computing blob bounding box");

  if ( this->GetBoundingBoxChildrenName().empty()
       || strstr( typeid( Self ).name(),
                  this->GetBoundingBoxChildrenName().c_str() ) )
    {
    typename PointListType::const_iterator it  = m_Points.begin();
    typename PointListType::const_iterator end = m_Points.end();

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
      }
    }

  return true;
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< unsigned int TDimension >
bool
LandmarkSpatialObject< TDimension >
::IsInside(const PointType & point) const
{
  typename PointListType::const_iterator it = m_Points.begin();
  typename PointListType::const_iterator itEnd = m_Points.end();

  if ( !this->SetInternalInverseTransformToWorldToIndexTransform() )
    {
    return false;
    }

  PointType transformedPoint =
    this->GetInternalInverseTransform()->TransformPoint(point);

  if ( this->GetBounds()->IsInside(transformedPoint) )
    {
    while ( it != itEnd )
      {
      if ( ( *it ).GetPosition() == transformedPoint )
        {
        return true;
        }
      it++;
      }
    }
  return false;
}

/** Test if the given point is inside the blob
 *  Note: ComputeBoundingBox should be called before. */
template< unsigned int TDimension >
bool
LandmarkSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth, char *name) const
{
  itkDebugMacro("Checking the point [" << point << "] is inside the blob");

  if ( name == ITK_NULLPTR )
    {
    if ( IsInside(point) )
      {
      return true;
      }
    }
  else if ( strstr(typeid( Self ).name(), name) )
    {
    if ( IsInside(point) )
      {
      return true;
      }
    }

  return Superclass::IsInside(point, depth, name);
}

/** Return true if the blob is evaluable at a given point
 *  i.e if the point is defined in the points list        */
template< unsigned int TDimension >
bool
LandmarkSpatialObject< TDimension >
::IsEvaluableAt(const PointType & point,
                unsigned int depth, char *name) const
{
  itkDebugMacro("Checking if the blob is evaluable at " << point);
  return IsInside(point, depth, name);
}

/** Return 1 if the point is in the points list */
template< unsigned int TDimension >
bool
LandmarkSpatialObject< TDimension >
::ValueAt(const PointType & point, double & value, unsigned int depth,
          char *name) const
{
  itkDebugMacro("Getting the value of the blob at " << point);
  if ( IsInside(point, 0, name) )
    {
    value = this->GetDefaultInsideValue();
    return true;
    }
  else if ( Superclass::IsEvaluableAt(point, depth, name) )
    {
    Superclass::ValueAt(point, value, depth, name);
    return true;
    }
  value = this->GetDefaultOutsideValue();
  return false;
}
} // end namespace itk

#endif
