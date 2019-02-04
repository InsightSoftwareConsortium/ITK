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
  this->SetTypeName("LandmarkSpatialObject");
  this->GetProperty()->SetRed(1);
  this->GetProperty()->SetGreen(0);
  this->GetProperty()->SetBlue(0);
  this->GetProperty()->SetAlpha(1);
}

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
::ComputeObjectBoundingBox() const
{
  itkDebugMacro("Computing Landmark bounding box");

  auto it  = m_Points.begin();
  auto end = m_Points.end();

  if ( it == end )
    {
    return false;
    }

  PointType pt = this->GetObjectToWorldTransform()->TransformPoint(
    ( *it ).GetPosition() );
  const_cast< BoundingBoxType * >( this->GetObjectBounds() )->SetMinimum(pt);
  const_cast< BoundingBoxType * >( this->GetObjectBounds() )->SetMaximum(pt);
  it++;

  while ( it != end )
    {
    pt = this->GetObjectToWorldTransform()->TransformPoint(
      ( *it ).GetPosition() );
    const_cast< BoundingBoxType * >( this->GetObjectBounds() )->
      ConsiderPoint(pt);
    it++;
    }

  return true;
}

template< unsigned int TDimension >
bool
LandmarkSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    PointType transformedPoint = this->GetObjectToWorldTransform()->
      GetInverse()->TransformPoint(point);

    if ( this->GetObjectBounds()->IsInside(transformedPoint) )
      {
      auto it = m_Points.begin();
      auto itEnd = m_Points.end();

      while ( it != itEnd )
        {
        bool match = true;
        for( unsigned int i=0; i<ObjectDimensions; ++i )
          {
          if ( !Math::AlmostEquals( ( *it ).GetPosition()[i],
               transformedPoint[i] ) )
            {
            match = false;
            break;
            }
          }
        if( match )
          {
          return true;
          }
        ++it;
        }
      }
    }

  if( depth > 0 )
    {
    return Superclass::IsInside( point, depth-1, name );
    }

  return false;
}

} // end namespace itk

#endif
