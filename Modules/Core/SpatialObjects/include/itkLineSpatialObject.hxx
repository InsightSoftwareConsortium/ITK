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
#ifndef itkLineSpatialObject_hxx
#define itkLineSpatialObject_hxx


#include "itkLineSpatialObject.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
LineSpatialObject< TDimension >
::LineSpatialObject()
{
  this->SetTypeName("LineSpatialObject");
  this->GetProperty()->SetRed(1);
  this->GetProperty()->SetGreen(0);
  this->GetProperty()->SetBlue(0);
  this->GetProperty()->SetAlpha(1);
}

/** Destructor */
template< unsigned int TDimension >
LineSpatialObject< TDimension >
::~LineSpatialObject() = default;

/** Set the list of Line points. */
template< unsigned int TDimension >
void
LineSpatialObject< TDimension >
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

  this->ComputeBoundingBox();
  this->Modified();
}

/** Print the object. */
template< unsigned int TDimension >
void
LineSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "LineSpatialObject(" << this << ")" << std::endl;
  os << indent << "ID: " << this->GetId() << std::endl;
  os << indent << "nb of points: "
     << static_cast< SizeValueType >( m_Points.size() ) << std::endl;
  Superclass::PrintSelf(os, indent);
}

/** Compute the boundaries of the line. */
template< unsigned int TDimension >
bool
LineSpatialObject< TDimension >
::ComputeObjectBoundingBox() const
{
  auto it  = m_Points.begin();
  auto end = m_Points.end();

  if ( it == end )
    {
    return false;
    }

  PointType pt =  this->GetObjectToWorldTransform()->TransformPoint(
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
LineSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    auto it = m_Points.begin();
    auto itEnd = m_Points.end();

    PointType transformedPoint = this->GetObjectToWorldTransform()->
      GetInverse()->TransformPoint(point);

    if ( this->GetObjectBounds()->IsInside(transformedPoint) )
      {
      while ( it != itEnd )
        {
        bool match = true;
        for( unsigned int i=0; i<ObjectDimension; ++i )
          {
          if ( ! Math::AlmostEquals( ( *it ).GetPosition()[i],
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
        it++;
        }
      }
    }

  if( depth > 0 )
    {
    return Superclass::IsInsideChildren( point, depth-1, name );
    }


  return false;
}

} // end namespace itk

#endif
