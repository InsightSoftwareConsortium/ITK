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
#ifndef itkBlobSpatialObject_hxx
#define itkBlobSpatialObject_hxx


#include "itkBlobSpatialObject.h"

#include "itkNumericTraits.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
BlobSpatialObject< TDimension >
::BlobSpatialObject()
{
  this->SetTypeName("BlobSpatialObject");
  this->GetProperty()->SetRed(1);
  this->GetProperty()->SetGreen(0);
  this->GetProperty()->SetBlue(0);
  this->GetProperty()->SetAlpha(1);

  m_Points.clear();
}

/** Get the list of points that defines the blob */
template< unsigned int TDimension >
typename BlobSpatialObject< TDimension >::PointListType &
BlobSpatialObject< TDimension >
::GetPoints()
{
  itkDebugMacro("Getting BlobPoint list");
  return m_Points;
}

/** Get the list of points which are defining the blob */
template< unsigned int TDimension >
const typename BlobSpatialObject< TDimension >::PointListType &
BlobSpatialObject< TDimension >
::GetPoints() const
{
  itkDebugMacro("Getting BlobPoint list");
  return m_Points;
}

/** Set the points which are defining the Blob structure */
template< unsigned int TDimension >
void
BlobSpatialObject< TDimension >
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
BlobSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "BlobSpatialObject(" << this << ")" << std::endl;
  os << indent << "ID: " << this->GetId() << std::endl;
  os << indent << "nb of points: "
     << static_cast< SizeValueType >( m_Points.size() ) << std::endl;
  Superclass::PrintSelf(os, indent);
}

/** Compute the bounds of the blob */
template< unsigned int TDimension >
bool
BlobSpatialObject< TDimension >
::ComputeObjectBoundingBox() const
{
  itkDebugMacro("Computing blob bounding box");

  auto it  = m_Points.begin();
  auto end = m_Points.end();

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
    it++;
    while ( it != end )
      {
      pt = this->GetObjectToWorldTransform()->TransformPoint(
        ( *it ).GetPosition() );
      const_cast< BoundingBoxType * >( this->GetObjectBounds() )
        ->ConsiderPoint(pt);
      it++;
      }
    }
  return true;
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< unsigned int TDimension >
bool
BlobSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  if( this->GetTypeName.find( name ) != std::string::npos )
    {
    auto it = m_Points.begin();
    auto itEnd = m_Points.end();

    PointType transformedPoint =
      this->GetObjectToWorldTransform()->GetInverse()->TransformPoint(point);

    if( this->GetBounds()->IsInside(transformedPoint) )
      {
      while ( it != itEnd )
        {
        bool equals = true;
        for( unsigned int i=0; i<ObjectDimension; ++i )
          {
          if( ! Math::AlmostEquals( transformedPoint[i], it->GetPosition() ) )
            {
            equals = false;
            break;
            }
          }
        if( equals )
          {
          return true;
          }
        it++;
        }
      }
    }
  if( depth > 0 )
    {
    return Superclass::IsInsideChildren(point, depth-1, name);
    }
  else
    {
    return false;
    }
}

} // end namespace itk

#endif
