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
#ifndef itkPolygonSpatialObject_hxx
#define itkPolygonSpatialObject_hxx

#include "itkPolygonSpatialObject.h"
#include "itkExceptionObject.h"
#include "itkMath.h"

namespace itk
{
template< unsigned int TDimension >
typename PolygonSpatialObject< TDimension >::PolygonGroupOrientation
PolygonSpatialObject< TDimension >
::Plane() const
{
  if ( Self::ObjectDimension == 2 )
    {
    return Axial;
    }

  PolygonGroupOrientation plane;
  // local typedef to shut up the compiler...

  const PointListType & points = this->GetPoints();
  typename PointListType::const_iterator it = points.begin();
  typename PointListType::const_iterator itend = points.end();
  double min[3], max[3];       // x, y, z
  int    i;
  for ( i = 0; i < 3; i++ )
    {
    max[i] = NumericTraits< double >::NonpositiveMin();
    min[i] = NumericTraits< double >::max();
    }
  while ( it != itend )
    {
    PointType curpoint = ( *it ).GetPosition();
    for ( i = 0; i < 3; i++ )
      {
      if ( min[i] > curpoint[i] ) { min[i] = curpoint[i]; }
      if ( max[i] < curpoint[i] ) { max[i] = curpoint[i]; }
      }
    it++;
    }
  if ( Math::ExactlyEquals(min[0], max[0]) && Math::NotExactlyEquals(min[1], max[1]) && Math::NotExactlyEquals(min[2], max[2]) )
    {
    plane = Sagittal;
    }
  else if ( Math::NotExactlyEquals(min[0], max[0]) && Math::ExactlyEquals(min[1], max[1]) && Math::NotExactlyEquals(min[2], max[2]) )
    {
    plane = Coronal;
    }
  else if ( Math::NotExactlyEquals(min[0], max[0]) && Math::NotExactlyEquals(min[1], max[1]) && Math::ExactlyEquals(min[2], max[2]) )
    {
    plane = Axial;
    }
  else
    {
    plane = Unknown;
    }
  return plane;
}

template< unsigned int TDimension >
bool
PolygonSpatialObject< TDimension >
::IsClosed() const
{
  const PointListType & points = this->GetPoints();

  typename PointListType::const_iterator it = points.begin();
  typename PointListType::const_iterator itend = points.end();
  itend--;
  return ( *it ).GetPosition() == ( *itend ).GetPosition();
}

template< unsigned int TDimension >
unsigned int
PolygonSpatialObject< TDimension >
::NumberOfPoints() const
{
  return static_cast<unsigned int>( ( this->GetPoints() ).size() );
}

template< unsigned int TDimension >
typename PolygonSpatialObject< TDimension >::PointType
PolygonSpatialObject< TDimension >
::ClosestPoint(const PointType & curPoint) const
{
  const PointListType & points = this->GetPoints();

  typename PointListType::const_iterator it = points.begin();
  typename PointListType::const_iterator itend = points.end();
  double distance = NumericTraits< double >::max();

  if ( it == itend )
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription(
      "PolygonSpatialObject: ClosestPoint called using an empty point list");
    throw exception;
    }

  PointType closestPoint;
  closestPoint.Fill(0.0);
  while ( it != itend )
    {
    typename SpatialObjectPoint< TDimension >::PointType curpos =
      ( *it ).GetPosition();
    double curdistance = curpos.EuclideanDistanceTo(curPoint);
    if ( curdistance < distance )
      {
      closestPoint = ( *it ).GetPosition();
      distance = curdistance;
      }
    it++;
    }
  return closestPoint;
}

template< unsigned int TDimension >
double
PolygonSpatialObject< TDimension >
::MeasureArea() const
{
  //To find the area of a planar polygon not in the x-y plane, use:
  //2 A(P) = std::abs(N . (sum_{i=0}^{n-1} (v_i x v_{i+1})))
  //where N is a unit vector normal to the plane. The `.' represents the
  //dot product operator, the `x' represents the cross product operator,
  //        and std::abs() is the absolute value function.
  double area = 0.0;
  int    numpoints = this->NumberOfPoints();
  int    X, Y;

  if ( numpoints < 3 )
    {
    return 0;
    }
  switch ( this->Plane() )
    {
    case Sagittal:
      X = 1; Y = 2;
      break;
    case Axial:
      X = 0; Y = 1;
      break;
    case Coronal:
      X = 0; Y = 2;
      break;
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("File cannot be read");
      throw exception;
    }
  const PointListType & points = this->GetPoints();
  typename PointListType::const_iterator it = points.begin();
  PointType start = ( *it ).GetPosition();
  for ( int i = 0; i < numpoints; i++ )
    {
    PointType a = ( *it ).GetPosition();
    PointType b;
    it++;
    if ( i == numpoints - 1 )
      {
      b = start;
      }
    else
      {
      b = ( *it ).GetPosition();
      }
    //
    // closed PolygonGroup has first and last points the same
    if ( a == b )
      {
      continue;
      }
    area += a[X] * b[Y] - a[Y] * b[X];
    }
  area *= 0.5;
  return area < 0.0 ? -area : area;
}

template< unsigned int TDimension >
double
PolygonSpatialObject< TDimension >
::MeasureVolume() const
{
  return m_Thickness * this->MeasureArea();
}

template< unsigned int TDimension >
double
PolygonSpatialObject< TDimension >
::MeasurePerimeter() const
{
  double perimeter = 0.0;
  int    numpoints = this->NumberOfPoints();

  if ( numpoints < 3 )
    {
    return 0;
    }
  const PointListType & points = this->GetPoints();

  typename PointListType::const_iterator it = points.begin();

  PointType start = ( *it ).GetPosition();
  for ( int i = 0; i < numpoints; i++ )
    {
    PointType a = ( *it ).GetPosition();
    PointType b;
    it++;
    if ( i == numpoints - 1 )
      {
      b = start;
      }
    else
      {
      b = ( *it ).GetPosition();
      }
    //
    // closed PolygonGroup has first and last points the same
    if ( a == b )
      {
      continue;
      }
    double curdistance = a.EuclideanDistanceTo(b);
    perimeter += curdistance;
    }
  return perimeter;
}

template< unsigned int TDimension >
bool
PolygonSpatialObject< TDimension >
::DeletePoint(const PointType & pointToDelete)
{
  PointListType & points = this->GetPoints();

  typename PointListType::iterator it = points.begin();
  typename PointListType::iterator itend = points.end();
  if ( it == itend )
    {
    return false;
    }

  while ( it != itend )
    {
    BlobPointType & curPoint = ( *it );
    typename SpatialObjectPoint< TDimension >::PointType curpos =
      curPoint.GetPosition();
    if ( curpos == pointToDelete )
      {
      points.erase(it);
      return true;
      }
    it++;
    }
  return false;
}

template< unsigned int TDimension >
bool
PolygonSpatialObject< TDimension >
::AddPoint(const PointType & pointToAdd)
{
  BlobPointType newPoint;

  newPoint.SetPosition(pointToAdd);
  this->GetPoints().push_back(newPoint);
  return true;
}

template< unsigned int TDimension >
bool
PolygonSpatialObject< TDimension >
::InsertPoint(const PointType & point1, const PointType & pointToAdd)
{
  PointListType & points = this->GetPoints();

  typename PointListType::iterator it = points.begin();
  typename PointListType::iterator itend = points.end();
  if ( it == itend )
    {
    this->AddPoint(pointToAdd);
    return true;
    }

  while ( it != itend )
    {
    BlobPointType & curPoint = ( *it );
    typename SpatialObjectPoint< TDimension >::PointType curpos =
      curPoint.GetPosition();
    if ( curpos == point1 )
      {
      typename PointListType::iterator after = it;
      after++;
      BlobPointType newPoint;
      newPoint.SetPosition(pointToAdd);
      points.insert(after, 1, newPoint);
      return true;
      }
    it++;
    }
  return false;
}

template< unsigned int TDimension >
bool
PolygonSpatialObject< TDimension >
::ReplacePoint(const PointType & oldpoint, const PointType & newPoint)
{
  if ( oldpoint == newPoint )
    {
    return true;
    }
  PointListType & points = this->GetPoints();
  typename PointListType::iterator it = points.begin();
  typename PointListType::iterator itend = points.end();
  if ( it == itend )
    {
    this->AddPoint(newPoint);
    return true;
    }

  while ( it != itend )
    {
    BlobPointType & curPoint = ( *it );
    typename SpatialObjectPoint< TDimension >::PointType curpos =
      curPoint.GetPosition();
    if ( curpos == oldpoint )
      {
      typename PointListType::iterator after = it;
      after++;
      BlobPointType newBlobPoint;
      newBlobPoint.SetPosition(newPoint);
      points.insert(after, 1, newBlobPoint);
      points.erase(it);
      return true;
      }
    it++;
    }
  return false;
}

template< unsigned int TDimension >
bool
PolygonSpatialObject< TDimension >
::RemoveSegment(const PointType & startPoint, const PointType & endPoint)
{
  PointListType & points = this->GetPoints();

  typename PointListType::iterator it = points.begin();
  typename PointListType::iterator itend = points.end();
  typename PointListType::iterator first;
  typename PointListType::iterator last;

  if ( it == itend )
    {
    return false;
    }
  int foundcount = 0;
  while ( it != itend )
    {
    BlobPointType & curPoint = ( *it );
    typename SpatialObjectPoint< TDimension >::PointType curpos =
      curPoint.GetPosition();
    if ( curpos == startPoint )
      {
      first = it;
      foundcount++;
      }
    //
    // make sure you find the start before you find the end
    else if ( foundcount > 0 && curpos == endPoint )
      {
      last = it;
      foundcount++;
      }
    if ( foundcount == 2 )
      {
      break;
      }
    it++;
    }
  if ( foundcount != 2 )
    {
    return false;
    }

  points.erase( first, points.erase(last) );
  return true;
}

template< unsigned int TDimension >
bool
PolygonSpatialObject< TDimension >
::IsInside(const PointType & point) const
{
  return this->IsInside(point, 0, ITK_NULLPTR);
}

template< unsigned int TDimension >
bool
PolygonSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int, char *) const
{
  int numpoints = this->NumberOfPoints();
  int X, Y;

  if ( numpoints < 3 )
    {
    return false;
    }
  switch ( this->Plane() )
    {
    case Sagittal:
      X = 1; Y = 2;
      break;
    case Axial:
      X = 0; Y = 1;
      break;
    case Coronal:
      X = 0; Y = 2;
      break;
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("non-planar polygon");
      throw exception;
    }

  if ( !this->SetInternalInverseTransformToWorldToIndexTransform() )
    {
    return false;
    }

  PointType transformedPoint =
    this->GetInternalInverseTransform()->TransformPoint(point);

  const PointListType & points = this->GetPoints();
  typename PointListType::const_iterator it = points.begin();
  typename PointListType::const_iterator itend = points.end();
  itend--;

  PointType first = ( *it ).GetPosition();

  // If last point same as first, don't bother with it.
  if ( this->IsClosed() )
    {
    numpoints--;
    }

  bool oddNodes = false;

  PointType node1;
  PointType node2;

  for ( int i = 0; i < numpoints; i++ )
    {
    node1 = ( *it ).GetPosition();
    it++;
    if ( i == numpoints - 1 )
      {
      node2 = first;
      }
    else
      {
      node2 = ( *it ).GetPosition();
      }

    const double x = transformedPoint[X];
    const double y = transformedPoint[Y];

    if ( ( node1[Y] < y && node2[Y] >= y )
         || ( node2[Y] < y && node1[Y] >= y ) )
      {
      if ( node1[X] + ( y - node1[Y] )
           / ( node2[Y] - node1[Y] ) * ( node2[X] - node1[X] ) < x )
        {
        oddNodes = !oddNodes;
        }
      }
    }

  return oddNodes;
}

template< unsigned int TDimension >
void
PolygonSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << m_Thickness << std::endl;
}
}
#endif
