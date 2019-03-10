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
PolygonSpatialObject< TDimension >
::PolygonSpatialObject()
{
  this->SetTypeName( "PolygonSpatialObject" );
  m_IsClosed = false;
  m_IsClosedMTime = this->GetMyMTime();
  m_OrientationInObjectSpace = -1;
  m_OrientationInObjectSpaceMTime = this->GetMyMTime();
  m_Thickness = 0.0;
}

template< unsigned int TDimension >
int
PolygonSpatialObject< TDimension >
::GetOrientationInObjectSpace() const
{
  if( m_OrientationInObjectSpaceMTime == this->GetMyMTime() )
    {
    return m_OrientationInObjectSpace;
    }
  m_OrientationInObjectSpaceMTime = this->GetMyMTime();

  const PolygonPointListType & points = this->GetPoints();
  auto it = points.begin();
  auto itend = points.end();
  PointType minPnt;
  PointType maxPnt;
  minPnt.Fill( NumericTraits< double >::max() );
  maxPnt.Fill( NumericTraits< double >::NonpositiveMin() );
  while ( it != itend )
    {
    PointType curpoint = this->GetObjectToWorldTransform()->TransformPoint(
      ( *it ).GetPositionInObjectSpace() );
    for ( unsigned int i = 0; i < ObjectDimension; i++ )
      {
      if ( minPnt[i] > curpoint[i] )
        {
        minPnt[i] = curpoint[i];
        }
      if ( maxPnt[i] < curpoint[i] )
        {
        maxPnt[i] = curpoint[i];
        }
      }
    it++;
    }
  m_OrientationInObjectSpace = -1;
  for ( unsigned int i = 0; i < ObjectDimension; i++ )
    {
    if ( Math::ExactlyEquals(minPnt[0], maxPnt[0]) )
      {
      m_OrientationInObjectSpace = i;
      break;
      }
    }
  return m_OrientationInObjectSpace;
}

template< unsigned int TDimension >
bool
PolygonSpatialObject< TDimension >
::IsClosed() const
{
  if( m_IsClosedMTime == this->GetMyMTime() )
    {
    return m_IsClosed;
    }
  m_IsClosedMTime = this->GetMyMTime();

  const PolygonPointListType & points = this->GetPoints();

  auto it = points.begin();
  auto itend = points.end();
  itend--;
  return ( *it ).GetPositionInObjectSpace() ==
    ( *itend ).GetPositionInObjectSpace();
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
  int    numpoints = this->GetNumberOfPoints();
  int    X = -1;
  int    Y = -1;

  if ( numpoints < 3 )
    {
    return 0;
    }

  for( unsigned int i=0; i<ObjectDimension ++i )
    {
    if( this->GetOrientationInObjectSpace() != i )
      {
      if( X == -1 )
        {
        X = i;
        }
      else
        {
        Y = i;
        break;
        }
      }
    }

  const PolygonPointListType & points = this->GetPoints();
  auto it = points.begin();
  PointType a;
  PointType b = ( *it ).GetPosition();  // In world space
  for ( int i = 0; i < numpoints; i++ )
    {
    a = b;
    it++;
    b = ( *it ).GetPosition();  // In world space

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
  int    numpoints = this->GetNumberOfPoints();

  if ( numpoints < 3 )
    {
    return 0;
    }
  const PolygonPointListType & points = this->GetPoints();

  auto it = points.begin();

  PointType a;
  PointType b = ( *it ).GetPosition();  // In world space
  for ( int i = 0; i < numpoints; i++ )
    {
    a = b;
    it++;
    b = ( *it ).GetPosition();  // In world space

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
::IsInside(const PointType & worldPoint, unsigned int depth,
  const std::string & name) const
{
  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    if( this->IsClosed() && this->GetMyBoundingBox()->IsInside( worldPoint ) )
      {
      int    numpoints = this->GetNumberOfPoints();
      int    X = -1;
      int    Y = -1;

      if ( numpoints >= 3 )
        {
        for( unsigned int i=0; i<ObjectDimension; ++i )
          {
          if( this->GetOrientationInObjectSpace() != i )
            {
            if( X == -1 )
              {
              X = i;
              }
            else
              {
              Y = i;
              break;
              }
            }
          }

        PointType transformedPoint = this->GetObjectToWorldTransform()->
          GetInverseTransform()->TransformPoint( worldPoint );

        const PolygonPointListType & points = this->GetPoints();
        auto it = points.begin();
        auto itend = points.end();
        itend--;

        PointType first = ( *it ).GetPositionInObjectSpace();

        bool oddNodes = false;

        PointType node1;
        PointType node2 = ( *it ).GetPositionInObjectSpace();
        for ( int i = 0; i < numpoints; i++ )
          {
          node1 = node2;
          it++;
          node2 = ( *it ).GetPositionInObjectSpace();

          if( node1 == node2 )
            {
            continue;
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

        if( oddNodes )
          {
          return true;
          }
        }
      }
    }

  if( depth > 0 )
    {
    return Superclass::IsInsideChildren( worldPoint, depth-1, name );
    }

  return false;
}

template< unsigned int TDimension >
void
PolygonSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "OrientationInObjectSpace: " << m_OrientationInObjectSpace
    << std::endl;
  os << indent << "OrientationInObjectSpace Time: "
    << m_OrientationInObjectSpaceMTime << std::endl;
  if( m_IsClosed )
    {
    os << indent << "IsClosed: True" << std::endl;
    }
  else
    {
    os << indent << "IsClosed: True" << std::endl;
    }
  os << indent << "IsClosed Time: " << m_IsClosedMTime << std::endl;
  os << indent << "Thickness: " << m_Thickness << std::endl;
}

} //namespace
#endif
