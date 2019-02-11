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
#ifndef itkTubeSpatialObject_hxx
#define itkTubeSpatialObject_hxx


#include "itkMath.h"
#include "itkTubeSpatialObject.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension, typename TTubePointType >
TubeSpatialObject< TDimension, TTubePointType >
::TubeSpatialObject()
{
  this->SetTypeName("TubeSpatialObject");
  this->GetProperty()->SetRed(1);
  this->GetProperty()->SetGreen(0);
  this->GetProperty()->SetBlue(0);
  this->GetProperty()->SetAlpha(1);

  m_Root = false;
  m_Artery = true;
  m_ParentPoint = -1;
  m_EndRounded = false; // default end-type is flat
}

/** Set the list of points composing the tube */
template< unsigned int TDimension, typename TTubePointType >
void
TubeSpatialObject< TDimension, TTubePointType >
::SetPoints(TubePointListType & points)
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

/** Print the object */
template< unsigned int TDimension, typename TTubePointType >
void
TubeSpatialObject< TDimension, TTubePointType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "TubeSpatialObject(" << this << ")" << std::endl;
  os << indent << "End Type : " << m_EndRounded << std::endl;
  os << indent << "Parent Point : " << m_ParentPoint << std::endl;
  os << indent << "Root : " << m_Root << std::endl;
  Superclass::PrintSelf(os, indent);
}

/** Compute the bounds of the tube */
template< unsigned int TDimension, typename TTubePointType >
bool
TubeSpatialObject< TDimension, TTubePointType >
::ComputeMyBoundingBox() const
{
  itkDebugMacro("Computing tube bounding box");

  auto it  = m_Points.begin();
  auto end = m_Points.end();

  if ( it == end )
    {
    return false;
    }

  PointType pt = it->GetPositionInObjectSpace();
  double ptRadius = it->GetRadiusInObjectSpace();

  // Compute a bounding box in object space
  PointType tmpPt;
  typename BoundingBoxType::Pointer bb = BoundingBoxType::New();
  for( unsigned int d=0; d<ObjectDimension; ++d )
    {
    tmpPt[d] = pnt[d] - ptRadius;
    }
  bb->SetMinimum(tmpPt);

  for( unsigned int d=0; d<ObjectDimension; ++d )
    {
    tmpPt[d] = pnt[d] + ptRadius;
    }
  bb->SetMaximum(tmpPt);

  it++;
  while ( it != end )
    {
    pt = it->GetPositionInObjectSpace();
    ptRadius = it->GetRadiusInObjectSpace();
    for( unsigned int d=0; d<ObjectDimension; ++d )
      {
      tmpPt[d] = pnt[d] - ptRadius;
      }
    bb->ConsiderPoint(tmpPt);

    for( unsigned int d=0; d<ObjectDimension; ++d )
      {
      tmpPt[d] = pnt[d] + ptRadius;
      }
    bb->ConsiderPoint(tmpPt);

    it++;
    }
  bb->ComputeBoundingBox();

  // Next Transform the corners of the bounding box into world space
  using PointsContainer = typename BoundingBoxType::PointsContainer;
  const PointsContainer *corners = bb->GetCorners();
  typename PointsContainer::Pointer transformedCorners =
    PointsContainer::New();
  transformedCorners->Reserve(
    static_cast<typename PointsContainer::ElementIdentifier>(
      corners->size() ) );

  auto it = corners->begin();
  auto itTrans = transformedCorners->begin();
  while ( it != corners->end() )
    {
    PointType pnt = this->GetObjectToWorldTransform()->TransformPoint(*it);
    *itTrans = pnt;
    ++it;
    ++itTrans;
    }

  // refresh the object's bounding box with the transformed corners
  const_cast< BoundingBoxType * >( this->GetMyBoundingBox() )
    ->SetPoints(transformedCorners);
  this->GetMyBoundingBox()->ComputeBoundingBox();

  return true;
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< unsigned int TDimension, typename TTubePointType >
bool
TubeSpatialObject< TDimension, TTubePointType >
::IsInside(const PointType & point, unsigned int depth,
  const std::string & name ) const
{
  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    if( this->GetMyBoundingBox()->IsInside(point) )
      {
      double minSquareDist = 999999.0;
      double tempSquareDist;
      auto it = m_Points.begin();
      auto it2 = m_Points.begin();
      auto end = m_Points.end();
      typename PointListType::const_iterator min;

      PointType transformedPoint = this->GetObjectToWorldTransform()->
        GetInverseTransform()->TransformPoint(point);

      if ( !m_EndRounded ) // flat end-type
        {
        it2++; // next point
        while ( it2 != end )
          {
          // Check if the point is on the normal plane
          PointType a = ( *it ).GetPositionInObjectSpace();
          PointType b = ( *it2 ).GetPositionInObjectSpace();

          double A = 0;
          double B = 0;

          for ( unsigned int i = 0; i < TDimension; i++ )
            {
            A += ( b[i] - a[i] ) * ( transformedPoint[i] - a[i] );
            B += ( b[i] - a[i] ) * ( b[i] - a[i] );
            }

          double lambda = A / B;

          if ( ( ( it != m_Points.begin() )
                 && ( lambda > -( ( *it ).GetRadiusInObjectSpace()
                     / ( 2 * std::sqrt(B) ) ) )
                 && ( lambda < 0 ) )
               || ( ( lambda <= 1.0 ) && ( lambda >= 0.0 ) )
                )
            {
            PointType p;

            if ( lambda >= 0 )
              {
              for ( unsigned int i = 0; i < TDimension; i++ )
                {
                p[i] = a[i] + lambda * ( b[i] - a[i] );
                }
              }
            else
              {
              for ( unsigned int i = 0; i < TDimension; i++ )
                {
                p[i] = b[i] + lambda * ( b[i] - a[i] );
                }
              }

            // TODO: Verify not squared?
            tempSquareDist = transformedPoint.EuclideanDistanceTo(p);

            double R;
            if ( lambda >= 0 )
              {
              R = ( *it ).GetRadiusInObjectSpace()
                + lambda * ( ( *it2 ).GetRadiusInObjectSpace()
                  - ( *it ).GetRadiusInObjectSpace() );
              }
            else
              {
              R = ( *it2 ).GetRadiusInObjectSpace()
                + lambda * ( ( *it2 ).GetRadiusInObjectSpace()
                  - ( *it ).GetRadiusInObjectSpace() );
              }

            if ( tempSquareDist <= R )
              {
              return true;
              }
            }
          it++;
          it2++;
          }
        }
      else // rounded end-type
        {
        while ( it != end )
          {
          tempSquareDist = transformedPoint.SquaredEuclideanDistanceTo(
            ( *it ).GetPositionInObjectSpace() );
          if ( tempSquareDist <= minSquareDist )
            {
            minSquareDist = tempSquareDist;
            min = it;
            }
          it++;
          }

        double dist = std::sqrt(minSquareDist);
        if ( dist <= ( ( *min ).GetRadius() ) )
          {
          return true;
          }
        }
      }
    }

  if( depth > 0 )
    {
    return Superclass::IsInsideChildren( point, depth-1, name );
    }

  return false;
}

/** Remove duplicate points */
template< unsigned int TDimension, typename TTubePointType >
unsigned int
TubeSpatialObject< TDimension, TTubePointType >
::RemoveDuplicatePointsInObjectSpace( double minSpacingInObjectSpace )
{
  int nPoints = 0;

  auto it = m_Points.begin();
  while( it != m_Points.end() )
    {
    PointType pnt = it->GetPositionInObjectSpace();
    ++it;
    if( it != m_Points.end() )
      {
      PointType pnt2 = it->GetPositionInObjectSpace();
      double dist = pnt.EuclideanDistanceTo( pnt2 );
      if ( dist <= minSpacingInObjectSpace )
        {
        it = m_Points.erase( it );
        nPoints++;
        --it;
        }
      }
    }
  return nPoints;
}

/** Compute the tangent of the centerline of the tube */
template< unsigned int TDimension, typename TTubePointType >
bool
TubeSpatialObject< TDimension, TTubePointType >
::ComputeTangentAndNormals()
{
  itkDebugMacro("Computing the tangent vectors of the tube");

  int length = this->GetNumberOfPoints();
  if ( length == 0 )
    {
    return false;
    }

  PointType  x1, x3;
  VectorType t;
  double     l;
  t.Fill(0.0);

  if ( length == 1 )
    {
    ( (TubePointType *)( this->GetPoint(0) ) )->SetTangent(t);
    return true;
    }

  unsigned int it1 = 0;
  unsigned int it2 = 1;
  unsigned int it3 = 2;

  while ( it3 < (unsigned int)length )
    {
    x1 = this->GetPoint(it1)->GetPosition();
    x3 = this->GetPoint(it3)->GetPosition();
    l = 0;
    for ( unsigned int i = 0; i < TDimension; i++ )
      {
      t[i] = ( x3[i] - x1[i] );
      l = l + t[i] * t[i];
      }

    l = std::sqrt(l);
    if ( Math::AlmostEquals( l, 0.0 ) )
      {
      std::cerr << "TubeSpatialObject::ComputeTangentAndNormals() : ";
      std::cerr << "length between two consecutive points is 0";
      std::cerr << " (use RemoveDuplicatePoints())" << std::endl;
      std::cerr << "   p1 = " << x1 << std::endl;
      std::cerr << "   p3 = " << x3 << std::endl;
      return false;
      }
    for ( unsigned int i = 0; i < TDimension; i++ )
      {
      t[i] /= l;
      }

    ( (TubePointType *)( this->GetPoint(it2) ) )->SetTangent(t);
    it1++;
    it2++;
    it3++;
    }

  it1 = 0;
  it2 = 1;
  t = ( (TubePointType *)( this->GetPoint(it2) ) )->GetTangent();
  ( (TubePointType *)( this->GetPoint(it1) ) )->SetTangent(t);
  it1 = length - 1;
  it2 = length - 2;
  t = ( (TubePointType *)( this->GetPoint(it2) ) )->GetTangent();
  ( (TubePointType *)( this->GetPoint(it1) ) )->SetTangent(t);

  // Compute the normal
  CovariantVectorType n1;
  CovariantVectorType n2;

  it1 = 0;
  while ( it1 < (unsigned int)length )
    {
    t = ( (TubePointType *)( this->GetPoint(it1) ) )->GetTangent();

    if ( TDimension == 2 )
      {
      t = ( (TubePointType *)( this->GetPoint(it1) ) )->GetTangent();
      n1[0] = -t[1];
      n1[1] = t[0];
      ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal1(n1);
      }
    else if ( TDimension == 3 )
      {
      n1[0] = -t[1];
      n1[1] = t[0];
      n1[2] = 0;

      if ( n1[0] + n1[1] + n1[2] == 0.0 ) // if the normal is null
        {
        n1[0] = 0;
        n1[1] = -t[2];
        n1[2] = t[1];
        }

      n2[0] = t[1] * n1[2] - t[2] * n1[1];
      n2[1] = t[2] * n1[0] - t[0] * n1[2];
      n2[2] = t[0] * n1[1] - t[1] * n1[0];

      ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal1(n1);
      ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal2(n2);
      }

    it1++;
    }

  it1 = 0;
  it2 = 1;
  n1 = ( (TubePointType *)( this->GetPoint(it2) ) )->GetNormal1();
  ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal1(n1);

  if ( TDimension == 3 )
    {
    n2 = ( (TubePointType *)( this->GetPoint(it2) ) )->GetNormal2();
    ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal2(n2);
    }

  it1 = length - 1;
  it2 = length - 2;
  n1 = ( (TubePointType *)( this->GetPoint(it2) ) )->GetNormal1();
  ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal1(n1);

  if ( TDimension == 3 )
    {
    n2 = ( (TubePointType *)( this->GetPoint(it2) ) )->GetNormal2();
    ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal2(n2);
    }

  return true;
}

/** Copy the information from another spatial object */
template< unsigned int TDimension, typename TTubePointType >
void
TubeSpatialObject< TDimension, TTubePointType >
::DeepCopy(const DataObject *data)
{
  // check if we are the same type
  const auto * source = dynamic_cast< const Self * >( data );

  if ( source == nullptr )
    {
    std::cout << "CopyInformation: objects are not of the same type"
              << std::endl;
    return;
    }

  // copy the properties
  Superclass::CopyInformation(data);

  // copy the internal info
  this->SetRoot( source->GetRoot() );
  this->SetArtery( source->GetArtery() );
  this->SetParentPoint( source->GetParentPoint() );
  this->SetEndType( source->GetEndType() );

  // We copy the points
  PointListType source_list = source->GetPoints();
  typename PointListType::const_iterator it_source = source_list.begin();

  this->m_Points.clear();

  while ( it_source != source_list.end() )
    {
    this->m_Points.push_back(*it_source);
    it_source++;
    }
}
} // end namespace itk

#endif // end itkTubeSpatialObject_hxx
