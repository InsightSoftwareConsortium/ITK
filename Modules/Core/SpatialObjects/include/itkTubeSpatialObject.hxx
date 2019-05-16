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

  this->Clear();

  this->Update();
}

template< unsigned int TDimension, typename TTubePointType >
void
TubeSpatialObject< TDimension, TTubePointType >
::Clear( void )
{
  Superclass::Clear();

  this->GetProperty().SetRed(1);
  this->GetProperty().SetGreen(0);
  this->GetProperty().SetBlue(0);
  this->GetProperty().SetAlpha(1);

  m_Root = false;
  m_ParentPoint = -1;
  m_EndRounded = false; // default end-type is flat

  this->Modified();
}

/** InternalClone */
template< unsigned int TDimension, typename TTubePointType >
typename LightObject::Pointer
TubeSpatialObject< TDimension, TTubePointType >
::InternalClone() const
{
  // Default implementation just copies the parameters from
  // this to new transform.
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval =
    dynamic_cast<Self *>(loPtr.GetPointer());
  if(rval.IsNull())
    {
    itkExceptionMacro(<< "downcast to type "
                      << this->GetNameOfClass()
                      << " failed.");
    }
  rval->SetEndRounded( this->GetEndRounded() );
  rval->SetParentPoint( this->GetParentPoint() );
  rval->SetRoot(this->GetRoot());

  return loPtr;
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
void
TubeSpatialObject< TDimension, TTubePointType >
::ComputeMyBoundingBox()
{
  itkDebugMacro("Computing tube bounding box");

  auto it  = this->m_Points.begin();
  auto end = this->m_Points.end();

  if ( it == end )
    {
    typename BoundingBoxType::PointType pnt;
    pnt.Fill( NumericTraits< typename BoundingBoxType::PointType::ValueType >::
      ZeroValue() );
    this->GetModifiableMyBoundingBoxInObjectSpace()->SetMinimum(pnt);
    this->GetModifiableMyBoundingBoxInObjectSpace()->SetMaximum(pnt);
    return;
    }

  PointType pt = it->GetPositionInObjectSpace();
  double ptRadius = it->GetRadiusInObjectSpace();

  // Compute a bounding box in object space
  PointType tmpPt;
  for( unsigned int d=0; d<TDimension; ++d )
    {
    tmpPt[d] = pt[d] - ptRadius;
    }
  this->GetModifiableMyBoundingBoxInObjectSpace()->SetMinimum(tmpPt);
  this->GetModifiableMyBoundingBoxInObjectSpace()->SetMaximum(tmpPt);

  for( unsigned int d=0; d<TDimension; ++d )
    {
    tmpPt[d] = pt[d] + ptRadius;
    }
  this->GetModifiableMyBoundingBoxInObjectSpace()->ConsiderPoint(tmpPt);

  it++;
  while ( it != end )
    {
    pt = it->GetPositionInObjectSpace();
    ptRadius = it->GetRadiusInObjectSpace();
    for( unsigned int d=0; d<TDimension; ++d )
      {
      tmpPt[d] = pt[d] - ptRadius;
      }
    this->GetModifiableMyBoundingBoxInObjectSpace()->ConsiderPoint(tmpPt);

    for( unsigned int d=0; d<TDimension; ++d )
      {
      tmpPt[d] = pt[d] + ptRadius;
      }
    this->GetModifiableMyBoundingBoxInObjectSpace()->ConsiderPoint(tmpPt);

    it++;
    }
  this->GetModifiableMyBoundingBoxInObjectSpace()->ComputeBoundingBox();
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< unsigned int TDimension, typename TTubePointType >
bool
TubeSpatialObject< TDimension, TTubePointType >
::IsInsideInObjectSpace(const PointType & point) const
{
  if( this->GetMyBoundingBoxInObjectSpace()->IsInside(point) )
    {
    double minSquareDist = 999999.0;
    double tempSquareDist;
    auto it = this->m_Points.begin();
    auto it2 = this->m_Points.begin();
    auto end = this->m_Points.end();
    auto minIt = it;

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
          A += ( b[i] - a[i] ) * ( point[i] - a[i] );
          B += ( b[i] - a[i] ) * ( b[i] - a[i] );
          }

        double lambda = A / B;

        if ( ( ( it != this->m_Points.begin() )
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
          tempSquareDist = point.EuclideanDistanceTo(p);

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
        tempSquareDist = point.SquaredEuclideanDistanceTo(
          ( *it ).GetPositionInObjectSpace() );
        if ( tempSquareDist <= minSquareDist )
          {
          minSquareDist = tempSquareDist;
          minIt = it;
          }
        it++;
        }

      double dist = std::sqrt(minSquareDist);
      if ( dist <= ( minIt->GetRadiusInObjectSpace() ) )
        {
        return true;
        }
      }
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

  auto it = this->m_Points.begin();
  while( it != this->m_Points.end() )
    {
    PointType pnt = it->GetPositionInObjectSpace();
    ++it;
    if( it != this->m_Points.end() )
      {
      PointType pnt2 = it->GetPositionInObjectSpace();
      double dist = pnt.EuclideanDistanceTo( pnt2 );
      if ( dist <= minSpacingInObjectSpace )
        {
        it = this->m_Points.erase( it );
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
    ( (TubePointType *)( this->GetPoint(0) ) )->SetTangentInObjectSpace(t);
    return true;
    }

  unsigned int it1 = 0;
  unsigned int it2 = 1;
  unsigned int it3 = 2;

  while ( it3 < (unsigned int)length )
    {
    x1 = this->GetPoint(it1)->GetPositionInObjectSpace();
    x3 = this->GetPoint(it3)->GetPositionInObjectSpace();
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

    ( (TubePointType *)( this->GetPoint(it2) ) )->SetTangentInObjectSpace(t);
    it1++;
    it2++;
    it3++;
    }

  it1 = 0;
  it2 = 1;
  t = ( (TubePointType *)( this->GetPoint(it2) ) )->GetTangentInObjectSpace();
  ( (TubePointType *)( this->GetPoint(it1) ) )->SetTangentInObjectSpace(t);
  it1 = length - 1;
  it2 = length - 2;
  t = ( (TubePointType *)( this->GetPoint(it2) ) )->GetTangentInObjectSpace();
  ( (TubePointType *)( this->GetPoint(it1) ) )->SetTangentInObjectSpace(t);

  // Compute the normal
  CovariantVectorType n1;
  CovariantVectorType n2;

  it1 = 0;
  while ( it1 < (unsigned int)length )
    {
    t = ( (TubePointType *)( this->GetPoint(it1) ) )->GetTangentInObjectSpace();

    if ( TDimension == 2 )
      {
      t = ( (TubePointType *)( this->GetPoint(it1) ) )->GetTangentInObjectSpace();
      n1[0] = -t[1];
      n1[1] = t[0];
      ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal1InObjectSpace(n1);
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

      ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal1InObjectSpace(n1);
      ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal2InObjectSpace(n2);
      }

    it1++;
    }

  it1 = 0;
  it2 = 1;
  n1 = ( (TubePointType *)( this->GetPoint(it2) ) )->GetNormal1InObjectSpace();
  ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal1InObjectSpace(n1);

  if ( TDimension == 3 )
    {
    n2 = ( (TubePointType *)( this->GetPoint(it2) ) )->GetNormal2InObjectSpace();
    ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal2InObjectSpace(n2);
    }

  it1 = length - 1;
  it2 = length - 2;
  n1 = ( (TubePointType *)( this->GetPoint(it2) ) )->GetNormal1InObjectSpace();
  ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal1InObjectSpace(n1);

  if ( TDimension == 3 )
    {
    n2 = ( (TubePointType *)( this->GetPoint(it2) ) )->GetNormal2InObjectSpace();
    ( (TubePointType *)( this->GetPoint(it1) ) )->SetNormal2InObjectSpace(n2);
    }

  return true;
}


} // end namespace itk

#endif // end itkTubeSpatialObject_hxx
