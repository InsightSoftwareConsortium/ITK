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
  m_Root = false;
  m_Artery = true;
  m_ParentPoint = -1;
  this->SetDimension(TDimension);
  this->SetTypeName("TubeSpatialObject");
  this->GetProperty()->SetRed(1);
  this->GetProperty()->SetGreen(0);
  this->GetProperty()->SetBlue(0);
  this->GetProperty()->SetAlpha(1);
  m_OldMTime = 0;
  m_IndexToWorldTransformMTime = 0;
  m_EndType = 0; // default end-type is flat
}

/** Destructor */
template< unsigned int TDimension, typename TTubePointType >
TubeSpatialObject< TDimension, TTubePointType >
::~TubeSpatialObject()
{}

/** Get the list of points composing the tube */
template< unsigned int TDimension, typename TTubePointType >
typename TubeSpatialObject< TDimension, TTubePointType >::PointListType &
TubeSpatialObject< TDimension, TTubePointType >
::GetPoints()
{
  itkDebugMacro("Getting TubePoint list");
  return m_Points;
}

/** Get the list of points composing the tube */
template< unsigned int TDimension, typename TTubePointType >
const typename
TubeSpatialObject< TDimension, TTubePointType >::PointListType &
TubeSpatialObject< TDimension, TTubePointType >
::GetPoints() const
{
  itkDebugMacro("Getting TubePoint list");
  return m_Points;
}

/** Set the list of points composing the tube */
template< unsigned int TDimension, typename TTubePointType >
void
TubeSpatialObject< TDimension, TTubePointType >
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

/** Remove the list of tube points */
template< unsigned int TDimension, typename TTubePointType >
void
TubeSpatialObject< TDimension, TTubePointType >
::Clear(void)
{
  m_Points.clear();
}

/** Print the object */
template< unsigned int TDimension, typename TTubePointType >
void
TubeSpatialObject< TDimension, TTubePointType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "TubeSpatialObject(" << this << ")" << std::endl;
  os << indent << "ID: " << this->GetId() << std::endl;
  os << indent << "nb of points: "
     << static_cast< SizeValueType >( m_Points.size() ) << std::endl;
  os << indent << "End Type : " << m_EndType << std::endl;
  os << indent << "Parent Point : " << m_ParentPoint << std::endl;
  os << indent << "Root : " << m_Root << std::endl;
  os << indent << "Artery : " << m_Artery << std::endl;
  Superclass::PrintSelf(os, indent);
}

/** Compute the bounds of the tube */
template< unsigned int TDimension, typename TTubePointType >
bool
TubeSpatialObject< TDimension, TTubePointType >
::ComputeLocalBoundingBox() const
{
  itkDebugMacro("Computing tube bounding box");

  // Check if the IndexToWorldTransform or the object itself has been modified
  if ( ( this->GetMTime() == m_OldMTime )
       && ( m_IndexToWorldTransformMTime ==
            this->GetIndexToWorldTransform()->GetMTime() )
        )
    {
    return true; // if not modified we return
    }

  m_OldMTime = this->GetMTime();
  m_IndexToWorldTransformMTime = this->GetIndexToWorldTransform()->GetMTime();

  if ( this->GetBoundingBoxChildrenName().empty()
       || strstr( typeid( Self ).name(), this->GetBoundingBoxChildrenName().c_str() ) )
    {
    typename PointListType::const_iterator it  = m_Points.begin();
    typename PointListType::const_iterator end = m_Points.end();

    if ( it == end )
      {
      return false;
      }
    else
      {
      // First we compute the bounding box in the index space
      typename BoundingBoxType::Pointer bb = BoundingBoxType::New();
      VectorType rad(( *it ).GetRadius());
      PointType ptMin = ( *it ).GetPosition() - rad;
      PointType ptMax = ( *it ).GetPosition() + rad;
      bb->SetMinimum(ptMin);
      bb->SetMaximum(ptMax);

      ptMin = this->GetIndexToWorldTransform()->TransformPoint(ptMin);
      const_cast< BoundingBoxType * >( this->GetBounds() )->SetMinimum(ptMin);
      ptMax = this->GetIndexToWorldTransform()->TransformPoint(ptMax);
      const_cast< BoundingBoxType * >( this->GetBounds() )->SetMaximum(ptMax);

      it++;
      while ( it != end )
        {
        rad = VectorType(( *it ).GetRadius());
        ptMin = ( *it ).GetPosition() - rad;
        ptMax = ( *it ).GetPosition() + rad;
        bb->ConsiderPoint(ptMin);
        bb->ConsiderPoint(ptMax);
        it++;
        }

      typedef typename BoundingBoxType::PointsContainer PointsContainer;
      const PointsContainer *corners = bb->GetCorners();
      typename BoundingBoxType::PointsContainer::const_iterator
      itBB = corners->begin();
      while ( itBB != corners->end() )
        {
        PointType pnt =
          this->GetIndexToWorldTransform()->TransformPoint(*itBB);
        const_cast< BoundingBoxType * >( this->GetBounds() )->ConsiderPoint(pnt);
        ++itBB;
        }
      }
    }
  return true;
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< unsigned int TDimension, typename TTubePointType >
bool
TubeSpatialObject< TDimension, TTubePointType >
::IsInside(const PointType & point) const
{
  this->ComputeLocalBoundingBox();
  if ( !this->GetBounds()->IsInside(point) )
    {
    return false;
    }

  double minSquareDist = 999999.0;
  double tempSquareDist;
  typename PointListType::const_iterator it = m_Points.begin();
  typename PointListType::const_iterator it2 = m_Points.begin();
  typename PointListType::const_iterator end = m_Points.end();
  typename PointListType::const_iterator min;

  if ( !this->SetInternalInverseTransformToWorldToIndexTransform() )
    {
    return false;
    }

  PointType transformedPoint =
    this->GetInternalInverseTransform()->TransformPoint(point);

  if ( m_EndType == 0 ) // flat end-type
    {
    it2++; // next point
    while ( it2 != end )
      {
      // Check if the point is on the normal plane
      PointType a = ( *it ).GetPosition();
      PointType b = ( *it2 ).GetPosition();

      double A = 0;
      double B = 0;

      for ( unsigned int i = 0; i < TDimension; i++ )
        {
        A += ( b[i] - a[i] ) * ( transformedPoint[i] - a[i] );
        B += ( b[i] - a[i] ) * ( b[i] - a[i] );
        }

      double lambda = A / B;

      if ( ( ( it != m_Points.begin() )
             && ( lambda > -( ( *it ).GetRadius() / ( 2 * std::sqrt(B) ) ) )
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

        tempSquareDist = transformedPoint.EuclideanDistanceTo(p);

        double R;
        if ( lambda >= 0 )
          {
          R = ( *it ).GetRadius() + lambda * ( ( *it2 ).GetRadius() - ( *it ).GetRadius() );
          }
        else
          {
          R = ( *it2 ).GetRadius() + lambda * ( ( *it2 ).GetRadius() - ( *it ).GetRadius() );
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
  else if ( m_EndType == 1 ) // rounded end-type
    {
    while ( it != end )
      {
      tempSquareDist = transformedPoint.SquaredEuclideanDistanceTo(
        ( *it ).GetPosition() );
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
  return false;
}

/** Return true if the given point is inside the tube */
template< unsigned int TDimension, typename TTubePointType >
bool
TubeSpatialObject< TDimension, TTubePointType >
::IsInside(const PointType & point, unsigned int depth, char *name) const
{
  itkDebugMacro("Checking the point [" << point << "] is inside the tube");

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

/** Remove duplicate points */
template< unsigned int TDimension, typename TTubePointType >
unsigned int
TubeSpatialObject< TDimension, TTubePointType >
::RemoveDuplicatePoints( unsigned int itkNotUsed(step) )
{
  int length = this->GetNumberOfPoints();

  if ( length <= 1 )
    {
    return 0;
    }

  int nPoints = 0;
  for ( int i = 0; i < length - 1; i++ )
    {
    if ( this->GetPoint(i)->GetPosition() == this->GetPoint(i + 1)->GetPosition() )
      {
      this->RemovePoint(i + 1);
      i--;
      length--;
      nPoints++;
      }
    if ( i >= 0 && i < length - 2
         && this->GetPoint(i)->GetPosition() == this->GetPoint(i + 2)->GetPosition() )
      {
      this->RemovePoint(i + 2);
      i--;
      length--;
      nPoints++;
      }
    }

  return nPoints;
}

/** Compute the tangent of the centerline of the tube */
template< unsigned int TDimension, typename TTubePointType >
bool
TubeSpatialObject< TDimension, TTubePointType >
::ComputeTangentAndNormals(void)
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

/** Return true if the tube is evaluable at a given point */
template< unsigned int TDimension, typename TTubePointType >
bool
TubeSpatialObject< TDimension, TTubePointType >
::IsEvaluableAt(const PointType & point,
                unsigned int depth, char *name) const
{
  itkDebugMacro("Checking if the tube is evaluable at " << point);
  return IsInside(point, depth, name);
}

/** Return the value of the tube at a specified point */
template< unsigned int TDimension, typename TTubePointType >
bool
TubeSpatialObject< TDimension, TTubePointType >
::ValueAt(const PointType & point, double & value, unsigned int depth,
          char *name) const
{
  itkDebugMacro("Getting the value of the tube at " << point);
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

/** Copy the information from another spatial object */
template< unsigned int TDimension, typename TTubePointType >
void
TubeSpatialObject< TDimension, TTubePointType >
::CopyInformation(const DataObject *data)
{
  // check if we are the same type
  const Self *source = dynamic_cast< const Self * >( data );

  if ( source == ITK_NULLPTR )
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
