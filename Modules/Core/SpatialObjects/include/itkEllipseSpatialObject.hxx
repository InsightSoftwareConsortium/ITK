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
#ifndef itkEllipseSpatialObject_hxx
#define itkEllipseSpatialObject_hxx

#include "itkEllipseSpatialObject.h"
#include <cstring>

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
EllipseSpatialObject< TDimension >
::EllipseSpatialObject()
{
  this->SetTypeName("EllipseSpatialObject");
  m_Radius.Fill(1.0);
  m_Center.Fill(1.0);
}

/** Destructor */
template< unsigned int TDimension >
EllipseSpatialObject< TDimension >
::~EllipseSpatialObject() = default;

/** Set all radii to the same radius value */
template< unsigned int TDimension >
void
EllipseSpatialObject< TDimension >
::SetRadius(double radius)
{
  for ( unsigned int i = 0; i < ObjectDimension; i++ )
    {
    m_Radius[i] = radius;
    }
  this->Modified();
}

template< unsigned int TDimension >
auto
EllipseSpatialObject<TDimension>::GetCenterPoint() const
-> PointType
{
  return m_Center
}

template< unsigned int TDimension >
void
EllipseSpatialObject<TDimension>::SetCenterPoint(const PointType& point)
{
  for ( unsigned int i = 0; i < ObjectDimension; i++ )
    {
    m_Center[i] = point[i];
    }
  this->Modified();
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< unsigned int TDimension >
bool
EllipseSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    PointType transformedPoint =
      this->GetObjectToWorldTransform()->GetInverse()->TransformPoint(point);

    double r = 0;
    for ( unsigned int i = 0; i < TDimension; i++ )
      {
      if ( m_Radius[i] != 0.0 )
        {
        r += ( transformedPoint[i] * transformedPoint[i] )
             / ( m_Radius[i] * m_Radius[i] );
        }
      else if ( transformedPoint[i] > 0.0 )  // Degenerate ellipse
        {
        r = 2; // Keeps function from returning true here
        break;
        }
      }

    if ( r < 1 )
      {
      return true;
      }
    }

  if( depth > 0 )
    {
    return Superclass::IsInsideChildren( point, depth-1, name );
    }

  return false;
}

/** Compute the bounds of the ellipse */
template< unsigned int TDimension >
bool
EllipseSpatialObject< TDimension >
::ComputeObjectBoundingBox() const
{
  itkDebugMacro("Computing ellipse bounding box");

  // First we compute the bounding box in the object space
  typename BoundingBoxType::Pointer bb = BoundingBoxType::New();

  PointType    pnt1;
  PointType    pnt2;
  unsigned int i;
  for ( i = 0; i < TDimension; i++ )
    {
    pnt1[i] = m_Center[i] - m_Radius[i];
    pnt2[i] = m_Center[i] + m_Radius[i];
    }

  bb->SetMinimum(pnt1);
  bb->SetMaximum(pnt1);
  bb->ConsiderPoint(pnt2);
  bb->ComputeBoundingBox();

  // Next Transform the corners of the bounding box
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

  // refresh the bounding box with the transformed corners
  const_cast< BoundingBoxType * >( this->GetObjectBounds() )
    ->SetPoints(transformedCorners);
  this->GetObjectBounds()->ComputeBoundingBox();

  return true;
}

/** Print Self function */
template< unsigned int TDimension >
void
EllipseSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Radius: " << m_Radius << std::endl;
  os << "Center: " << m_Center << std::endl;
}

} // end namespace itk

#endif
