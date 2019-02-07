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
#ifndef itkArrowSpatialObject_hxx
#define itkArrowSpatialObject_hxx

#include "itkArrowSpatialObject.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
ArrowSpatialObject< TDimension >
::ArrowSpatialObject()
{
  this->SetTypeName("ArrowSpatialObject");
  this->GetProperty()->SetRed(1);
  this->GetProperty()->SetGreen(0);
  this->GetProperty()->SetBlue(0);
  this->GetProperty()->SetAlpha(1);

  m_ObjectDirection.Fill(0);
  m_ObjectDirection[0] = 1; // along the x direction by default
  m_ObjectPosition.Fill(0);
  m_ObjectLength = 1;

  m_WorldDirection.Fill(0);
  m_WorldDirection[0] = 1; // along the x direction by default
  m_WorldPosition.Fill(0);
  m_WorldLength = 1;
}

/** Set the length of the arrow */
template< unsigned int TDimension >
void
ArrowSpatialObject< TDimension >
::SetObjectLength(double length)
{
  m_ObjectLength = length;
  this->Modified();
}

/** Compute the bounding box */
template< unsigned int TDimension >
bool
ArrowSpatialObject< TDimension >
::ComputeObjectBoundingBox() const
{
  itkDebugMacro("Computing Rectangle bounding box");

  PointType pnt = this->GetPosition();
  PointType pnt2;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    pnt2[i] = pnt[i] + m_ObjectLength * m_ObjectDirection[i];
    }

  pnt = this->GetObjectToWorldTransform()->TransformPoint(pnt);
  pnt2 = this->GetObjectToWorldTransform()->TransformPoint(pnt2);

  const_cast< typename Superclass::BoundingBoxType * >(
    this->GetObjectBounds() )->SetMinimum(pnt);
  const_cast< typename Superclass::BoundingBoxType * >(
    this->GetObjectBounds() )->SetMaximum(pnt);
  const_cast< typename Superclass::BoundingBoxType * >(
    this->GetObjectBounds() )->ConsiderPoint(pnt2);

  return true;
}

/** Check if a given point is on the arrow */
template< unsigned int TDimension >
bool
ArrowSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  itkDebugMacro("Checking the point [" << point << "] is on the Line");

  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    if ( this->GetObjectBounds()->IsInside(point) )
      {
      PointType transformedPoint =
        this->GetObjectToWorldTransform()->GetInverse()->TransformPoint(point);

      PointType pnt = this->GetPosition();
      PointType pnt2;
      for ( unsigned int i = 0; i < TDimension; i++ )
        {
        pnt2[i] = pnt[i] + m_ObjectLength * m_ObjectDirection[i];
        }

      VectorType v = pnt2 - pnt;
      VectorType v2 = transformedPoint - pnt;

      v.Normalize();
      v2.Normalize();

      if ( Math::AlmostEquals( dot_product( v.GetVnlVector(), v2.GetVnlVector() ),
          NumericTraits< typename VectorType::ValueType >::OneValue() ) )
        {
        return true;
        }
      }
    }

  if( depth > 0 )
    {
    return Superclass::IsInsideChildren( point, depth-1, name );
    }

  return false;
}

/** Update world coordinate representation */
template< unsigned int TDimension >
void
ArrowSpatialObject< TDimension >
::Update()
{
  PointType pnt = this->GetPosition();
  PointType pnt2;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    pnt2[i] = pnt[i] + m_ObjectLength * m_ObjectDirection[i];
    }

  pnt = this->GetObjectToWorldTransform()->TransformPoint( pnt );
  pnt2 = this->GetObjectToWorldTransform()->TransformPoint( pnt2 );

  m_WorldPosition = pnt;
  m_WorldDirection = pnt2 - pnt;
  m_WorldDirection.Normalize();
  m_WorldLength = pnt.EuclideanDistanceTo( pnt2 );
}

template< unsigned int TDimension >
void
ArrowSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "ArrowSpatialObject(" << this << ")" << std::endl;
  Superclass::PrintSelf(os, indent);
  os << indent << "Object Position = " << m_ObjectPosition << std::endl;
  os << indent << "Object Direction = " << m_ObjectDirection << std::endl;
  os << indent << "Object Length = " << m_ObjectLength << std::endl;
  os << indent << "World Position = " << m_WorldPosition << std::endl;
  os << indent << "World Direction = " << m_WorldDirection << std::endl;
  os << indent << "World Length = " << m_WorldLength << std::endl;
}
} // end namespace itk

#endif // end itkArrowSpatialObject_hxx
