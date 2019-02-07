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

  m_DirectionInObjectSpace.Fill(0);
  m_DirectionInObjectSpace[0] = 1; // along the x direction by default
  m_PositionInObjectSpace.Fill(0);
  m_LengthInObjectSpace = 1;

  this->Update();
}

/** Destructor */
template< unsigned int TDimension >
ArrowSpatialObject< TDimension >
::~ArrowSpatialObject() = default;

/** Set the length of the arrow */
template< unsigned int TDimension >
void
ArrowSpatialObject< TDimension >
::SetLengthInObjectSpace(double length)
{
  m_LengthInObjectSpace = length;
  this->Modified();
}

/** Compute the bounding box */
template< unsigned int TDimension >
bool
ArrowSpatialObject< TDimension >
::ComputeMyBoundingBox() const
{
  itkDebugMacro("Computing Rectangle bounding box");

  PointType pnt = this->GetPosition();
  PointType pnt2;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    pnt2[i] = pnt[i] + m_LengthInObjectSpace * m_DirectionInObjectSpace[i];
    }

  pnt = this->GetObjectToWorldTransform()->TransformPoint(pnt);
  pnt2 = this->GetObjectToWorldTransform()->TransformPoint(pnt2);

  const_cast< typename Superclass::BoundingBoxType * >(
    this->GetMyBounds() )->SetMinimum(pnt);
  const_cast< typename Superclass::BoundingBoxType * >(
    this->GetMyBounds() )->SetMaximum(pnt);
  const_cast< typename Superclass::BoundingBoxType * >(
    this->GetMyBounds() )->ConsiderPoint(pnt2);

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
    if ( this->GetMyBounds()->IsInside(point) )
      {
      PointType transformedPoint =
        this->GetObjectToWorldTransform()->GetInverse()->TransformPoint(point);

      PointType pnt = this->GetPosition();
      PointType pnt2;
      for ( unsigned int i = 0; i < TDimension; i++ )
        {
        pnt2[i] = pnt[i] + m_LengthInObjectSpace * m_DirectionInObjectSpace[i];
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
    pnt2[i] = pnt[i] + m_LengthInObjectSpace * m_DirectionInObjectSpace[i];
    }

  pnt = this->GetObjectToWorldTransform()->TransformPoint( pnt );
  pnt2 = this->GetObjectToWorldTransform()->TransformPoint( pnt2 );

  m_Position = pnt;
  m_Direction = pnt2 - pnt;
  m_Direction.Normalize();
  m_Length = pnt.EuclideanDistanceTo( pnt2 );
}

template< unsigned int TDimension >
void
ArrowSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "ArrowSpatialObject(" << this << ")" << std::endl;
  Superclass::PrintSelf(os, indent);
  os << indent << "Object Position = " << m_PositionInObjectSpace << std::endl;
  os << indent << "Object Direction = " << m_DirectionInObjectSpace << std::endl;
  os << indent << "Object Length = " << m_LengthInObjectSpace << std::endl;
  os << indent << "World Position = " << m_Position << std::endl;
  os << indent << "World Direction = " << m_Direction << std::endl;
  os << indent << "World Length = " << m_Length << std::endl;
}
} // end namespace itk

#endif // end itkArrowSpatialObject_hxx
