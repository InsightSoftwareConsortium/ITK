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
  this->GetProperty().SetRed(1);
  this->GetProperty().SetGreen(0);
  this->GetProperty().SetBlue(0);
  this->GetProperty().SetAlpha(1);

  m_DirectionInObjectSpace.Fill(0);
  m_DirectionInObjectSpace[0] = 1; // along the x direction by default
  m_PositionInObjectSpace.Fill(0);
  m_LengthInObjectSpace = 1;

  this->Update();
}

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

  PointType pnt = this->GetPositionInObjectSpace();

  const_cast< typename Superclass::BoundingBoxType * >(
    this->GetMyBoundingBoxInObjectSpace() )->SetMinimum(pnt);
  const_cast< typename Superclass::BoundingBoxType * >(
    this->GetMyBoundingBoxInObjectSpace() )->SetMaximum(pnt);

  return true;
}

/** Check if a given point is on the arrow */
template< unsigned int TDimension >
bool
ArrowSpatialObject< TDimension >
::IsInsideInObjectSpace(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  itkDebugMacro("Checking the point [" << point << "] is on the Line");

  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    PointType pnt = this->GetPositionInObjectSpace();

    PointType tPnt = this->GetObjectToWorldTransform()
      ->GetInverseTransform()->TransformPoint(point);

    bool isInside = true;
    for ( unsigned int i = 0; i < TDimension; i++ )
      {
      if( Math::NotExactlyEquals( pnt[i], tPnt[i] ) )
        {
        isInside = false;
        break;
        }
      }
    if( isInside )
      {
      return true;
      }
    }

  if( depth > 0 )
    {
    std::cout << "Testing children" << std::endl;
    return Superclass::IsInsideChildrenInObjectSpace( point, depth-1, name );
    }

  return false;
}

/** Update world coordinate representation */
template< unsigned int TDimension >
void
ArrowSpatialObject< TDimension >
::Update()
{
  PointType pnt = this->GetPositionInObjectSpace();
  PointType pnt2;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    pnt2[i] = pnt[i] + m_LengthInObjectSpace * m_DirectionInObjectSpace[i];
    }

  pnt = this->GetObjectToWorldTransform()->TransformPoint( pnt );
  pnt2 = this->GetObjectToWorldTransform()->TransformPoint( pnt2 );

  m_PositionInWorldSpace = pnt;
  m_DirectionInWorldSpace = pnt2 - pnt;
  m_DirectionInWorldSpace.Normalize();
  m_LengthInWorldSpace = pnt.EuclideanDistanceTo( pnt2 );

  Superclass::Update();
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
  os << indent << "World Position = " << m_PositionInWorldSpace << std::endl;
  os << indent << "World Direction = " << m_DirectionInWorldSpace << std::endl;
  os << indent << "World Length = " << m_LengthInWorldSpace << std::endl;
}
} // end namespace itk

#endif // end itkArrowSpatialObject_hxx
