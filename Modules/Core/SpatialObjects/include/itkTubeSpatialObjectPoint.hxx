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

#ifndef itkTubeSpatialObjectPoint_hxx
#define itkTubeSpatialObjectPoint_hxx

#include "itkTubeSpatialObjectPoint.h"

namespace itk
{
/** Constructor */
template< unsigned int TPointDimension >
TubeSpatialObjectPoint< TPointDimension >
::TubeSpatialObjectPoint()
{
  m_TangentInObjectSpace.Fill(0);
  m_Normal1InObjectSpace.Fill(0);
  m_Normal2InObjectSpace.Fill(0);
  m_RadiusInObjectSpace = 0;
}

/** Destructor */
template< unsigned int TPointDimension >
TubeSpatialObjectPoint< TPointDimension >
::~TubeSpatialObjectPoint() = default;

/** Get the radius */
template< unsigned int TPointDimension >
float
TubeSpatialObjectPoint< TPointDimension >
::GetRadiusInObjectSpace() const
{
  return m_RadiusInObjectSpace;
}

/** Get the radius */
template< unsigned int TPointDimension >
float
TubeSpatialObjectPoint< TPointDimension >
::GetRadius() const
{
  CovariantVectorType cVect;
  cVect.Fill( m_RadiusInObjectSpace );
  cVect = Superclass::m_SpatialObject->GetObjectToWorldTransform()->
    TransformCovariantVector( cVect );
  double worldR = 0;
  for( unsigned int d=0; d<TPointDimension; ++d )
    {
    worldR += cVect[d];
    }
  worldR /= TPointDimension;
  return worldR;
}

/** Set the radius */
template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetRadiusInObjectSpace(float newR)
{
  m_RadiusInObjectSpace = newR;
}

/** Get the radius */
template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetRadius(float newR)
{
  CovariantVectorType cVect;
  cVect.Fill( newR );
  cVect = Superclass::m_SpatialObject->GetObjectToWorldTransform()->GetInverseTransform()->
    TransformCovariantVector( cVect );
  m_RadiusInObjectSpace = 0;
  for( unsigned int d=0; d<TPointDimension; ++d )
    {
    m_RadiusInObjectSpace += cVect[d];
    }
  m_RadiusInObjectSpace /= TPointDimension;
}

template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::VectorType &
TubeSpatialObjectPoint< TPointDimension >
::GetTangentInObjectSpace() const
{
  return m_TangentInObjectSpace;
}

template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::VectorType &
TubeSpatialObjectPoint< TPointDimension >
::GetTangent() const
{
  return Superclass::m_SpatialObject->GetObjectToWorldTransform()->
    TransformVector( m_TangentInObjectSpace );
}

template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetTangentInObjectSpace(const VectorType & newT)
{
  m_TangentInObjectSpace = newT;
}

template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetTangent(const VectorType & newT)
{
  m_TangentInObjectSpace = Superclass::m_SpatialObject->GetObjectToWorldTransform()->
    GetInveseTransform()->TransformVector( newT );
}

template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::CovariantVectorType &
TubeSpatialObjectPoint< TPointDimension >
::GetNormal1InObjectSpace() const
{
  return m_Normal1InObjectSpace;
}

template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::CovariantVectorType &
TubeSpatialObjectPoint< TPointDimension >
::GetNormal1() const
{
  return Superclass::m_SpatialObject->GetObjectToWorldTransform()->
    TrasnformCovariantVector( m_Normal1InObjectSpace );
}

template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetNormal1InObjectSpace(const CovariantVectorType & newV1)
{
  m_Normal1InObjectSpace = newV1;
}

template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetNormal1(const CovariantVectorType & newV1)
{
  m_Normal1InObjectSpace = Superclass::m_SpatialObject->GetObjectToWorldTransform()->
    GetInverseTransform()->TransformCovariantVector( newV1 );
}

template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::CovariantVectorType &
TubeSpatialObjectPoint< TPointDimension >
::GetNormal2InObjectSpace() const
{
  return m_Normal2InObjectSpace;
}

template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::CovariantVectorType &
TubeSpatialObjectPoint< TPointDimension >
::GetNormal2() const
{
  return Superclass::m_SpatialObject->GetObjectToWorldTransform()->
    TrasnformCovariantVector( m_Normal2InObjectSpace );
}

template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetNormal2InObjectSpace(const CovariantVectorType & newV2)
{
  m_Normal2InObjectSpace = newV2;
}

template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetNormal2(const CovariantVectorType & newV2)
{
  m_Normal2InObjectSpace = Superclass::m_SpatialObject->GetObjectToWorldTransform()->
    GetInverseTransform()->TransformCovariantVector( newV2 );
}

template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Radius In Object Space: " << m_RadiusInObjectSpace
    << std::endl;
  os << indent << "Tangent In Object Space: " << m_TangentInObjectSpace
    << std::endl;
  os << indent << "Normal1 In Object Space: " << m_Normal1InObjectSpace
    << std::endl;
  os << indent << "Normal2 In Object Space: " << m_Normal2InObjectSpace
    << std::endl;
}

template< unsigned int TPointDimension >
typename TubeSpatialObjectPoint< TPointDimension >::Self &
TubeSpatialObjectPoint< TPointDimension >
::operator=(const TubeSpatialObjectPoint & rhs)
{
  if(this != &rhs)
    {
    // Superclass
    this->SetId( rhs.GetId() );
    this->SetPositionInObjectSpace( rhs.GetPositionInObjectSpace() );
    this->SetColor( rhs.GetColor() );

    // class
    this->SetRadiusInObjectSpace( rhs.GetRadiusInObjectSpace() );
    this->SetTangentInObjectSpace( rhs.GetTangentInObjectSpace() );
    this->SetNormal1InObjectSpace( rhs.GetNormal1InObjectSpace() );
    this->SetNormal2InObjectSpace( rhs.GetNormal2InObjectSpace() );
    }
  return *this;
}
} // end namespace itk

#endif
