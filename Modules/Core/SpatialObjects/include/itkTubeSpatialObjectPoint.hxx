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
::TubeSpatialObjectPoint(void)
{
  m_NumDimensions = TPointDimension;
  m_T.Fill(0);
  m_Normal1.Fill(0);
  m_Normal2.Fill(0);
  m_R = 0;
}

/** Destructor */
template< unsigned int TPointDimension >
TubeSpatialObjectPoint< TPointDimension >
::~TubeSpatialObjectPoint(void)
{}

/** Get the radius */
template< unsigned int TPointDimension >
float
TubeSpatialObjectPoint< TPointDimension >
::GetRadius(void) const
{
  return m_R;
}

/** Set the radius */
template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetRadius(const float newR)
{
  m_R = newR;
}

template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::VectorType &
TubeSpatialObjectPoint< TPointDimension >
::GetTangent(void) const
{
  return m_T;
}

// n-D case
template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetTangent(const VectorType & newT)
{
  m_T = newT;
}

// 3-D case
template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetTangent(const double t0, const double t1, const double t2)
{
  m_T[0] = t0;
  m_T[1] = t1;
  m_T[2] = t2;
}

// 2-D case
template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetTangent(const double t0, const double t1)
{
  m_T[0] = t0;
  m_T[1] = t1;
}

template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::CovariantVectorType &
TubeSpatialObjectPoint< TPointDimension >
::GetNormal1() const
{
  return m_Normal1;
}

// n-D case
template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetNormal1(const CovariantVectorType & newV1)
{
  m_Normal1 = newV1;
}

// 3-D case
template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetNormal1(const double v10, const double v11, const double v12)
{
  m_Normal1[0] = v10;
  m_Normal1[1] = v11;
  m_Normal1[2] = v12;
}

// 2-D case
template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetNormal1(const double v10, const double v11)
{
  m_Normal1[0] = v10;
  m_Normal1[1] = v11;
}

template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::CovariantVectorType &
TubeSpatialObjectPoint< TPointDimension >
::GetNormal2() const
{
  return m_Normal2;
}

// n-D case
template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetNormal2(const CovariantVectorType & newV2)
{
  m_Normal2 = newV2;
}

// 3-D case
template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetNormal2(const double v20, const double v21, const double v22)
{
  m_Normal2[0] = v20;
  m_Normal2[1] = v21;
  m_Normal2[2] = v22;
}

// 2-D case
template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::SetNormal2(const double v20, const double v21)
{
  m_Normal2[0] = v20;
  m_Normal2[1] = v21;
}

template< unsigned int TPointDimension >
unsigned short int
TubeSpatialObjectPoint< TPointDimension >
::GetNumDimensions(void) const
{
  return m_NumDimensions;
}

template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "#Dims: " << m_NumDimensions << std::endl;
  os << indent << "R: " << m_R << std::endl;
  os << indent << "X: " << this->m_X << std::endl;
  os << indent << "T: " << m_T << std::endl;
  os << indent << "Normal1: " << m_Normal1 << std::endl;
  os << indent << "Normal2: " << m_Normal2 << std::endl;
}

template< unsigned int TPointDimension >
typename TubeSpatialObjectPoint< TPointDimension >::Self &
TubeSpatialObjectPoint< TPointDimension >
::operator=(const TubeSpatialObjectPoint & rhs)
{
  if(this != &rhs)
    {
    this->m_ID = rhs.m_ID;
    m_R = rhs.m_R;
    m_NumDimensions = rhs.m_NumDimensions;
    this->m_X = rhs.m_X;
    m_T = rhs.m_T;
    m_Normal1 = rhs.m_Normal1;
    m_Normal2 = rhs.m_Normal2;
    this->m_Color = rhs.m_Color;
    }
  return *this;
}
} // end namespace itk

#endif
