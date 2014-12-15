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
#ifndef itkVesselTubeSpatialObjectPoint_hxx
#define itkVesselTubeSpatialObjectPoint_hxx

#include "itkVesselTubeSpatialObjectPoint.h"

namespace itk
{
/** Constructor */
template< unsigned int TPointDimension >
VesselTubeSpatialObjectPoint< TPointDimension >
::VesselTubeSpatialObjectPoint(void):Superclass()
{
  m_Medialness = 0;
  m_Ridgeness = 0;
  m_Branchness = 0;
  m_Mark = false;
  m_Alpha1 = 0;
  m_Alpha2 = 0;
  m_Alpha3 = 0;
}

/** Destructor */
template< unsigned int TPointDimension >
VesselTubeSpatialObjectPoint< TPointDimension >
::~VesselTubeSpatialObjectPoint(void)
{}

template< unsigned int TPointDimension >
float
VesselTubeSpatialObjectPoint< TPointDimension >
::GetMedialness(void) const
{
  return m_Medialness;
}

template< unsigned int TPointDimension >
void
VesselTubeSpatialObjectPoint< TPointDimension >
::SetMedialness(const float newMedialness)
{
  m_Medialness = newMedialness;
}

template< unsigned int TPointDimension >
float
VesselTubeSpatialObjectPoint< TPointDimension >
::GetRidgeness(void) const
{
  return m_Ridgeness;
}

template< unsigned int TPointDimension >
void
VesselTubeSpatialObjectPoint< TPointDimension >
::SetRidgeness(const float newRidgeness)
{
  m_Ridgeness = newRidgeness;
}

template< unsigned int TPointDimension >
float
VesselTubeSpatialObjectPoint< TPointDimension >
::GetBranchness(void) const
{
  return m_Branchness;
}

template< unsigned int TPointDimension >
void
VesselTubeSpatialObjectPoint< TPointDimension >
::SetBranchness(const float newBranchness)
{
  m_Branchness = newBranchness;
}

template< unsigned int TPointDimension >
bool
VesselTubeSpatialObjectPoint< TPointDimension >
::GetMark(void) const
{
  return m_Mark;
}

template< unsigned int TPointDimension >
void
VesselTubeSpatialObjectPoint< TPointDimension >
::SetMark(const bool newMark)
{
  m_Mark = newMark;
}

template< unsigned int TPointDimension >
float
VesselTubeSpatialObjectPoint< TPointDimension >
::GetAlpha1(void) const
{
  return m_Alpha1;
}

template< unsigned int TPointDimension >
void
VesselTubeSpatialObjectPoint< TPointDimension >
::SetAlpha1(const float newAlpha)
{
  m_Alpha1 = newAlpha;
}

template< unsigned int TPointDimension >
float
VesselTubeSpatialObjectPoint< TPointDimension >
::GetAlpha2(void) const
{
  return m_Alpha2;
}

template< unsigned int TPointDimension >
void
VesselTubeSpatialObjectPoint< TPointDimension >
::SetAlpha2(const float newAlpha)
{
  m_Alpha2 = newAlpha;
}

template< unsigned int TPointDimension >
float
VesselTubeSpatialObjectPoint< TPointDimension >
::GetAlpha3(void) const
{
  return m_Alpha3;
}

template< unsigned int TPointDimension >
void
VesselTubeSpatialObjectPoint< TPointDimension >
::SetAlpha3(const float newAlpha)
{
  m_Alpha3 = newAlpha;
}

template< unsigned int TPointDimension >
void
VesselTubeSpatialObjectPoint< TPointDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Medialness: " << m_Medialness << std::endl;
  os << indent << "Ridgeness: " << m_Ridgeness << std::endl;
  os << indent << "Alpha1: " << m_Alpha1 << std::endl;
  os << indent << "Alpha2: " << m_Alpha2 << std::endl;
  os << indent << "Alpha3: " << m_Alpha3 << std::endl;
  os << indent << "Mark: " << m_Mark << std::endl;
}

template< unsigned int TPointDimension >
typename VesselTubeSpatialObjectPoint< TPointDimension >::Self &
VesselTubeSpatialObjectPoint< TPointDimension >
::operator=(const VesselTubeSpatialObjectPoint & rhs)
{
  if(this != &rhs)
    {
    this->m_ID = rhs.m_ID;
    this->m_R = rhs.m_R;
    m_Medialness = rhs.m_Medialness;
    m_Ridgeness = rhs.m_Ridgeness;
    m_Branchness = rhs.m_Branchness;
    m_Mark = rhs.m_Mark;
    this->m_NumDimensions = rhs.m_NumDimensions;
    this->m_X = rhs.m_X;
    this->m_T = rhs.m_T;
    this->m_Normal1 = rhs.m_Normal1;
    this->m_Normal2 = rhs.m_Normal2;
    m_Alpha1 = rhs.m_Alpha1;
    m_Alpha2 = rhs.m_Alpha2;
    m_Alpha3 = rhs.m_Alpha3;
    this->m_Color = rhs.m_Color;
    }
  return *this;
}
} // end namespace itk

#endif
