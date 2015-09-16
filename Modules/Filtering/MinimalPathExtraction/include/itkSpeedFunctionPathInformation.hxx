/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkSpeedFunctionPathInformation.hxx,v $
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  This software is distributed WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the above copyright notices for more information.

  Copyright Insight Software Consortium

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

         http://www.apache.org/licenses/LICENSE-2.0.txt

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

=========================================================================*/

#ifndef itkSpeedFunctionPathInformation_hxx
#define itkSpeedFunctionPathInformation_hxx

#include "itkSpeedFunctionPathInformation.h"

namespace itk
{

template <typename TPoint>
SpeedFunctionPathInformation<TPoint>::SpeedFunctionPathInformation()
{
  this->ClearInfo();
}


template <typename TPoint>
SpeedFunctionPathInformation<TPoint>::~SpeedFunctionPathInformation()
{}


template <typename TPoint>
void
SpeedFunctionPathInformation<TPoint>::ClearInfo()
{
  m_Info.clear();
  m_Info.resize(2);
  m_Front = 1;
}


template <typename TPoint>
void
SpeedFunctionPathInformation<TPoint>::SetStartPoint(const PointType & start)
{
  m_Info[1] = start;
}


template <typename TPoint>
void
SpeedFunctionPathInformation<TPoint>::SetEndPoint(const PointType & end)
{
  m_Info[0] = end;
}


template <typename TPoint>
void
SpeedFunctionPathInformation<TPoint>::AddWayPoint(const PointType & way)
{
  m_Info.push_back(way);
  m_Front++;
}


template <typename TPoint>
unsigned int
SpeedFunctionPathInformation<TPoint>::GetNumberOfPoints() const
{
  return m_Info.size();
}


template <typename TPoint>
const typename SpeedFunctionPathInformation<TPoint>::PointType &
SpeedFunctionPathInformation<TPoint>::GetStartPoint() const
{
  return m_Info[1];
}


template <typename TPoint>
const typename SpeedFunctionPathInformation<TPoint>::PointType &
SpeedFunctionPathInformation<TPoint>::GetEndPoint() const
{
  return m_Info[0];
}


template <typename TPoint>
const typename SpeedFunctionPathInformation<TPoint>::PointType &
SpeedFunctionPathInformation<TPoint>::GetWayPoint(SizeValueType i) const
{
  return m_Info[2 + i];
}


template <typename TPoint>
bool
SpeedFunctionPathInformation<TPoint>::HasNextFront() const
{
  return m_Front >= 1;
}


template <typename TPoint>
const typename SpeedFunctionPathInformation<TPoint>::PointType &
SpeedFunctionPathInformation<TPoint>::GetCurrentFrontAndAdvance()
{
  return m_Info[m_Front--];
}


template <typename TPoint>
const typename SpeedFunctionPathInformation<TPoint>::PointType &
SpeedFunctionPathInformation<TPoint>::PeekCurrentFront() const
{
  return m_Info[m_Front];
}


template <typename TPoint>
const typename SpeedFunctionPathInformation<TPoint>::PointType &
SpeedFunctionPathInformation<TPoint>::PeekNextFront() const
{
  if (m_Front <= 1)
  {
    return m_Info[1];
  }
  else
  {
    return m_Info[m_Front - 1];
  }
}


template <typename TPoint>
const typename SpeedFunctionPathInformation<TPoint>::PointType &
SpeedFunctionPathInformation<TPoint>::PeekPreviousFront() const
{
  if (m_Front == m_Info.size() - 1)
  {
    return m_Info[0];
  }
  else
  {
    return m_Info[m_Front + 1];
  }
}


template <typename TPoint>
void
SpeedFunctionPathInformation<TPoint>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
