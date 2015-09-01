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

=========================================================================*/

#ifndef __itkSpeedFunctionPathInformation_hxx
#define __itkSpeedFunctionPathInformation_hxx

#include "itkSpeedFunctionPathInformation.h"

namespace itk
{

template <class TPoint>
SpeedFunctionPathInformation<TPoint>::SpeedFunctionPathInformation()
{
  this->ClearInfo();
}

template <class TPoint>
SpeedFunctionPathInformation<TPoint>::~SpeedFunctionPathInformation()
{}

template <class TPoint>
void
SpeedFunctionPathInformation<TPoint>::ClearInfo()
{
  m_Info.clear();
  m_Info.resize(2);
  m_Front = 1;
}

template <class TPoint>
void
SpeedFunctionPathInformation<TPoint>::SetStartPoint(const PointType & start)
{
  m_Info[1] = start;
}

template <class TPoint>
void
SpeedFunctionPathInformation<TPoint>::SetEndPoint(const PointType & end)
{
  m_Info[0] = end;
}

template <class TPoint>
void
SpeedFunctionPathInformation<TPoint>::AddWayPoint(const PointType & way)
{
  m_Info.push_back(way);
  m_Front++;
}

template <class TPoint>
unsigned int
SpeedFunctionPathInformation<TPoint>::GetNumberOfPoints() const
{
  return m_Info.size();
}

template <class TPoint>
const typename SpeedFunctionPathInformation<TPoint>::PointType &
SpeedFunctionPathInformation<TPoint>::GetStartPoint() const
{
  return m_Info[1];
}

template <class TPoint>
const typename SpeedFunctionPathInformation<TPoint>::PointType &
SpeedFunctionPathInformation<TPoint>::GetEndPoint() const
{
  return m_Info[0];
}

template <class TPoint>
const typename SpeedFunctionPathInformation<TPoint>::PointType &
SpeedFunctionPathInformation<TPoint>::GetWayPoint(unsigned int i) const
{
  return m_Info[2 + i];
}

template <class TPoint>
bool
SpeedFunctionPathInformation<TPoint>::HasNextFront() const
{
  return m_Front >= 1;
}

template <class TPoint>
const typename SpeedFunctionPathInformation<TPoint>::PointType &
SpeedFunctionPathInformation<TPoint>::GetCurrentFrontAndAdvance()
{
  return m_Info[m_Front--];
}

template <class TPoint>
const typename SpeedFunctionPathInformation<TPoint>::PointType &
SpeedFunctionPathInformation<TPoint>::PeekCurrentFront() const
{
  return m_Info[m_Front];
}

template <class TPoint>
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

template <class TPoint>
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

/**
 *
 */
template <class TPoint>
void
SpeedFunctionPathInformation<TPoint>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
