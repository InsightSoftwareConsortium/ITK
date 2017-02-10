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
#ifndef itkContourSpatialObjectPoint_hxx
#define itkContourSpatialObjectPoint_hxx

#include "itkContourSpatialObjectPoint.h"

namespace itk
{
template< unsigned int TPointDimension >
ContourSpatialObjectPoint< TPointDimension >
::ContourSpatialObjectPoint(void)
{
  this->m_ID = 0;
  m_Normal.Fill(0);
  m_PickedPoint.Fill(0);
}

template< unsigned int TPointDimension >
ContourSpatialObjectPoint< TPointDimension >
::~ContourSpatialObjectPoint(void)
{}

template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::SetPickedPoint(const PointType & point)
{
  m_PickedPoint = point;
}

template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::SetPickedPoint(const double pointx, const double pointy)
{
  m_PickedPoint[0] = pointx;
  m_PickedPoint[1] = pointy;
}

template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::SetPickedPoint(const double pointx, const double pointy, const double pointz)
{
  m_PickedPoint[0] = pointx;
  m_PickedPoint[1] = pointy;
  m_PickedPoint[2] = pointz;
}

template< unsigned int TPointDimension >
const typename ContourSpatialObjectPoint< TPointDimension >::PointType &
ContourSpatialObjectPoint< TPointDimension >
::GetPickedPoint(void) const
{
  return m_PickedPoint;
}

template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::SetNormal(const VectorType & normal)
{
  m_Normal = normal;
}

template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::SetNormal(const double normalx, const double normaly)
{
  m_Normal[0] = normalx;
  m_Normal[1] = normaly;
}

template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::SetNormal(const double normalx, const double normaly, const double normalz)
{
  m_Normal[0] = normalx;
  m_Normal[1] = normaly;
  m_Normal[2] = normalz;
}

template< unsigned int TPointDimension >
const typename ContourSpatialObjectPoint< TPointDimension >::VectorType &
ContourSpatialObjectPoint< TPointDimension >
::GetNormal(void) const
{
  return m_Normal;
}

template< unsigned int TPointDimension >
typename ContourSpatialObjectPoint< TPointDimension >::Self &
ContourSpatialObjectPoint< TPointDimension >
::operator=(const ContourSpatialObjectPoint & rhs)
{
  if(this != &rhs)
    {
    this->m_ID = rhs.GetID();
    this->m_X = rhs.GetPosition();
    this->m_Normal = rhs.GetNormal();
    this->m_PickedPoint = rhs.GetPickedPoint();
    }
  return *this;
}

template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Picked Point: " <<  m_PickedPoint << std::endl;
  os << indent << "Normal: " <<  m_Normal << std::endl;
}
} // end namespace itk

#endif
