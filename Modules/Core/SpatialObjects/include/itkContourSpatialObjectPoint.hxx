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
#ifndef __itkContourSpatialObjectPoint_hxx
#define __itkContourSpatialObjectPoint_hxx

#include "itkContourSpatialObjectPoint.h"

namespace itk
{
/** Constructor */
template< unsigned int TPointDimension >
ContourSpatialObjectPoint< TPointDimension >
::ContourSpatialObjectPoint(void)
{
  this->m_ID = 0;
  m_Normal.Fill(0);
  m_PickedPoint.Fill(0);
}

/** Destructor */
template< unsigned int TPointDimension >
ContourSpatialObjectPoint< TPointDimension >
::~ContourSpatialObjectPoint(void)
{}

/** Set the picked point : N-D case */
template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::SetPickedPoint(const PointType & point)
{
  m_PickedPoint = point;
}

/** Set the picked point : 2D case */
template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::SetPickedPoint(const double pointx, const double pointy)
{
  m_PickedPoint[0] = pointx;
  m_PickedPoint[1] = pointy;
}

/** Set the picked point : 3D case */
template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::SetPickedPoint(const double pointx, const double pointy, const double pointz)
{
  m_PickedPoint[0] = pointx;
  m_PickedPoint[1] = pointy;
  m_PickedPoint[2] = pointz;
}

/** Get the normal at one point */
template< unsigned int TPointDimension >
const typename ContourSpatialObjectPoint< TPointDimension >::PointType &
ContourSpatialObjectPoint< TPointDimension >
::GetPickedPoint(void) const
{
  return m_PickedPoint;
}

/** Set the normal : N-D case */
template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::SetNormal(const VectorType & normal)
{
  m_Normal = normal;
}

/** Set the normal : 2D case */
template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::SetNormal(const double normalx, const double normaly)
{
  m_Normal[0] = normalx;
  m_Normal[1] = normaly;
}

/** Set the normal : 3D case */
template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::SetNormal(const double normalx, const double normaly, const double normalz)
{
  m_Normal[0] = normalx;
  m_Normal[1] = normaly;
  m_Normal[2] = normalz;
}

/** Get the normal at one point */
template< unsigned int TPointDimension >
const typename ContourSpatialObjectPoint< TPointDimension >::VectorType &
ContourSpatialObjectPoint< TPointDimension >
::GetNormal(void) const
{
  return m_Normal;
}

/** Print the object */
template< unsigned int TPointDimension >
void
ContourSpatialObjectPoint< TPointDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ContourSpatialObjectPoint(" << this << ")" << std::endl;
  os << indent << "Picked Point: ";
  os << indent <<  m_PickedPoint << std::endl;
  os << indent << "Normal: ";
  os << indent <<  m_Normal << std::endl;
}

/** Copy a surface point to another */
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
} // end namespace itk

#endif
