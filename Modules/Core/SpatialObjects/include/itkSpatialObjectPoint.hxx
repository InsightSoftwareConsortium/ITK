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
#ifndef itkSpatialObjectPoint_hxx
#define itkSpatialObjectPoint_hxx

#include "itkSpatialObjectPoint.h"

namespace itk
{
/** Constructor */
template< unsigned int TPointDimension >
SpatialObjectPoint< TPointDimension >
::SpatialObjectPoint(void)
{
  m_ID = -1;
  m_Color.SetRed(1.0); // red by default
  m_Color.SetGreen(0);
  m_Color.SetBlue(0);
  m_Color.SetAlpha(1);
  for ( unsigned int i = 0; i < TPointDimension; i++ )
    {
    m_X[i] = 0;
    }
}

/** Destructor */
template< unsigned int TPointDimension >
SpatialObjectPoint< TPointDimension >
::~SpatialObjectPoint(void)
{}

/** Return the color of the point */
template< unsigned int TPointDimension >
const typename SpatialObjectPoint< TPointDimension >::PixelType &
SpatialObjectPoint< TPointDimension >
::GetColor(void) const
{
  return m_Color;
}

/** Set the color of the point */
template< unsigned int TPointDimension >
void
SpatialObjectPoint< TPointDimension >
::SetColor(const PixelType & color)
{
  m_Color = color;
}

/** Set the color of the point */
template< unsigned int TPointDimension >
void
SpatialObjectPoint< TPointDimension >
::SetColor(float r, float g, float b, float a)
{
  m_Color.SetRed(r);
  m_Color.SetGreen(g);
  m_Color.SetBlue(b);
  m_Color.SetAlpha(a);
}

/** Set the red channel of the point */
template< unsigned int TPointDimension >
void
SpatialObjectPoint< TPointDimension >
::SetRed(float r)
{
  m_Color.SetRed(r);
}

/** Return the red channel of the point */
template< unsigned int TPointDimension >
float
SpatialObjectPoint< TPointDimension >
::GetRed(void) const
{
  return m_Color.GetRed();
}

/** Set the green channel of the point */
template< unsigned int TPointDimension >
void
SpatialObjectPoint< TPointDimension >
::SetGreen(float g)
{
  m_Color.SetGreen(g);
}

/** Return the green channel of the point */
template< unsigned int TPointDimension >
float
SpatialObjectPoint< TPointDimension >
::GetGreen(void) const
{
  return m_Color.GetGreen();
}

/** Set the blue channel of the point */
template< unsigned int TPointDimension >
void
SpatialObjectPoint< TPointDimension >
::SetBlue(float b)
{
  m_Color.SetBlue(b);
}

/** Return the blue channel of the point */
template< unsigned int TPointDimension >
float
SpatialObjectPoint< TPointDimension >
::GetBlue(void) const
{
  return m_Color.GetBlue();
}

/** Set the alpha value of the point */
template< unsigned int TPointDimension >
void
SpatialObjectPoint< TPointDimension >
::SetAlpha(float a)
{
  m_Color.SetAlpha(a);
}

/** Return the alpha value of the point */
template< unsigned int TPointDimension >
float
SpatialObjectPoint< TPointDimension >
::GetAlpha(void) const
{
  return m_Color.GetAlpha();
}

/** Set the Identification number of a point */
template< unsigned int TPointDimension >
void
SpatialObjectPoint< TPointDimension >
::SetID(const int newID)
{
  m_ID = newID;
}

/** Get the Identification number of a point */
template< unsigned int TPointDimension >
int
SpatialObjectPoint< TPointDimension >
::GetID(void) const
{
  return m_ID;
}

/** Return the position of a point */
template< unsigned int TPointDimension >
const typename SpatialObjectPoint< TPointDimension >::PointType &
SpatialObjectPoint< TPointDimension >
::GetPosition(void) const
{
  return m_X;
}

/** Set the position : n-D case */
template< unsigned int TPointDimension >
void
SpatialObjectPoint< TPointDimension >
::SetPosition(const PointType & newX)
{
  m_X = newX;
}

/** Set the position : 3D case */
template< unsigned int TPointDimension >
void
SpatialObjectPoint< TPointDimension >
::SetPosition(const double x0, const double x1, const double x2)
{
  m_X[0] = x0;
  m_X[1] = x1;
  m_X[2] = x2;
}

/** Set the position : 2D case */
template< unsigned int TPointDimension >
void
SpatialObjectPoint< TPointDimension >
::SetPosition(const double x0, const double x1)
{
  m_X[0] = x0;
  m_X[1] = x1;
}

/** Copy a point to another point */
template< unsigned int TPointDimension >
typename SpatialObjectPoint< TPointDimension >::Self &
SpatialObjectPoint< TPointDimension >
::operator=(const SpatialObjectPoint & rhs)
{
  m_ID = rhs.m_ID;
  m_X = rhs.m_X;
  m_Color = rhs.m_Color;
  return *this;
}

/** PrintSelfMethod */
template< unsigned int TPointDimension >
void
SpatialObjectPoint< TPointDimension >
::Print(std::ostream & os) const
{
  this->PrintSelf(os, 3);
}

/** PrintSelfMethod */
template< unsigned int TPointDimension >
void
SpatialObjectPoint< TPointDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "RGBA: " << m_Color.GetRed() << " ";
  os << m_Color.GetGreen() << " ";
  os << m_Color.GetBlue() << " ";
  os << m_Color.GetAlpha() << std::endl;
  os << indent << "Position: ";
  for ( unsigned int i = 1; i < TPointDimension; i++ )
    {
    os << m_X[i - 1] << ",";
    }
  os <<  m_X[TPointDimension - 1] << std::endl;
}
} // end namespace itk

#endif
