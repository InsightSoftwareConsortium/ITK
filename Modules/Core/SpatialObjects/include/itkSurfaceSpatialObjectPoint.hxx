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
#ifndef itkSurfaceSpatialObjectPoint_hxx
#define itkSurfaceSpatialObjectPoint_hxx

#include "itkSurfaceSpatialObjectPoint.h"

namespace itk
{
/** Constructor */
template< unsigned int TPointDimension >
SurfaceSpatialObjectPoint< TPointDimension >
::SurfaceSpatialObjectPoint(void)
{
  this->m_ID = 0;
  m_Normal.Fill(0);
}

/** Destructor */
template< unsigned int TPointDimension >
SurfaceSpatialObjectPoint< TPointDimension >
::~SurfaceSpatialObjectPoint(void)
{}

/** Set the normal : N-D case */
template< unsigned int TPointDimension >
void
SurfaceSpatialObjectPoint< TPointDimension >
::SetNormal(const VectorType & normal)
{
  m_Normal = normal;
}

/** Set the normal : 2D case */
template< unsigned int TPointDimension >
void
SurfaceSpatialObjectPoint< TPointDimension >
::SetNormal(const double normalx, const double normaly)
{
  m_Normal[0] = normalx;
  m_Normal[1] = normaly;
}

/** Set the normal : 3D case */
template< unsigned int TPointDimension >
void
SurfaceSpatialObjectPoint< TPointDimension >
::SetNormal(const double normalx, const double normaly, const double normalz)
{
  m_Normal[0] = normalx;
  m_Normal[1] = normaly;
  m_Normal[2] = normalz;
}

/** Get the normal at one point */
template< unsigned int TPointDimension >
const typename SurfaceSpatialObjectPoint< TPointDimension >::VectorType &
SurfaceSpatialObjectPoint< TPointDimension >
::GetNormal(void) const
{
  return m_Normal;
}

/** Print the object */
template< unsigned int TPointDimension >
void
SurfaceSpatialObjectPoint< TPointDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "SurfaceSpatialObjectPoint(" << this << ")" << std::endl;
  os << indent << "Normal definition: ";
  os << indent <<  m_Normal << std::endl;
}

/** Copy a surface point to another */
template< unsigned int TPointDimension >
typename SurfaceSpatialObjectPoint< TPointDimension >::Self &
SurfaceSpatialObjectPoint< TPointDimension >
::operator=(const SurfaceSpatialObjectPoint & rhs)
{
  this->m_ID = rhs.m_ID;
  this->m_X = rhs.m_X;
  return *this;
}
} // end namespace itk

#endif
