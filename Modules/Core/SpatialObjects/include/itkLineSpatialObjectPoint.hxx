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
#ifndef __itkLineSpatialObjectPoint_hxx
#define __itkLineSpatialObjectPoint_hxx

#include "itkLineSpatialObjectPoint.h"

namespace itk
{
/** Constructor */
template< unsigned int TPointDimension >
LineSpatialObjectPoint< TPointDimension >
::LineSpatialObjectPoint(void)
{
  this->m_ID = 0;
  for ( unsigned int i = 0; i < TPointDimension - 1; i++ )
    {
    VectorType normal;
    normal.Fill(0);
    m_NormalArray[i] = normal;
    }
}

/** Destructor */
template< unsigned int TPointDimension >
LineSpatialObjectPoint< TPointDimension >
::~LineSpatialObjectPoint(void)
{}

/** Get the specified normal */
template< unsigned int TPointDimension >
const typename LineSpatialObjectPoint< TPointDimension >::VectorType &
LineSpatialObjectPoint< TPointDimension >
::GetNormal(unsigned int index) const
{
  return m_NormalArray[index];
}

/** Print the object */
template< unsigned int TPointDimension >
void
LineSpatialObjectPoint< TPointDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "LineSpatialObjectPoint(" << this << ")" << std::endl;
  for ( unsigned int i = 0; i < TPointDimension - 1; i++ )
    {
    os << indent <<   m_NormalArray[i] << std::endl;
    }
}

/** Set the specified normal */
template< unsigned int TPointDimension >
void
LineSpatialObjectPoint< TPointDimension >
::SetNormal(VectorType & normal, unsigned int index)
{
  m_NormalArray[index] = normal;
}

/** Copy a point to another */
template< unsigned int TPointDimension >
typename LineSpatialObjectPoint< TPointDimension >::Self &
LineSpatialObjectPoint< TPointDimension >
::operator=(const LineSpatialObjectPoint & rhs)
{
  this->m_ID = rhs.m_ID;
  this->m_X = rhs.m_X;
  return *this;
}
} // end namespace itk

#endif
