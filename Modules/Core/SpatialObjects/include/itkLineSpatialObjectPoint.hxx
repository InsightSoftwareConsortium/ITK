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
#ifndef itkLineSpatialObjectPoint_hxx
#define itkLineSpatialObjectPoint_hxx

#include "itkLineSpatialObjectPoint.h"

namespace itk
{
/** Constructor */
template< unsigned int TPointDimension >
LineSpatialObjectPoint< TPointDimension >
::LineSpatialObjectPoint(void)
{
  this->m_ID = 0;
  unsigned int ii = 0;
  VectorType normal;
  normal.Fill(0);
  while( ii < TPointDimension - 1 )
    {
    this->m_NormalArray[ii] = normal;
    ++ii;
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
  unsigned int ii = 0;
  while( ii < TPointDimension - 1 )
    {
    os << indent << this->m_NormalArray[ii] << std::endl;
    ++ii;
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
