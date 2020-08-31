/*=========================================================================
 *
 *  Copyright NumFOCUS
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
template <unsigned int TPointDimension>
LineSpatialObjectPoint<TPointDimension>::LineSpatialObjectPoint()
{
  unsigned int        ii = 0;
  CovariantVectorType normal;
  normal.Fill(0);
  while (ii < TPointDimension - 1)
  {
    this->m_NormalArrayInObjectSpace[ii] = normal;
    ++ii;
  }
}

/** Copy Constructor */
template <unsigned int TPointDimension>
LineSpatialObjectPoint<TPointDimension>::LineSpatialObjectPoint(const LineSpatialObjectPoint & other)
  : Superclass(other)
{
  this->m_NormalArrayInObjectSpace = other.m_NormalArrayInObjectSpace;
}

/** Get the specified normal */
template <unsigned int TPointDimension>
const typename LineSpatialObjectPoint<TPointDimension>::CovariantVectorType &
LineSpatialObjectPoint<TPointDimension>::GetNormalInObjectSpace(unsigned int index) const
{
  return m_NormalArrayInObjectSpace[index];
}

/** Print the object */
template <unsigned int TPointDimension>
void
LineSpatialObjectPoint<TPointDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "LineSpatialObjectPoint(" << this << ")" << std::endl;
  unsigned int ii = 0;
  while (ii < TPointDimension - 1)
  {
    os << indent << this->m_NormalArrayInObjectSpace[ii] << std::endl;
    ++ii;
  }
}

/** Set the specified normal */
template <unsigned int TPointDimension>
void
LineSpatialObjectPoint<TPointDimension>::SetNormalInObjectSpace(CovariantVectorType & normal, unsigned int index)
{
  m_NormalArrayInObjectSpace[index] = normal;
}

/** Copy a point to another */
template <unsigned int TPointDimension>
typename LineSpatialObjectPoint<TPointDimension>::Self &
LineSpatialObjectPoint<TPointDimension>::operator=(const LineSpatialObjectPoint & rhs)
{
  if (this != &rhs)
  {
    Superclass::operator=(rhs);
    this->m_NormalArrayInObjectSpace = rhs.m_NormalArrayInObjectSpace;
  }
  return *this;
}

} // end namespace itk

#endif
