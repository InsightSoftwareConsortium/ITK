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
#ifndef itkSurfaceSpatialObjectPoint_hxx
#define itkSurfaceSpatialObjectPoint_hxx

#include "itkSurfaceSpatialObjectPoint.h"

namespace itk
{
/** Constructor */
template <unsigned int TPointDimension>
SurfaceSpatialObjectPoint<TPointDimension>::SurfaceSpatialObjectPoint()
{
  m_NormalInObjectSpace.Fill(0);
}

/** Set the normal : N-D case */
template <unsigned int TPointDimension>
void
SurfaceSpatialObjectPoint<TPointDimension>::SetNormalInObjectSpace(const CovariantVectorType & normal)
{
  m_NormalInObjectSpace = normal;
}

/** Get the normal at one point */
template <unsigned int TPointDimension>
const typename SurfaceSpatialObjectPoint<TPointDimension>::CovariantVectorType &
SurfaceSpatialObjectPoint<TPointDimension>::GetNormalInObjectSpace() const
{
  return m_NormalInObjectSpace;
}

/** Print the object */
template <unsigned int TPointDimension>
void
SurfaceSpatialObjectPoint<TPointDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "SurfaceSpatialObjectPoint(" << this << ")" << std::endl;
  os << indent << "Normal definition: ";
  os << indent << m_NormalInObjectSpace << std::endl;
}

/** Copy a surface point to another */
template <unsigned int TPointDimension>
typename SurfaceSpatialObjectPoint<TPointDimension>::Self &
SurfaceSpatialObjectPoint<TPointDimension>::operator=(const SurfaceSpatialObjectPoint & rhs)
{
  this->m_Id = rhs.m_Id;
  this->m_Color = rhs.m_Color;
  this->m_SpatialObject = rhs.m_SpatialObject;
  this->m_PositionInObjectSpace = rhs.m_PositionInObjectSpace;
  this->m_NormalInObjectSpace = rhs.m_NormalInObjectSpace;
  return *this;
}
} // end namespace itk

#endif
