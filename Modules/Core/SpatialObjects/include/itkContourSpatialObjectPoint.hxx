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
#ifndef itkContourSpatialObjectPoint_hxx
#define itkContourSpatialObjectPoint_hxx

#include "itkContourSpatialObjectPoint.h"

namespace itk
{
template <unsigned int TPointDimension>
ContourSpatialObjectPoint<TPointDimension>::ContourSpatialObjectPoint()
{
  m_NormalInObjectSpace.Fill(0);
  m_PickedPointInObjectSpace.Fill(0);
}

template <unsigned int TPointDimension>
void
ContourSpatialObjectPoint<TPointDimension>::SetPickedPointInObjectSpace(const PointType & point)
{
  m_PickedPointInObjectSpace = point;
}

template <unsigned int TPointDimension>
const typename ContourSpatialObjectPoint<TPointDimension>::PointType &
ContourSpatialObjectPoint<TPointDimension>::GetPickedPointInObjectSpace() const
{
  return m_PickedPointInObjectSpace;
}

template <unsigned int TPointDimension>
void
ContourSpatialObjectPoint<TPointDimension>::SetNormalInObjectSpace(const CovariantVectorType & normal)
{
  m_NormalInObjectSpace = normal;
}

template <unsigned int TPointDimension>
const typename ContourSpatialObjectPoint<TPointDimension>::CovariantVectorType &
ContourSpatialObjectPoint<TPointDimension>::GetNormalInObjectSpace() const
{
  return m_NormalInObjectSpace;
}

template <unsigned int TPointDimension>
typename ContourSpatialObjectPoint<TPointDimension>::Self &
ContourSpatialObjectPoint<TPointDimension>::operator=(const ContourSpatialObjectPoint & rhs)
{
  if (this != &rhs)
  {
    this->m_Id = rhs.GetId();
    this->m_PositionInObjectSpace = rhs.GetPositionInObjectSpace();
    this->m_Color = rhs.GetColor();
    this->m_SpatialObject = rhs.GetSpatialObject();
    this->m_NormalInObjectSpace = rhs.GetNormalInObjectSpace();
    this->m_PickedPointInObjectSpace = rhs.GetPickedPointInObjectSpace();
  }
  return *this;
}

template <unsigned int TPointDimension>
void
ContourSpatialObjectPoint<TPointDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Picked PointInObjectSpace: " << m_PickedPointInObjectSpace << std::endl;
  os << indent << "NormalInObjectSpace: " << m_NormalInObjectSpace << std::endl;
}
} // end namespace itk

#endif
