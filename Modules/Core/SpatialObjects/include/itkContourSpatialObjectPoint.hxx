/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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


namespace itk
{

template <unsigned int TPointDimension>
ContourSpatialObjectPoint<TPointDimension>::ContourSpatialObjectPoint()
{
  m_NormalInObjectSpace.Fill(0);
  m_PickedPointInObjectSpace.Fill(0);
}

template <unsigned int TPointDimension>
ContourSpatialObjectPoint<TPointDimension>::ContourSpatialObjectPoint(const ContourSpatialObjectPoint & other)
  : Superclass(other)
{
  this->m_NormalInObjectSpace = other.GetNormalInObjectSpace();
  this->m_PickedPointInObjectSpace = other.GetPickedPointInObjectSpace();
}

template <unsigned int TPointDimension>
void
ContourSpatialObjectPoint<TPointDimension>::SetPickedPointInObjectSpace(const PointType & point)
{
  m_PickedPointInObjectSpace = point;
}

template <unsigned int TPointDimension>
auto
ContourSpatialObjectPoint<TPointDimension>::GetPickedPointInObjectSpace() const -> const PointType &
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
auto
ContourSpatialObjectPoint<TPointDimension>::GetNormalInObjectSpace() const -> const CovariantVectorType &
{
  return m_NormalInObjectSpace;
}

template <unsigned int TPointDimension>
auto
ContourSpatialObjectPoint<TPointDimension>::operator=(const ContourSpatialObjectPoint & rhs) -> Self &
{
  if (this != &rhs)
  {
    Superclass::operator=(rhs);
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

  os << indent << "PickedPointInObjectSpace: " << m_PickedPointInObjectSpace << std::endl;
  os << indent
     << "NormalInObjectSpace: " << static_cast<typename NumericTraits<PointType>::PrintType>(m_NormalInObjectSpace)
     << std::endl;
}
} // end namespace itk

#endif
