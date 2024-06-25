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
#ifndef itkLineSpatialObjectPoint_hxx
#define itkLineSpatialObjectPoint_hxx


namespace itk
{

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

template <unsigned int TPointDimension>
auto
LineSpatialObjectPoint<TPointDimension>::GetNormalInObjectSpace(unsigned int index) const -> const CovariantVectorType &
{
  return m_NormalArrayInObjectSpace[index];
}

template <unsigned int TPointDimension>
void
LineSpatialObjectPoint<TPointDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NormalArrayInObjectSpace: " << m_NormalArrayInObjectSpace << std::endl;
}

/** Set the specified normal */
template <unsigned int TPointDimension>
void
LineSpatialObjectPoint<TPointDimension>::SetNormalInObjectSpace(CovariantVectorType & normal, unsigned int index)
{
  m_NormalArrayInObjectSpace[index] = normal;
}

} // end namespace itk

#endif
