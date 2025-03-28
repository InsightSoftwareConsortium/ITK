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
#ifndef itkTorusInteriorExteriorSpatialFunction_hxx
#define itkTorusInteriorExteriorSpatialFunction_hxx

#include "itkMath.h"


namespace itk
{
template <unsigned int VDimension, typename TInput>
auto
TorusInteriorExteriorSpatialFunction<VDimension, TInput>::Evaluate(const InputType & position) const -> OutputType
{
  const double x = position[0] - m_Origin[0];
  const double y = position[1] - m_Origin[1];
  const double z = position[2] - m_Origin[2];

  const double k = Math::sqr(m_MajorRadius - std::sqrt(x * x + y * y)) + z * z;

  if (k <= (m_MinorRadius * m_MinorRadius))
  {
    return true;
  }

  return false;
}

template <unsigned int VDimension, typename TInput>
void
TorusInteriorExteriorSpatialFunction<VDimension, TInput>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Origin: [";
  for (unsigned int i = 0; i < VDimension - 1; ++i)
  {
    os << m_Origin[i] << ", ";
  }
  os << ']' << std::endl;

  os << indent << "Major radius: " << m_MajorRadius << std::endl;

  os << indent << "Minor radius: " << m_MinorRadius << std::endl;
}
} // end namespace itk

#endif
