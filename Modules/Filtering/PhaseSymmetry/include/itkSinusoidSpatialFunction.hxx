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
#ifndef itkSinusoidSpatialFunction_hxx
#define itkSinusoidSpatialFunction_hxx

#include <cmath>
#include "vnl/vnl_math.h"

namespace itk
{
template <typename TOutput, unsigned int VImageDimension, typename TInput>
SinusoidSpatialFunction<TOutput, VImageDimension, TInput>::SinusoidSpatialFunction()

{
  m_Frequency.Fill(1.0);
}


template <typename TOutput, unsigned int VImageDimension, typename TInput>
SinusoidSpatialFunction<TOutput, VImageDimension, TInput>::~SinusoidSpatialFunction() = default;


template <typename TOutput, unsigned int VImageDimension, typename TInput>
typename SinusoidSpatialFunction<TOutput, VImageDimension, TInput>::OutputType
SinusoidSpatialFunction<TOutput, VImageDimension, TInput>::Evaluate(const TInput & position) const
{

  double frequencyTerm = 0.0;
  for (unsigned int ii = 0; ii < VImageDimension; ++ii)
  {
    frequencyTerm += this->m_Frequency[ii] * position[ii];
  }
  frequencyTerm *= 2.0 * vnl_math::pi;
  frequencyTerm += this->m_PhaseOffset;
  const double value = std::cos(frequencyTerm);
  return static_cast<TOutput>(value);

  return (TOutput)value;
}


template <typename TOutput, unsigned int VImageDimension, typename TInput>
void
SinusoidSpatialFunction<TOutput, VImageDimension, TInput>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Sinusoid frequency: [";
  for (unsigned int ii = 0; ii < VImageDimension; ++ii)
  {
    os << this->m_Frequency[ii];
    if (ii != VImageDimension - 1)
    {
      os << ", ";
    }
  }
  os << "]" << std::endl;
}

} // end namespace itk

#endif
