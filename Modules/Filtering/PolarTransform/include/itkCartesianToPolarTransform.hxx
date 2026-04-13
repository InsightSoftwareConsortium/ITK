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
#ifndef itkCartesianToPolarTransform_hxx
#define itkCartesianToPolarTransform_hxx

#include "itkMath.h"

namespace itk
{

template <typename TParametersValueType, unsigned int NDimensions>
CartesianToPolarTransform<TParametersValueType, NDimensions>::CartesianToPolarTransform()
  : Superclass(ParametersDimension)
{
  this->m_Center.Fill(0.0);
}


template <typename TParametersValueType, unsigned int NDimensions>
CartesianToPolarTransform<TParametersValueType, NDimensions>::~CartesianToPolarTransform() = default;


template <typename TParametersValueType, unsigned int NDimensions>
void
CartesianToPolarTransform<TParametersValueType, NDimensions>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Center: " << m_Center << std::endl;
}


template <typename TParametersValueType, unsigned int NDimensions>
typename CartesianToPolarTransform<TParametersValueType, NDimensions>::OutputPointType
CartesianToPolarTransform<TParametersValueType, NDimensions>::TransformPoint(const InputPointType & inputPoint) const
{
  OutputPointType outputPoint(inputPoint);

  const InputVectorType vector = inputPoint - this->m_Center;

  outputPoint[1] = std::sqrt(vector[0] * vector[0] + vector[1] * vector[1]); // r= sqrt(x^2 + y^2)
  outputPoint[0] = std::acos(vector[0] / outputPoint[1]);                    // alpha = acos(x/r)
  outputPoint[0] += m_AngleOffset; // add offset before 2*pi adjustment to keep values within [-pi,pi]
  if (vector[1] < 0.0)
  {
    outputPoint[0] = Math::twopi - outputPoint[0];
  }

  if (m_ConstArcIncr)
  {
    outputPoint[0] *= outputPoint[1]; // arc= r*alpha
  }

  return outputPoint;
}

} // namespace itk

#endif
