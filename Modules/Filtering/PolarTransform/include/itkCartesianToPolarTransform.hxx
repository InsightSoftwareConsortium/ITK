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
#ifndef itkCartesianToPolarTransform_hxx
#define itkCartesianToPolarTransform_hxx

#include "itkCartesianToPolarTransform.h"
#include "itkMath.h"

namespace itk
{

template <typename TParametersValueType, unsigned int NDimensions>
CartesianToPolarTransform<TParametersValueType, NDimensions>::CartesianToPolarTransform()
{
  this->m_Center.Fill(0.0);
}


template <typename TParametersValueType, unsigned int NDimensions>
CartesianToPolarTransform<TParametersValueType, NDimensions>::~CartesianToPolarTransform()
{}


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

  const InputPointType vector = inputPoint - this->m_Center;

  outputPoint[1] = std::sqrt(vector[0] * vector[0] + vector[1] * vector[1]);
  outputPoint[0] = std::acos(vector[0] / outputPoint[1]);
  if (vector[1] < 0.0)
  {
    outputPoint[0] = Math::twopi - outputPoint[0];
  }

  return outputPoint;
}

} // namespace itk

#endif
