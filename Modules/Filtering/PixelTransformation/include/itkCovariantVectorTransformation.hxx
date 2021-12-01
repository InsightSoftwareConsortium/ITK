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
#ifndef itkCovariantVectorTransformation_hxx
#define itkCovariantVectorTransformation_hxx

#include "itkCovariantVectorTransformation.h"
namespace itk
{
/*
template <typename TPixelType,
          typename TTransformType,
          typename TOutputPointType>
CovariantVectorTransformation<TPixelType, TTransformType, TOutputPointType>::
  CovariantVectorTransformation()
{
  // initialize variables
}
*/

template <typename TPixelType, typename TTransformType, typename TOutputPointType>
auto
CovariantVectorTransformation<TPixelType, TTransformType, TOutputPointType>::Transform(
  const PixelType &       value,
  const InputPointType &  inputPoint,
  const OutputPointType & outputPoint) -> PixelType
{
  typename TransformType::JacobianPositionType jacobian;
  this->m_ImageTransform->ComputeJacobianWithRespectToPosition(inputPoint, jacobian);
  PixelType result;

  for (unsigned int i = 0; i < Dimension; ++i)
  {
    result[i] = NumericTraits<typename PixelType::ValueType>::ZeroValue();
    for (unsigned int j = 0; j < Dimension; ++j)
    {
      result[i] += jacobian[j][i] * value[j];
    }
  }
  return result;
}

template <typename TPixelType, typename TTransformType, typename TOutputPointType>
void
CovariantVectorTransformation<TPixelType, TTransformType, TOutputPointType>::PrintSelf(std::ostream & os,
                                                                                       Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
