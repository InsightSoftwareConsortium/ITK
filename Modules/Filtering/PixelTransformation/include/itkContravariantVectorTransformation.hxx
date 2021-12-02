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
#ifndef itkContravariantVectorTransformation_hxx
#define itkContravariantVectorTransformation_hxx

#include "itkContravariantVectorTransformation.h"
namespace itk
{

template <typename TPixelType, typename TTransformType, typename TOutputPointType>
ContravariantVectorTransformation<TPixelType, TTransformType, TOutputPointType>::ContravariantVectorTransformation()
{
  // initialize variables
}


template <typename TPixelType, typename TTransformType, typename TOutputPointType>
auto
ContravariantVectorTransformation<TPixelType, TTransformType, TOutputPointType>::Transform(
  const PixelType &       value,
  const InputPointType &  inputPoint,
  const OutputPointType & outputPoint) -> PixelType
{
  TransformType * inverseTransform = this->m_ImageTransform->GetInverseTransform();
  if (inverseTransform)
  {
    return inverseTransform->TransformVector(value, outputPoint);
  }
  else
  {
    typename TransformType::InverseJacobianPositionType jacobian;
    this->m_ImageTransform->ComputeInverseJacobianWithRespectToPosition(inputPoint, jacobian);
    PixelType result;
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      result[i] = NumericTraits<typename PixelType::ValueType>::ZeroValue();
      for (unsigned int j = 0; j < Dimension; ++j)
      {
        result[i] += jacobian[i][j] * value[j];
      }
    }
    return result;
  }
}

template <typename TPixelType, typename TTransformType, typename TOutputPointType>
void
ContravariantVectorTransformation<TPixelType, TTransformType, TOutputPointType>::PrintSelf(std::ostream & os,
                                                                                           Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
