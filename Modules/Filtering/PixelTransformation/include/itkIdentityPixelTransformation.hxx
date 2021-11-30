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
#ifndef itkIdentityPixelTransformation_hxx
#define itkIdentityPixelTransformation_hxx

#include "itkIdentityPixelTransformation.h"
namespace itk
{
/*
template <typename TPixelType,
          typename TTransformType,
          typename TOutputPointType>
IdentityPixelTransformation<TPixelType, TTransformType, TOutputPointType>::
  IdentityPixelTransformation()
{
  // initialize variables
}
*/

template <typename TPixelType, typename TTransformType, typename TOutputPointType>
typename IdentityPixelTransformation<TPixelType, TTransformType, TOutputPointType>::PixelType &
IdentityPixelTransformation<TPixelType, TTransformType, TOutputPointType>::Transform(
  const PixelType &       value,
  const InputPointType &  itkNotUsed(inputPoint),
  const OutputPointType & itkNotUsed(outputPoint))
{
  return value;
}

template <typename TPixelType, typename TTransformType, typename TOutputPointType>
void
IdentityPixelTransformation<TPixelType, TTransformType, TOutputPointType>::PrintSelf(std::ostream & os,
                                                                                     Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
