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
#ifndef __itkMeanSquaresImageToImageMetricv4_hxx
#define __itkMeanSquaresImageToImageMetricv4_hxx

#include "itkMeanSquaresImageToImageMetricv4.h"

namespace itk
{

template < class TFixedImage, class TMovingImage, class TVirtualImage, class TInternalComputationValueType, class TMetricTraits >
MeanSquaresImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType,TMetricTraits>
::MeanSquaresImageToImageMetricv4()
{
  // We have our own GetValueAndDerivativeThreader's that we want
  // ImageToImageMetricv4 to use.
  this->m_DenseGetValueAndDerivativeThreader  = MeanSquaresDenseGetValueAndDerivativeThreaderType::New();
  this->m_SparseGetValueAndDerivativeThreader = MeanSquaresSparseGetValueAndDerivativeThreaderType::New();
}

template < class TFixedImage, class TMovingImage, class TVirtualImage, class TInternalComputationValueType, class TMetricTraits >
MeanSquaresImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType,TMetricTraits>
::~MeanSquaresImageToImageMetricv4()
{
}

template < class TFixedImage, class TMovingImage, class TVirtualImage, class TInternalComputationValueType, class TMetricTraits >
void
MeanSquaresImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType,TMetricTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk


#endif
