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
#ifndef itkANTSNeighborhoodCorrelationImageToImageMetricv4_hxx
#define itkANTSNeighborhoodCorrelationImageToImageMetricv4_hxx

#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkNumericTraits.h"

namespace itk
{

template<typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
ANTSNeighborhoodCorrelationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::ANTSNeighborhoodCorrelationImageToImageMetricv4()
{
  // initialize radius. note that a radius of 1 can be unstable
  typedef typename RadiusType::SizeValueType RadiusValueType;
  this->m_Radius.Fill( static_cast<RadiusValueType>(2) );
  // We have our own GetValueAndDerivativeThreader's that we want
  // ImageToImageMetricv4 to use.
  this->m_DenseGetValueAndDerivativeThreader  = ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreaderType::New();
  this->m_SparseGetValueAndDerivativeThreader = ANTSNeighborhoodCorrelationImageToImageMetricv4SparseGetValueAndDerivativeThreaderType::New();
}

template<typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
ANTSNeighborhoodCorrelationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::~ANTSNeighborhoodCorrelationImageToImageMetricv4()
{
}

template<typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ANTSNeighborhoodCorrelationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::Initialize(void)
{
  Superclass::Initialize();
}

template<typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ANTSNeighborhoodCorrelationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Correlation window radius: " << m_Radius << std::endl;
}

} // end namespace itk

#endif
