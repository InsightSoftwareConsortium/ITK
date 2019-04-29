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
#ifndef itkVectorThresholdSegmentationLevelSetImageFilter_hxx
#define itkVectorThresholdSegmentationLevelSetImageFilter_hxx

#include "itkVectorThresholdSegmentationLevelSetImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TFeatureImage, typename TOutputType >
VectorThresholdSegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::VectorThresholdSegmentationLevelSetImageFilter()
{
  m_ThresholdFunction = ThresholdFunctionType::New();
  m_ThresholdFunction->SetThreshold(0);

  this->SetSegmentationFunction(m_ThresholdFunction);
}

template< typename TInputImage, typename TFeatureImage, typename TOutputType >
void
VectorThresholdSegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "ThresholdFunction: " << m_ThresholdFunction;
}
} // end namespace itk

#endif
