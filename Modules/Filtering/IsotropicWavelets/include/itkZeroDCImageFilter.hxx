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
#ifndef itkZeroDCImageFilter_hxx
#define itkZeroDCImageFilter_hxx

#include "itkZeroDCImageFilter.h"
#include <itkSubtractImageFilter.h>
#include <itkStatisticsImageFilter.h>
#include "itkProgressAccumulator.h"

namespace itk
{
/**
 * Default constructor
 */
template <typename TImageType>
ZeroDCImageFilter<TImageType>::ZeroDCImageFilter()
{
  m_StatisticsFilter = StatisticsFilterType::New();
  m_SubtractFilter = SubtractFilterType::New();
}

template <typename TImageType>
void
ZeroDCImageFilter<TImageType>::GenerateData()
{
  ImagePointer outputPtr = this->GetOutput();

  this->AllocateOutputs();
  auto progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  /***** Calculate mean value and subtract: ****/
  m_StatisticsFilter->SetInput(this->GetInput());
  progress->RegisterInternalFilter(m_StatisticsFilter, 1.0f / 2.0f);
  m_StatisticsFilter->Update();

  m_SubtractFilter->SetInput1(this->GetInput());
  m_SubtractFilter->SetConstant2(m_StatisticsFilter->GetMean());
  progress->RegisterInternalFilter(m_SubtractFilter, 1.0f / 2.0f);
  m_SubtractFilter->GraftOutput(outputPtr);
  m_SubtractFilter->Update();
  this->GraftOutput(m_SubtractFilter->GetOutput());
  /**********************************************/
}

template <typename TImageType>
void
ZeroDCImageFilter<TImageType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(StatisticsFilter);
  itkPrintSelfObjectMacro(SubtractFilter);
}
} // end namespace itk

#endif
