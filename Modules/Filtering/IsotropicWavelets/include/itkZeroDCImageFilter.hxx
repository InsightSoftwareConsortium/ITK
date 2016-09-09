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
{}

template <typename TImageType>
void
ZeroDCImageFilter<TImageType>::GenerateData()
{
  ImagePointer outputPtr = this->GetOutput();
  this->AllocateOutputs();
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  /***** Calculate mean value and substract: ****/
  typedef itk::StatisticsImageFilter<ImageType> StatisticsFilterType;
  typename StatisticsFilterType::Pointer        statisticsFilter = StatisticsFilterType::New();
  statisticsFilter->SetInput(this->GetInput());
  progress->RegisterInternalFilter(statisticsFilter, 1.0f / 2.0f);
  statisticsFilter->Update();

  typedef itk::SubtractImageFilter<ImageType> SubtractFilterType;
  typename SubtractFilterType::Pointer        subtractFilter = SubtractFilterType::New();
  subtractFilter->SetInput1(this->GetInput());
  subtractFilter->SetConstant2(statisticsFilter->GetMean());
  progress->RegisterInternalFilter(subtractFilter, 1.0f / 2.0f);
  subtractFilter->GraftOutput(outputPtr);
  subtractFilter->Update();
  this->GraftOutput(outputPtr);
  /**********************************************/
}
} // end namespace itk

#endif
