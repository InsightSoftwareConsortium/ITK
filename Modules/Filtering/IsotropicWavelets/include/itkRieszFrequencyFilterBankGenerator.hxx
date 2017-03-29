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
#ifndef itkRieszFrequencyFilterBankGenerator_hxx
#define itkRieszFrequencyFilterBankGenerator_hxx
#include "itkRieszFrequencyFilterBankGenerator.h"
#include "itkNumericTraits.h"

namespace itk
{
template <typename TOutputImage, typename TRieszFunction, typename TFrequencyRegionIterator>
RieszFrequencyFilterBankGenerator<TOutputImage, TRieszFunction, TFrequencyRegionIterator>::
  RieszFrequencyFilterBankGenerator()
{
  this->SetNumberOfRequiredOutputs(ImageDimension);
  for (unsigned int dir = 0; dir < ImageDimension; ++dir)
  {
    this->SetNthOutput(dir, this->MakeOutput(dir));
  }
}

template <typename TOutputImage, typename TRieszFunction, typename TFrequencyRegionIterator>
void
RieszFrequencyFilterBankGenerator<TOutputImage, TRieszFunction, TFrequencyRegionIterator>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
}

/* ******* Get Outputs *****/
template <typename TOutputImage, typename TRieszFunction, typename TFrequencyRegionIterator>
std::vector<typename RieszFrequencyFilterBankGenerator<TOutputImage, TRieszFunction, TFrequencyRegionIterator>::
              OutputImagePointer>
RieszFrequencyFilterBankGenerator<TOutputImage, TRieszFunction, TFrequencyRegionIterator>::GetOutputs()
{
  std::vector<OutputImagePointer> outputList;
  for (unsigned int dir = 0; dir < ImageDimension; ++dir)
  {
    outputList.push_back(this->GetOutput(dir));
  }
  return outputList;
}

template <typename TOutputImage, typename TRieszFunction, typename TFrequencyRegionIterator>
void
RieszFrequencyFilterBankGenerator<TOutputImage, TRieszFunction, TFrequencyRegionIterator>::GenerateData()
{
  typename RieszFunctionType::Pointer evaluator = RieszFunctionType::New();

  /***************** Allocate Outputs *****************/
  std::vector<OutputImagePointer>   outputList;
  std::vector<OutputRegionIterator> outputItList;
  for (unsigned int dir = 0; dir < ImageDimension; ++dir)
  {
    outputList.push_back(this->GetOutput(dir));
    OutputImagePointer & outputPtr = outputList.back();
    // GenerateImageSource superclass allocates primary output, so use its region.
    outputPtr->SetRegions(outputList[0]->GetLargestPossibleRegion());
    outputPtr->Allocate();
    outputPtr->FillBuffer(0);
    outputItList.push_back(OutputRegionIterator(outputPtr, outputPtr->GetRequestedRegion()));
    outputItList.back().GoToBegin();
  }

  /***************** Set Outputs *****************/
  OutputRegionIterator frequencyIt(outputList[0], outputList[0]->GetRequestedRegion());
  for (frequencyIt.GoToBegin(); !frequencyIt.IsAtEnd(); ++frequencyIt)
  {
    typename TRieszFunction::OutputComplexArrayType evaluatedArray =
      evaluator->EvaluateArray(frequencyIt.GetFrequency());
    for (unsigned int dir = 0; dir < ImageDimension; ++dir)
    {
      outputItList[dir].Set(outputItList[dir].Get() + evaluatedArray[dir].imag());
      ++outputItList[dir];
    }
    itkDebugMacro(<< "w_vector: " << frequencyIt.GetFrequency() << " w2: " << frequencyIt.GetFrequencyModuloSquare()
                  << "  frequencyItIndex: " << frequencyIt.GetIndex() << "  Evaluated Riesz Components: "
                  << evaluatedArray << " outputIndex: " << outputItList[0].GetIndex());
  }
}
} // end namespace itk
#endif
