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
  this->m_Evaluator = RieszFunctionType::New();
  this->SetOrder(1);
}

template <typename TOutputImage, typename TRieszFunction, typename TFrequencyRegionIterator>
void
RieszFrequencyFilterBankGenerator<TOutputImage, TRieszFunction, TFrequencyRegionIterator>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_Order: " << this->m_Order << std::endl;
  itkPrintSelfObjectMacro(Evaluator)
}

/* ******* Get Outputs *****/
template <typename TOutputImage, typename TRieszFunction, typename TFrequencyRegionIterator>
typename RieszFrequencyFilterBankGenerator<TOutputImage, TRieszFunction, TFrequencyRegionIterator>::OutputsType
RieszFrequencyFilterBankGenerator<TOutputImage, TRieszFunction, TFrequencyRegionIterator>::GetOutputs()
{
  OutputsType outputList;
  for (unsigned int comp = 0; comp < this->GetNumberOfOutputs(); ++comp)
  {
    outputList.push_back(this->GetOutput(comp));
  }
  return outputList;
}

template <typename TOutputImage, typename TRieszFunction, typename TFrequencyRegionIterator>
void
RieszFrequencyFilterBankGenerator<TOutputImage, TRieszFunction, TFrequencyRegionIterator>::BeforeThreadedGenerateData()
{
  /***************** Allocate Outputs *****************/
  OutputImageType * firstOutput = this->GetOutput(0);
  for (unsigned int comp = 0; comp < this->GetNumberOfOutputs(); ++comp)
  {
    OutputImageType * outputPtr = this->GetOutput(comp);
    outputPtr->SetRegions(firstOutput->GetLargestPossibleRegion());
    outputPtr->Allocate();
    outputPtr->FillBuffer(0);
  }
}

template <typename TOutputImage, typename TRieszFunction, typename TFrequencyRegionIterator>
void
RieszFrequencyFilterBankGenerator<TOutputImage, TRieszFunction, TFrequencyRegionIterator>::DynamicThreadedGenerateData(
  const OutputImageRegionType & threadRegion)
{
  // Init iterators for all outputs.
  std::vector<OutputRegionIterator> outputItList;
  for (unsigned int comp = 0; comp < this->GetNumberOfOutputs(); ++comp)
  {
    OutputImageType * outputPtr = this->GetOutput(comp);
    outputItList.push_back(OutputRegionIterator(outputPtr, threadRegion));
    outputItList.back().GoToBegin();
  }

  /***************** Set Outputs *****************/
  OutputImageType *    firstOutput = this->GetOutput(0);
  OutputRegionIterator frequencyIt(firstOutput, threadRegion);
  for (frequencyIt.GoToBegin(); !frequencyIt.IsAtEnd(); ++frequencyIt)
  {
    typename TRieszFunction::OutputComponentsType evaluatedArray =
      this->m_Evaluator->EvaluateAllComponents(frequencyIt.GetFrequency());
    for (unsigned int comp = 0; comp < this->GetNumberOfOutputs(); ++comp)
    {
      outputItList[comp].Set(static_cast<typename OutputImageType::PixelType>(evaluatedArray[comp]));
      ++outputItList[comp];
    }
    itkDebugMacro(<< "w_vector: " << frequencyIt.GetFrequency() << " w2: " << frequencyIt.GetFrequencyModuloSquare()
                  << "  frequencyItIndex: "
                  << frequencyIt.GetIndex()
                  // << "  Evaluated Riesz Components: " << evaluatedArray
                  << " outputIndex: " << outputItList[0].GetIndex());
  }
}
} // end namespace itk
#endif
