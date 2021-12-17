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
#ifndef itkMonogenicSignalFrequencyImageFilter_hxx
#define itkMonogenicSignalFrequencyImageFilter_hxx
#include "itkImageRegionIterator.h"
namespace itk
{
template <typename TInputImage, typename TFrequencyImageRegionConstIterator>
MonogenicSignalFrequencyImageFilter<TInputImage,
                                    TFrequencyImageRegionConstIterator>::MonogenicSignalFrequencyImageFilter()
{
  m_Evaluator = RieszFunctionType::New();
  m_Evaluator->SetOrder(1);

  this->DynamicMultiThreadingOn();
}

template <typename TInputImage, typename TFrequencyImageRegionConstIterator>
void
MonogenicSignalFrequencyImageFilter<TInputImage, TFrequencyImageRegionConstIterator>::GenerateOutputInformation()
{
  this->Superclass::GenerateOutputInformation();

  OutputImageType * output = this->GetOutput();
  output->SetNumberOfComponentsPerPixel(ImageDimension + 1);
}

template <typename TInputImage, typename TFrequencyImageRegionConstIterator>
void
MonogenicSignalFrequencyImageFilter<TInputImage, TFrequencyImageRegionConstIterator>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  // Allocate the outputs
  this->AllocateOutputs();
  InputFrequencyImageRegionConstIterator inFreqIt(this->GetInput(), outputRegionForThread);
  ImageRegionIterator<OutputImageType>   outIt(this->GetOutput(), outputRegionForThread);

  typename RieszFunctionType::OutputComponentsType evaluatedArray;
  typename OutputImageType::PixelType              out_value;
  for (inFreqIt.GoToBegin(), outIt.GoToBegin(); !inFreqIt.IsAtEnd(); ++inFreqIt, ++outIt)
  {
    evaluatedArray = this->m_Evaluator->EvaluateAllComponents(inFreqIt.GetFrequency());
    out_value = outIt.Get();
    out_value[0] = inFreqIt.Get();
    for (unsigned int dir = 0; dir < ImageDimension; ++dir)
    {
      // This is a complex number multiplication.
      out_value[dir + 1] = inFreqIt.Get() * evaluatedArray[dir];
    }
    outIt.Set(out_value);
  }
}

template <typename TInputImage, typename TFrequencyImageRegionConstIterator>
void
MonogenicSignalFrequencyImageFilter<TInputImage, TFrequencyImageRegionConstIterator>::PrintSelf(std::ostream & os,
                                                                                                Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_Evaluator: ";
  if (!this->m_Evaluator)
  {
    os << "0" << std::endl;
  }
  else
  {
    os << this->m_Evaluator << std::endl;
  }
}
} // end namespace itk
#endif
