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
#ifndef itkSaltAndPepperNoiseImageFilter_hxx
#define itkSaltAndPepperNoiseImageFilter_hxx

#include "itkSaltAndPepperNoiseImageFilter.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkImageScanlineIterator.h"
#include "itkTotalProgressReporter.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
SaltAndPepperNoiseImageFilter<TInputImage, TOutputImage>::SaltAndPepperNoiseImageFilter()
  : m_SaltValue(NumericTraits<OutputImagePixelType>::max())
  , m_PepperValue(NumericTraits<OutputImagePixelType>::NonpositiveMin())
{
  this->DynamicMultiThreadingOff();
  this->ThreaderUpdateProgressOff();
}

template <class TInputImage, class TOutputImage>
void
SaltAndPepperNoiseImageFilter<TInputImage, TOutputImage>::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType)
{
  const InputImageType * inputPtr = this->GetInput();
  OutputImageType *      outputPtr = this->GetOutput(0);

  // Create a random generator per thread
  IndexValueType indSeed = 0;
  for (unsigned d = 0; d < TOutputImage::ImageDimension; d++)
  {
    indSeed += outputRegionForThread.GetIndex(d);
  }
  typename Statistics::MersenneTwisterRandomVariateGenerator::Pointer rand =
    Statistics::MersenneTwisterRandomVariateGenerator::New();
  const uint32_t seed = Self::Hash(this->GetSeed(), uint32_t(indSeed));
  rand->Initialize(seed);

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  InputImageRegionType inputRegionForThread;
  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  // Define the iterators
  ImageScanlineConstIterator<TInputImage> inputIt(inputPtr, inputRegionForThread);
  ImageScanlineIterator<TOutputImage>     outputIt(outputPtr, outputRegionForThread);

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  TotalProgressReporter progress(this, outputPtr->GetRequestedRegion().GetNumberOfPixels());

  while (!inputIt.IsAtEnd())
  {
    while (!inputIt.IsAtEndOfLine())
    {
      if (rand->GetVariate() < m_Probability)
      {
        if (rand->GetVariate() < 0.5)
        {
          // Salt
          outputIt.Set(m_SaltValue);
        }
        else
        {
          // Pepper
          outputIt.Set(m_PepperValue);
        }
      }
      else
      {
        // Keep the data unchanged
        outputIt.Set((OutputImagePixelType)inputIt.Get());
      }
      ++inputIt;
      ++outputIt;
    }
    inputIt.NextLine();
    outputIt.NextLine();
    progress.Completed(outputRegionForThread.GetSize()[0]);
  }
}

template <class TInputImage, class TOutputImage>
void
SaltAndPepperNoiseImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Probability: " << static_cast<typename NumericTraits<double>::PrintType>(m_Probability) << std::endl;
}
} // end namespace itk

#endif
