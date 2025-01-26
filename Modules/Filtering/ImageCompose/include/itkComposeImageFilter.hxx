/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkComposeImageFilter_hxx
#define itkComposeImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkTotalProgressReporter.h"

namespace itk
{
//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
ComposeImageFilter<TInputImage, TOutputImage>::ComposeImageFilter()
{
  int nbOfComponents = NumericTraits<OutputPixelType>::GetLength({});
  nbOfComponents = std::max(1, nbOfComponents); // require at least one input
  this->SetNumberOfRequiredInputs(nbOfComponents);
  this->DynamicMultiThreadingOn();
  this->ThreaderUpdateProgressOff();
}

//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
ComposeImageFilter<TInputImage, TOutputImage>::SetInput1(const InputImageType * image1)
{
  // The ProcessObject is not const-correct so the const_cast is required here
  this->SetNthInput(0, const_cast<InputImageType *>(image1));
}

//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
ComposeImageFilter<TInputImage, TOutputImage>::SetInput2(const InputImageType * image2)
{
  // The ProcessObject is not const-correct so the const_cast is required here
  this->SetNthInput(1, const_cast<InputImageType *>(image2));
}

//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
ComposeImageFilter<TInputImage, TOutputImage>::SetInput3(const InputImageType * image3)
{
  // The ProcessObject is not const-correct so the const_cast is required here
  this->SetNthInput(2, const_cast<InputImageType *>(image3));
}

//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
ComposeImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{
  // Override the method in itkImageSource, so we can set the vector length of
  // the output itk::VectorImage

  this->Superclass::GenerateOutputInformation();

  OutputImageType * output = this->GetOutput();
  output->SetNumberOfComponentsPerPixel(static_cast<unsigned int>(this->GetNumberOfIndexedInputs()));
}

//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
ComposeImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  // Check to verify all inputs are specified and have the same metadata,
  // spacing etc...
  const auto numberOfInputs = static_cast<const unsigned int>(this->GetNumberOfIndexedInputs());
  RegionType region;

  for (unsigned int i = 0; i < numberOfInputs; ++i)
  {
    auto * input = this->GetInput(i);
    if (!input)
    {
      itkExceptionMacro("Input " << i << " not set!");
    }
    if (i == 0)
    {
      region = input->GetLargestPossibleRegion();
    }
    else if (input->GetLargestPossibleRegion() != region)
    {
      itkExceptionMacro("All Inputs must have the same dimensions.");
    }
  }
}

//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
ComposeImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(const RegionType & outputRegionForThread)
{
  const typename OutputImageType::Pointer outputImage = this->GetOutput();

  TotalProgressReporter progress(this, outputImage->GetRequestedRegion().GetNumberOfPixels());


  InputIteratorContainerType inputItContainer;
  inputItContainer.reserve(this->GetNumberOfIndexedInputs());

  for (unsigned int i = 0; i < this->GetNumberOfIndexedInputs(); ++i)
  {
    const InputImageType * inputImage = this->GetInput(i);

    inputItContainer.emplace_back(inputImage, outputRegionForThread);
  }

  OutputPixelType pix;
  NumericTraits<OutputPixelType>::SetLength(pix, static_cast<unsigned int>(this->GetNumberOfIndexedInputs()));
  for (ImageScanlineIterator oit(outputImage, outputRegionForThread); !oit.IsAtEnd(); oit.NextLine())
  {
    while (!oit.IsAtEndOfLine())
    {
      if constexpr (std::is_same<OutputPixelType,
                                 std::complex<typename NumericTraits<OutputPixelType>::ValueType>>::value)
      {
        using ValueType = typename NumericTraits<OutputPixelType>::ValueType;
        const OutputPixelType current_pixel = OutputPixelType{ static_cast<ValueType>(inputItContainer[0].Get()),
                                                               static_cast<ValueType>(inputItContainer[1].Get()) };
        oit.Set(current_pixel);
        ++(inputItContainer[0]);
        ++(inputItContainer[1]);
      }
      else
      {
        unsigned int i = 0;
        for (auto & it : inputItContainer)
        {
          pix[i] = static_cast<typename NumericTraits<OutputPixelType>::ValueType>(it.Get());
          ++i;
          ++it;
        }

        oit.Set(pix);
      }
      ++oit;
    }
    for (auto & it : inputItContainer)
    {
      it.NextLine();
    }
    progress.Completed(outputRegionForThread.GetSize()[0]);
  }
}
} // end namespace itk

#endif
