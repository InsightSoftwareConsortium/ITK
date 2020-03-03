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
#ifndef itkUnaryGeneratorImageFilter_hxx
#define itkUnaryGeneratorImageFilter_hxx

#include "itkUnaryGeneratorImageFilter.h"
#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"
#include "itkTotalProgressReporter.h"

namespace itk
{
/**
 * Constructor
 */
template <typename TInputImage, typename TOutputImage>
UnaryGeneratorImageFilter<TInputImage, TOutputImage>::UnaryGeneratorImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  this->InPlaceOff();
  this->DynamicMultiThreadingOn();
}

/**
 * UnaryGeneratorImageFilter can produce an image which is a different resolution
 * than its input image.  As such, UnaryGeneratorImageFilter needs to provide an
 * implementation for GenerateOutputInformation() in order to inform
 * the pipeline execution model.  The original documentation of this
 * method is below.
 *
 * \sa ProcessObject::GenerateOutputInformation()
 */
template <typename TInputImage, typename TOutputImage>
void
UnaryGeneratorImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{

  // do not call the superclass' implementation of this method since
  // this filter allows the input the output to be of different dimensions

  // get pointers to the input and output
  auto *                 outputPtr = dynamic_cast<OutputImageType *>(this->GetOutput());
  const InputImageType * inputPtr = this->GetInput();

  if (!outputPtr || !inputPtr)
  {
    return;
  }

  // Set the output image largest possible region.  Use a RegionCopier
  // so that the input and output images can be different dimensions.
  OutputImageRegionType outputLargestPossibleRegion;
  this->CallCopyInputRegionToOutputRegion(outputLargestPossibleRegion, inputPtr->GetLargestPossibleRegion());
  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);

  ImageToImageFilterDetail::ImageInformationCopier<Superclass::OutputImageDimension, Superclass::InputImageDimension>
    informationCopier;
  informationCopier(outputPtr, inputPtr);
}


template <typename TInputImage, typename TOutputImage>
void
UnaryGeneratorImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  m_DynamicThreadedGenerateDataFunction(outputRegionForThread);
}


template <typename TInputImage, typename TOutputImage>
template <typename TFunctor>
void
UnaryGeneratorImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateDataWithFunctor(
  const TFunctor &              functor,
  const OutputImageRegionType & outputRegionForThread)
{
  const typename OutputImageRegionType::SizeType & regionSize = outputRegionForThread.GetSize();

  const TInputImage * inputPtr = this->GetInput();
  TOutputImage *      outputPtr = this->GetOutput(0);

  TotalProgressReporter progress(this, outputPtr->GetRequestedRegion().GetNumberOfPixels());

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
  while (!inputIt.IsAtEnd())
  {
    while (!inputIt.IsAtEndOfLine())
    {
      outputIt.Set(functor(inputIt.Get()));
      ++inputIt;
      ++outputIt;
    }
    progress.Completed(regionSize[0]);
    inputIt.NextLine();
    outputIt.NextLine();
  }
}
} // end namespace itk

#endif
