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
#ifndef itkCastImageFilter_hxx
#define itkCastImageFilter_hxx

#include "itkProgressReporter.h"
#include "itkImageAlgorithm.h"
#include "itkImageRegionRange.h"
#include "itkVariableLengthVector.h"

namespace itk
{


template <typename TInputImage, typename TOutputImage>
CastImageFilter<TInputImage, TOutputImage>::CastImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  this->InPlaceOff();
  this->DynamicMultiThreadingOn();
}

template <typename TInputImage, typename TOutputImage>
void
CastImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  if (this->GetInPlace() && this->CanRunInPlace())
  {
    // nothing to do, so avoid iterating over all the pixels
    // for nothing! Allocate the output, generate a fake progress and exit
    this->AllocateOutputs();
    const ProgressReporter progress(this, 0, 1);
    return;
  }
  // else do normal Before+Threaded+After
  Superclass::GenerateData();
}


template <typename TInputImage, typename TOutputImage>
void
CastImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{
  // do not call the superclass' implementation of this method since
  // this filter allows the input the output to be of different dimensions

  // get pointers to the input and output
  TOutputImage *      outputPtr = this->GetOutput();
  const TInputImage * inputPtr = this->GetInput();

  if (!outputPtr || !inputPtr)
  {
    return;
  }

  // Set the output image largest possible region.  Use a RegionCopier
  // so that the input and output images can be different dimensions.
  OutputImageRegionType outputLargestPossibleRegion;
  this->CallCopyInputRegionToOutputRegion(outputLargestPossibleRegion, inputPtr->GetLargestPossibleRegion());
  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);

  const ImageToImageFilterDetail::ImageInformationCopier<Superclass::OutputImageDimension,
                                                         Superclass::InputImageDimension>
    informationCopier;
  informationCopier(outputPtr, inputPtr);
}


template <typename TInputImage, typename TOutputImage>
void
CastImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  DynamicThreadedGenerateDataDispatched<InputPixelType, OutputPixelType>(outputRegionForThread);
}

template <typename TInputImage, typename TOutputImage>
template <typename TInputPixelType,
          typename TOutputPixelType,
          std::enable_if_t<mpl::is_static_castable<TInputPixelType, TOutputPixelType>::value, int>>
void
CastImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateDataDispatched(
  const OutputImageRegionType & outputRegionForThread)
{
  const TInputImage * inputPtr = this->GetInput();
  TOutputImage *      outputPtr = this->GetOutput(0);

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  typename TInputImage::RegionType inputRegionForThread;

  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  ImageAlgorithm::Copy(inputPtr, outputPtr, inputRegionForThread, outputRegionForThread);
}


template <typename TInputImage, typename TOutputImage>
template <typename TInputPixelType,
          typename TOutputPixelType,
          std::enable_if_t<!mpl::is_static_castable<TInputPixelType, TOutputPixelType>::value, int>>
void
CastImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateDataDispatched(
  const OutputImageRegionType & outputRegionForThread)
{
  // Implementation for non-implicit convertible pixels which are
  // itk-array-like.

  static_assert(std::is_convertible_v<typename InputPixelType::ValueType, typename OutputPixelType::ValueType>,
                "Component types are required to be convertible.");

  const typename OutputImageRegionType::SizeType & regionSize = outputRegionForThread.GetSize();

  if (regionSize[0] == 0)
  {
    return;
  }
  const TInputImage * inputPtr = this->GetInput();
  TOutputImage *      outputPtr = this->GetOutput(0);

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  typename TInputImage::RegionType inputRegionForThread;

  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  ImageRegionRange<const TInputImage> inputRange(*inputPtr, inputRegionForThread);
  ImageRegionRange<TOutputImage>      outputRange(*outputPtr, outputRegionForThread);

  auto       inputIt = inputRange.begin();
  auto       outputIt = outputRange.begin();
  const auto inputEnd = inputRange.end();

  // Note: This loop has been timed for performance with conversions between image of vectors and VectorImages and other
  // combinations. The following was evaluated to be the best performance usage of iterators. Important considerations:
  //  - Usage of NumericTraits::GetLength() is sometimes consant vs virutal method GetNumberOfComponentsPerPixel()
  //  - The construction of inputPixel and outputPixel for VectorImages both reference the internal buffer and don't
  //  require memory allocations.
  const unsigned int componentsPerPixel = itk::NumericTraits<OutputPixelType>::GetLength(*outputIt);
  while (inputIt != inputEnd)
  {
    const InputPixelType & inputPixel = *inputIt;

    using OutputPixelValueType = typename OutputPixelType::ValueType;

    constexpr bool isVariableLengthVector = std::is_same_v<OutputPixelType, VariableLengthVector<OutputPixelValueType>>;

    // If the output pixel type is a VariableLengthVector, it behaves as a "reference" to the internal data. Otherwise
    // declare outputPixel as a reference, `OutputPixelType &`, to allow it to access the internal buffer directly.
    std::conditional_t<isVariableLengthVector, OutputPixelType, OutputPixelType &> outputPixel{ *outputIt };

    for (unsigned int k = 0; k < componentsPerPixel; ++k)
    {
      outputPixel[k] = static_cast<OutputPixelValueType>(inputPixel[k]);
    }
    ++inputIt;
    ++outputIt;
  }
}

} // end namespace itk

#endif
