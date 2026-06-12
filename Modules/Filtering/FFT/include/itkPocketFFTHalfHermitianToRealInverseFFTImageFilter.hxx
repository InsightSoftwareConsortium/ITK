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
#ifndef itkPocketFFTHalfHermitianToRealInverseFFTImageFilter_hxx
#define itkPocketFFTHalfHermitianToRealInverseFFTImageFilter_hxx

#include "itkProgressReporter.h"
#include "itkPocketFFTCommon.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
void
PocketFFTHalfHermitianToRealInverseFFTImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  const typename InputImageType::ConstPointer inputPtr = this->GetInput();
  const typename OutputImageType::Pointer     outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  const ProgressReporter progress(this, 0, 1);

  outputPtr->SetBufferedRegion(outputPtr->GetRequestedRegion());
  outputPtr->Allocate();

  const InputSizeType  inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const OutputSizeType outputSize = outputPtr->GetLargestPossibleRegion().GetSize();

  // c2r takes the shape of the real (output) image.
  const pocketfft::shape_t  shape = PocketFFTCommon::MakeShape(outputSize, ImageDimension);
  const pocketfft::stride_t strideIn = PocketFFTCommon::MakeStride(inputSize, ImageDimension, sizeof(InputPixelType));
  const pocketfft::stride_t strideOut =
    PocketFFTCommon::MakeStride(outputSize, ImageDimension, sizeof(OutputPixelType));
  const pocketfft::shape_t axes = PocketFFTCommon::MakeAxes(ImageDimension);

  const SizeValueType   totalOutputSize = outputPtr->GetLargestPossibleRegion().GetNumberOfPixels();
  const OutputPixelType scale = OutputPixelType{ 1 } / static_cast<OutputPixelType>(totalOutputSize);

  pocketfft::c2r(shape,
                 strideIn,
                 strideOut,
                 axes,
                 pocketfft::BACKWARD,
                 inputPtr->GetBufferPointer(),
                 outputPtr->GetBufferPointer(),
                 scale);
}

template <typename TInputImage, typename TOutputImage>
SizeValueType
PocketFFTHalfHermitianToRealInverseFFTImageFilter<TInputImage, TOutputImage>::GetSizeGreatestPrimeFactor() const
{
  // All sizes are supported; sizes factoring into 2,3,5,7,11 use fast kernels.
  return 11;
}

} // namespace itk

#endif
