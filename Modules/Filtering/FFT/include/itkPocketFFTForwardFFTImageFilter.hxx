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
#ifndef itkPocketFFTForwardFFTImageFilter_hxx
#define itkPocketFFTForwardFFTImageFilter_hxx

#include "itkProgressReporter.h"
#include "itkPocketFFTCommon.h"
#include "pocketfft_hdronly.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
void
PocketFFTForwardFFTImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  const typename InputImageType::ConstPointer inputPtr = this->GetInput();
  const typename OutputImageType::Pointer     outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  const ProgressReporter progress(this, 0, 1);

  const InputSizeType inputSize = inputPtr->GetLargestPossibleRegion().GetSize();

  outputPtr->SetBufferedRegion(outputPtr->GetRequestedRegion());
  outputPtr->Allocate();

  const pocketfft::shape_t  shape = PocketFFTCommon::MakeShape(inputSize, ImageDimension);
  const pocketfft::stride_t stride = PocketFFTCommon::MakeStride(inputSize, ImageDimension, sizeof(OutputPixelType));
  const pocketfft::shape_t  axes = PocketFFTCommon::MakeAxes(ImageDimension);

  const SizeValueType    totalSize = inputPtr->GetLargestPossibleRegion().GetNumberOfPixels();
  const InputPixelType * in = inputPtr->GetBufferPointer();
  OutputPixelType *      out = outputPtr->GetBufferPointer();
  for (SizeValueType i = 0; i < totalSize; ++i)
  {
    out[i] = OutputPixelType(in[i], 0);
  }

  pocketfft::c2c(shape,
                 stride,
                 stride,
                 axes,
                 pocketfft::FORWARD,
                 out,
                 out,
                 InputPixelType{ 1 },
                 this->GetMultiThreader()->GetMaximumNumberOfThreads());
}

template <typename TInputImage, typename TOutputImage>
SizeValueType
PocketFFTForwardFFTImageFilter<TInputImage, TOutputImage>::GetSizeGreatestPrimeFactor() const
{
  // All sizes are supported; sizes factoring into 2,3,5,7,11 use fast kernels.
  return 11;
}

} // namespace itk

#endif
