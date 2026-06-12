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
#ifndef itkPocketFFTInverseFFTImageFilter_hxx
#define itkPocketFFTInverseFFTImageFilter_hxx

#include "itkProgressReporter.h"
#include "itk_pocketfft_hdronly.h"

#include <vector>

namespace itk
{

template <typename TInputImage, typename TOutputImage>
void
PocketFFTInverseFFTImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  const typename InputImageType::ConstPointer inputPtr = this->GetInput();
  const typename OutputImageType::Pointer     outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  const ProgressReporter progress(this, 0, 1);

  const OutputSizeType outputSize = outputPtr->GetLargestPossibleRegion().GetSize();

  outputPtr->SetBufferedRegion(outputPtr->GetRequestedRegion());
  outputPtr->Allocate();

  // pocketfft shape is listed slowest-varying axis first; the ITK buffer is
  // x-fastest, so dimension i maps to shape index (ImageDimension - 1 - i).
  pocketfft::shape_t  shape(ImageDimension);
  pocketfft::stride_t stride(ImageDimension);
  pocketfft::shape_t  axes(ImageDimension);
  ptrdiff_t           byteStride = sizeof(InputPixelType);
  SizeValueType       totalSize = 1;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    shape[ImageDimension - 1 - i] = outputSize[i];
    stride[ImageDimension - 1 - i] = byteStride;
    byteStride *= static_cast<ptrdiff_t>(outputSize[i]);
    totalSize *= outputSize[i];
  }
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    axes[i] = i;
  }

  const InputPixelType *      in = inputPtr->GetBufferPointer();
  std::vector<InputPixelType> work(in, in + totalSize);

  const OutputPixelType scale = OutputPixelType{ 1 } / static_cast<OutputPixelType>(totalSize);
  pocketfft::c2c(shape, stride, stride, axes, pocketfft::BACKWARD, work.data(), work.data(), scale);

  OutputPixelType * out = outputPtr->GetBufferPointer();
  for (SizeValueType i = 0; i < totalSize; ++i)
  {
    out[i] = work[i].real();
  }
}

template <typename TInputImage, typename TOutputImage>
SizeValueType
PocketFFTInverseFFTImageFilter<TInputImage, TOutputImage>::GetSizeGreatestPrimeFactor() const
{
  // All sizes are supported; sizes factoring into 2,3,5,7,11 use fast kernels.
  return 11;
}

} // namespace itk

#endif
