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
#ifndef itkPocketFFTComplexToComplexFFTImageFilter_hxx
#define itkPocketFFTComplexToComplexFFTImageFilter_hxx

#include "itkProgressReporter.h"
#include "itkImageAlgorithm.h"
#include "itkPocketFFTCommon.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
void
PocketFFTComplexToComplexFFTImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  const InputImageType *  input = this->GetInput();
  OutputImageType * const output = this->GetOutput();

  if (!input || !output)
  {
    return;
  }

  const ProgressReporter progress(this, 0, 1);

  const typename ImageType::RegionType bufferedRegion = input->GetBufferedRegion();
  const typename ImageType::SizeType & imageSize = bufferedRegion.GetSize();

  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Copy the input to the output and transform in place on the output.
  ImageAlgorithm::Copy<InputImageType, OutputImageType>(input, output, bufferedRegion, bufferedRegion);

  const pocketfft::shape_t  shape = PocketFFTCommon::MakeShape(imageSize, ImageDimension);
  const pocketfft::stride_t stride = PocketFFTCommon::MakeStride(imageSize, ImageDimension, sizeof(PixelType));
  const pocketfft::shape_t  axes = PocketFFTCommon::MakeAxes(ImageDimension);

  using ValueType = typename PixelType::value_type;
  PixelType * buffer = output->GetBufferPointer();

  const bool inverse = this->GetTransformDirection() == Superclass::TransformDirectionEnum::INVERSE;
  const auto scale =
    inverse ? ValueType{ 1 } / static_cast<ValueType>(bufferedRegion.GetNumberOfPixels()) : ValueType{ 1 };

  pocketfft::c2c(
    shape, stride, stride, axes, inverse ? pocketfft::BACKWARD : pocketfft::FORWARD, buffer, buffer, scale);
}

} // namespace itk

#endif
