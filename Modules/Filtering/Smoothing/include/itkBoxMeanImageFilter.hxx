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
#ifndef itkBoxMeanImageFilter_hxx
#define itkBoxMeanImageFilter_hxx

#include "itkProgressAccumulator.h"
#include "itkBoxUtilities.h"


/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://www.insight-journal.org/browse/publication/160
 *
 */

namespace itk
{
template <typename TInputImage, typename TOutputImage>
BoxMeanImageFilter<TInputImage, TOutputImage>::BoxMeanImageFilter()
{
  this->DynamicMultiThreadingOn();
}

template <typename TInputImage, typename TOutputImage>
void
BoxMeanImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  // Accumulate type is too small
  using AccPixType = typename NumericTraits<PixelType>::RealType;
  using AccumImageType = Image<AccPixType, TInputImage::ImageDimension>;

  typename TInputImage::SizeType internalRadius;
  for (unsigned int i = 0; i < TInputImage::ImageDimension; ++i)
  {
    internalRadius[i] = this->GetRadius()[i] + 1;
  }

  const InputImageType * inputImage = this->GetInput();
  OutputImageType *      outputImage = this->GetOutput();
  RegionType             accumRegion = outputRegionForThread;
  accumRegion.PadByRadius(internalRadius);
  accumRegion.Crop(inputImage->GetRequestedRegion());

  auto accImage = AccumImageType::New();
  accImage->SetRegions(accumRegion);
  accImage->Allocate();

  BoxAccumulateFunction<TInputImage, AccumImageType>(inputImage, accImage, accumRegion, accumRegion);
  BoxMeanCalculatorFunction<AccumImageType, TOutputImage>(
    accImage.GetPointer(), outputImage, accumRegion, outputRegionForThread, this->GetRadius());
}
} // end namespace itk
#endif
