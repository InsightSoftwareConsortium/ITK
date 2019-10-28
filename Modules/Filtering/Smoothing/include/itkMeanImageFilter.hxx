/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkMeanImageFilter_hxx
#define itkMeanImageFilter_hxx
#include "itkMeanImageFilter.h"

#include "itkBufferedImageNeighborhoodPixelAccessPolicy.h"
#include "itkImageNeighborhoodOffsets.h"
#include "itkImageRegionRange.h"
#include "itkIndexRange.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkOffset.h"
#include "itkShapedImageNeighborhoodRange.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
MeanImageFilter<TInputImage, TOutputImage>::MeanImageFilter()
{
  this->DynamicMultiThreadingOn();
}

template <typename TInputImage, typename TOutputImage>
void
MeanImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  typename OutputImageType::Pointer     output = this->GetOutput();
  typename InputImageType::ConstPointer input = this->GetInput();

  // Find the data-set boundary "faces"
  const auto radius = this->GetRadius();
  const auto faceList =
    NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>{}(input, outputRegionForThread, radius);
  auto fit = faceList.cbegin();
  auto endOfFaces = faceList.cend();

  if (fit != endOfFaces)
  {
    using namespace Experimental;
    const auto neighborhoodShape = RectangularImageNeighborhoodShape<InputImageDimension>(radius);
    const auto neighborhoodSize = neighborhoodShape.GetNumberOfOffsets();
    const auto neighborhoodOffsets =
      std::unique_ptr<Offset<InputImageDimension>[]>(new Offset<InputImageDimension>[neighborhoodSize]);
    neighborhoodShape.FillOffsets(neighborhoodOffsets.get());

    GenerateDataInSubregion<BufferedImageNeighborhoodPixelAccessPolicy<InputImageType>>(
      *input, *output, *fit, neighborhoodOffsets.get(), neighborhoodSize);
    ++fit;

    // Process each of the other boundary faces. These are N-d regions which border
    // the edge of the buffer.
    while (fit != endOfFaces)
    {
      GenerateDataInSubregion<ZeroFluxNeumannImageNeighborhoodPixelAccessPolicy<InputImageType>>(
        *input, *output, *fit, neighborhoodOffsets.get(), neighborhoodSize);
      ++fit;
    }
  }
}


template <typename TInputImage, typename TOutputImage>
template <typename TPixelAccessPolicy>
void
MeanImageFilter<TInputImage, TOutputImage>::GenerateDataInSubregion(
  const TInputImage &                       inputImage,
  TOutputImage &                            outputImage,
  const ImageRegion<InputImageDimension> &  imageRegion,
  const Offset<InputImageDimension> * const neighborhoodOffsets,
  const std::size_t                         neighborhoodSize)
{
  using namespace Experimental;
  auto neighborhoodRange = ShapedImageNeighborhoodRange<const InputImageType, TPixelAccessPolicy>(
    inputImage, Index<InputImageDimension>(), neighborhoodOffsets, neighborhoodSize);
  const auto outputImageRegionRange = ImageRegionRange<OutputImageType>(outputImage, imageRegion);
  auto       outputIterator = outputImageRegionRange.begin();
  auto       indexIterator = ImageRegionIndexRange<InputImageDimension>(imageRegion).cbegin();

  for (auto i = outputImageRegionRange.size(); i > 0; --i)
  {
    neighborhoodRange.SetLocation(*indexIterator);

    auto sum = NumericTraits<InputRealType>::ZeroValue();

    for (const InputPixelType pixelValue : neighborhoodRange)
    {
      sum += static_cast<InputRealType>(pixelValue);
    }

    // get the mean value
    *outputIterator = static_cast<typename OutputImageType::PixelType>(sum / double(neighborhoodSize));

    ++indexIterator;
    ++outputIterator;
  }
}
} // end namespace itk

#endif
