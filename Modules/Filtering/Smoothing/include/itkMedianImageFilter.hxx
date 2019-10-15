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
#ifndef itkMedianImageFilter_hxx
#define itkMedianImageFilter_hxx
#include "itkMedianImageFilter.h"

#include "itkBufferedImageNeighborhoodPixelAccessPolicy.h"
#include "itkImageNeighborhoodOffsets.h"
#include "itkImageRegionRange.h"
#include "itkIndexRange.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkOffset.h"
#include "itkShapedImageNeighborhoodRange.h"
#include "itkTotalProgressReporter.h"

#include <vector>
#include <algorithm>

namespace itk
{
template <typename TInputImage, typename TOutputImage>
MedianImageFilter<TInputImage, TOutputImage>::MedianImageFilter()
{
  this->DynamicMultiThreadingOn();
  this->ThreaderUpdateProgressOff();
}

template <typename TInputImage, typename TOutputImage>
void
MedianImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  // Allocate output
  OutputImageType *      output = this->GetOutput();
  const InputImageType * input = this->GetInput();

  const auto radius = this->GetRadius();

  // Find the data-set boundary "faces" and the center non-boundary subregion.
  const auto calculatorResult =
    NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::Compute(*input, outputRegionForThread, radius);

  const auto neighborhoodOffsets = GenerateRectangularImageNeighborhoodOffsets<InputImageDimension>(radius);
  const auto neighborhoodSize = neighborhoodOffsets.size();

  // All of our neighborhoods have an odd number of pixels, so there is
  // always a median.
  std::vector<InputPixelType> pixels(neighborhoodSize);
  const auto                  medianIterator = pixels.begin() + (neighborhoodSize / 2);


  TotalProgressReporter progress(this, output->GetRequestedRegion().GetNumberOfPixels());

  const auto nonBoundaryRegion = calculatorResult.GetNonBoundaryRegion();
  if (!nonBoundaryRegion.GetSize().empty())
  {
    // Process the non-boundary subregion, using a faster pixel access policy without boundary extrapolation.
    auto neighborhoodRange =
      ShapedImageNeighborhoodRange<const InputImageType, BufferedImageNeighborhoodPixelAccessPolicy<InputImageType>>(
        *input, Index<InputImageDimension>(), neighborhoodOffsets);
    auto outputIterator = ImageRegionRange<OutputImageType>(*output, nonBoundaryRegion).begin();

    for (const auto & index : ImageRegionIndexRange<InputImageDimension>(nonBoundaryRegion))
    {
      neighborhoodRange.SetLocation(index);
      std::copy_n(neighborhoodRange.cbegin(), neighborhoodSize, pixels.begin());
      std::nth_element(pixels.begin(), medianIterator, pixels.end());
      *outputIterator = *medianIterator;
      ++outputIterator;
      progress.CompletedPixel();
    }
  }

  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for (const auto & boundaryFace : calculatorResult.GetBoundaryFaces())
  {
    auto neighborhoodRange =
      ShapedImageNeighborhoodRange<const InputImageType>(*input, Index<InputImageDimension>(), neighborhoodOffsets);
    auto outputIterator = ImageRegionRange<OutputImageType>(*output, boundaryFace).begin();

    for (const auto & index : ImageRegionIndexRange<InputImageDimension>(boundaryFace))
    {
      neighborhoodRange.SetLocation(index);
      std::copy_n(neighborhoodRange.cbegin(), neighborhoodSize, pixels.begin());
      std::nth_element(pixels.begin(), medianIterator, pixels.end());
      *outputIterator = *medianIterator;
      ++outputIterator;
      progress.CompletedPixel();
    }
  }
}
} // end namespace itk

#endif
