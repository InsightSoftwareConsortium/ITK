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
#include "itkDefaultConvertPixelTraits.h"

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

  const auto radius = this->GetRadius();

  // Find the data-set boundary "faces" and the center non-boundary subregion.
  const auto calculatorResult =
    NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::Compute(*input, outputRegionForThread, radius);

  const auto neighborhoodOffsets = GenerateRectangularImageNeighborhoodOffsets<InputImageDimension>(radius);

  // Process the non-boundary subregion, using a faster pixel access policy without boundary extrapolation.
  GenerateDataInSubregion<BufferedImageNeighborhoodPixelAccessPolicy<InputImageType>>(
    *input,
    *output,
    calculatorResult.GetNonBoundaryRegion(),
    neighborhoodOffsets,
    static_cast<InputPixelType *>(nullptr));

  // Process each of the boundary faces. These are N-d regions which border
  // the edge of the buffer.
  for (const auto & boundaryFace : calculatorResult.GetBoundaryFaces())
  {
    GenerateDataInSubregion<ZeroFluxNeumannImageNeighborhoodPixelAccessPolicy<InputImageType>>(
      *input, *output, boundaryFace, neighborhoodOffsets, static_cast<InputPixelType *>(nullptr));
  }
}


template <typename TInputImage, typename TOutputImage>
template <typename TPixelAccessPolicy, typename TPixelType>
void
MeanImageFilter<TInputImage, TOutputImage>::GenerateDataInSubregion(
  const TInputImage &                              inputImage,
  TOutputImage &                                   outputImage,
  const ImageRegion<InputImageDimension> &         imageRegion,
  const std::vector<Offset<InputImageDimension>> & neighborhoodOffsets,
  const TPixelType *)
{
  const auto neighborhoodSize = static_cast<double>(neighborhoodOffsets.size());

  auto neighborhoodRange = ShapedImageNeighborhoodRange<const InputImageType, TPixelAccessPolicy>(
    inputImage, Index<InputImageDimension>(), neighborhoodOffsets);
  auto outputIterator = ImageRegionRange<OutputImageType>(outputImage, imageRegion).begin();

  for (const auto & index : ImageRegionIndexRange<InputImageDimension>(imageRegion))
  {
    neighborhoodRange.SetLocation(index);

    auto sum = NumericTraits<InputRealType>::ZeroValue();

    for (const InputPixelType pixelValue : neighborhoodRange)
    {
      sum += static_cast<InputRealType>(pixelValue);
    }

    // get the mean value
    *outputIterator = static_cast<typename OutputImageType::PixelType>(sum / neighborhoodSize);
    ++outputIterator;
  }
}

template <typename TInputImage, typename TOutputImage>
template <typename TPixelAccessPolicy, typename TValueType>
void
MeanImageFilter<TInputImage, TOutputImage>::GenerateDataInSubregion(
  const TInputImage &                              inputImage,
  TOutputImage &                                   outputImage,
  const ImageRegion<InputImageDimension> &         imageRegion,
  const std::vector<Offset<InputImageDimension>> & neighborhoodOffsets,
  const VariableLengthVector<TValueType> *)
{
  const auto neighborhoodSize = static_cast<double>(neighborhoodOffsets.size());

  auto neighborhoodRange = ShapedImageNeighborhoodRange<const InputImageType, TPixelAccessPolicy>(
    inputImage, Index<InputImageDimension>(), neighborhoodOffsets);
  auto outputIterator = ImageRegionRange<OutputImageType>(outputImage, imageRegion).begin();

  // These temp variable are needed outside the loop for
  // VariableLengthVectors to avoid memory allocations on a per-pixel
  // basis.
  InputRealType   sum(inputImage.GetNumberOfComponentsPerPixel());
  OutputPixelType out(inputImage.GetNumberOfComponentsPerPixel());

  for (const auto & index : ImageRegionIndexRange<InputImageDimension>(imageRegion))
  {
    neighborhoodRange.SetLocation(index);

    using PixelComponentType = typename NumericTraits<InputRealType>::ValueType;
    sum.Fill(NumericTraits<PixelComponentType>::Zero);

    for (const InputPixelType pixelValue : neighborhoodRange)
    {
      sum += pixelValue;
    }

    sum /= neighborhoodSize;

    // The following line is an implicit conversion from the
    // InputRealType to the output pixel. If an explicit construction
    // or static_cast was used a new VariableLengthVector temporary would be
    // constructed requiring the array to be dynamic allocated. This
    // implicit assignment reuses the array allocated in the variable out.
    // *DO NOT USE static_cast*
    out = sum;
    *outputIterator = out;
    ++outputIterator;
  }
}


} // end namespace itk

#endif
