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
#ifndef itkImageAlgorithm_hxx
#define itkImageAlgorithm_hxx

#include "itkImageAlgorithm.h"
#include "itkArray.h"
#include "itkImageRegionIterator.h"
#include "itkImageScanlineIterator.h"


namespace itk
{

template <typename InputImageType, typename OutputImageType>
void
ImageAlgorithm::DispatchedCopy(const InputImageType *                       inImage,
                               OutputImageType *                            outImage,
                               const typename InputImageType::RegionType &  inRegion,
                               const typename OutputImageType::RegionType & outRegion,
                               FalseType)
{
  if (inRegion.GetSize()[0] == outRegion.GetSize()[0])
  {
    itk::ImageScanlineConstIterator<InputImageType> it(inImage, inRegion);
    itk::ImageScanlineIterator<OutputImageType>     ot(outImage, outRegion);

    while (!it.IsAtEnd())
    {
      while (!it.IsAtEndOfLine())
      {
        ot.Set(static_cast<typename OutputImageType::PixelType>(it.Get()));
        ++ot;
        ++it;
      }
      ot.NextLine();
      it.NextLine();
    }
    return;
  }

  itk::ImageRegionConstIterator<InputImageType> it(inImage, inRegion);
  itk::ImageRegionIterator<OutputImageType>     ot(outImage, outRegion);

  while (!it.IsAtEnd())
  {
    ot.Set(static_cast<typename OutputImageType::PixelType>(it.Get()));
    ++ot;
    ++it;
  }
}

template <typename InputImageType, typename OutputImageType>
void
ImageAlgorithm::DispatchedCopy(const InputImageType *                       inImage,
                               OutputImageType *                            outImage,
                               const typename InputImageType::RegionType &  inRegion,
                               const typename OutputImageType::RegionType & outRegion,
                               TrueType)
{
  using _RegionType = typename InputImageType::RegionType;
  using _IndexType = typename InputImageType::IndexType;

  // Get the number of bytes of each pixel in the buffer.
  const size_t NumberOfInternalComponents = ImageAlgorithm::PixelSize<InputImageType>::Get(inImage);

  // We wish to copy whole lines, otherwise just use the basic implementation.
  // Check that the number of internal components match
  if (inRegion.GetSize()[0] != outRegion.GetSize()[0] ||
      NumberOfInternalComponents != ImageAlgorithm::PixelSize<OutputImageType>::Get(outImage))
  {
    ImageAlgorithm::DispatchedCopy<InputImageType, OutputImageType>(inImage, outImage, inRegion, outRegion);
    return;
  }

  const typename InputImageType::InternalPixelType * in = inImage->GetBufferPointer();
  typename OutputImageType::InternalPixelType *      out = outImage->GetBufferPointer();

  const _RegionType & inBufferedRegion = inImage->GetBufferedRegion();
  const _RegionType & outBufferedRegion = outImage->GetBufferedRegion();

  // Compute the number of continuous pixel which can be copied.
  size_t       numberOfPixel = 1;
  unsigned int movingDirection = 0;
  do
  {
    numberOfPixel *= inRegion.GetSize(movingDirection);
    ++movingDirection;
  }
  // The copy regions must extend to the full buffered region, to
  // ensure continuity of pixels between dimensions.
  while (movingDirection < _RegionType::ImageDimension &&
         inRegion.GetSize(movingDirection - 1) == inBufferedRegion.GetSize(movingDirection - 1) &&
         outRegion.GetSize(movingDirection - 1) == outBufferedRegion.GetSize(movingDirection - 1) &&
         inBufferedRegion.GetSize(movingDirection - 1) == outBufferedRegion.GetSize(movingDirection - 1));

  const size_t sizeOfChunkInInternalComponents = numberOfPixel * NumberOfInternalComponents;

  _IndexType inCurrentIndex = inRegion.GetIndex();
  _IndexType outCurrentIndex = outRegion.GetIndex();

  while (inRegion.IsInside(inCurrentIndex))
  {
    size_t inOffset = 0; // in pixels
    size_t outOffset = 0;

    size_t inSubDimensionQuantity = 1; // in pixels
    size_t outSubDimensionQuantity = 1;

    for (unsigned int i = 0; i < _RegionType::ImageDimension; ++i)
    {
      inOffset += inSubDimensionQuantity * static_cast<size_t>(inCurrentIndex[i] - inBufferedRegion.GetIndex(i));
      inSubDimensionQuantity *= inBufferedRegion.GetSize(i);

      outOffset += outSubDimensionQuantity * static_cast<size_t>(outCurrentIndex[i] - outBufferedRegion.GetIndex(i));
      outSubDimensionQuantity *= outBufferedRegion.GetSize(i);
    }

    const typename InputImageType::InternalPixelType * inBuffer = in + inOffset * NumberOfInternalComponents;
    typename OutputImageType::InternalPixelType *      outBuffer = out + outOffset * NumberOfInternalComponents;

    CopyHelper(inBuffer, inBuffer + sizeOfChunkInInternalComponents, outBuffer);

    if (movingDirection == _RegionType::ImageDimension)
    {
      break;
    }

    // increment index to next chunk
    ++inCurrentIndex[movingDirection];
    for (unsigned int i = movingDirection; i + 1 < _RegionType::ImageDimension; ++i)
    {
      // When reaching the end of the moving index in the copy region
      // dimension, carry to higher dimensions.
      if (static_cast<SizeValueType>(inCurrentIndex[i] - inRegion.GetIndex(i)) >= inRegion.GetSize(i))
      {
        inCurrentIndex[i] = inRegion.GetIndex(i);
        ++inCurrentIndex[i + 1];
      }
    }

    // increment index to next chunk
    ++outCurrentIndex[movingDirection];
    for (unsigned int i = movingDirection; i + 1 < _RegionType::ImageDimension; ++i)
    {
      if (static_cast<SizeValueType>(outCurrentIndex[i] - outRegion.GetIndex(i)) >= outRegion.GetSize(i))
      {
        outCurrentIndex[i] = outRegion.GetIndex(i);
        ++outCurrentIndex[i + 1];
      }
    }
  }
}


template <typename InputImageType, typename OutputImageType>
typename OutputImageType::RegionType
ImageAlgorithm::EnlargeRegionOverBox(const typename InputImageType::RegionType & inputRegion,
                                     const InputImageType *                      inputImage,
                                     const OutputImageType *                     outputImage)
{
  class DummyTransform
  {
  public:
    using PointType = Point<SpacePrecisionType, OutputImageType::ImageDimension>;
    PointType
    TransformPoint(const PointType & p) const
    {
      return p;
    }
  };
  return EnlargeRegionOverBox<InputImageType, OutputImageType, DummyTransform>(
    inputRegion, inputImage, outputImage, nullptr);
}

template <typename InputImageType, typename OutputImageType, typename TransformType>
typename OutputImageType::RegionType
ImageAlgorithm::EnlargeRegionOverBox(const typename InputImageType::RegionType & inputRegion,
                                     const InputImageType *                      inputImage,
                                     const OutputImageType *                     outputImage,
                                     const TransformType *                       transform)
{
  typename OutputImageType::RegionType outputRegion;

  // Get the index of the corners of the input region, map them to input physical space.
  // Convert input physical corners to output physical corners using transform
  // and finally map output physical corners to index of output image.
  // The input region has 2^ImageDimension corners, each
  // of which is either on the inferior or superior edge
  // along each dimension.
  unsigned int numberOfInputCorners = 1;
  for (unsigned int dim = 0; dim < InputImageType::ImageDimension; ++dim)
  {
    numberOfInputCorners *= 2;
  }
  using ContinuousInputIndexType = ContinuousIndex<double, InputImageType::ImageDimension>;
  using ContinuousOutputIndexType = ContinuousIndex<double, OutputImageType::ImageDimension>;

  std::vector<ContinuousOutputIndexType> outputCorners(numberOfInputCorners);


  for (unsigned int count = 0; count < numberOfInputCorners; ++count)
  {
    ContinuousInputIndexType currentInputCornerIndex;
    currentInputCornerIndex.Fill(0);
    unsigned int localCount = count;

    // For each dimension, set the current index to either
    // the highest or lowest index along this dimension.
    // Since we need all the space covered by the input image to
    // be taken into account, including the half-pixel border,
    // we start half a pixel before index 0 and stop half a pixel
    // after size
    for (unsigned int dim = 0; dim < InputImageType::ImageDimension; ++dim)
    {
      if (localCount % 2)
      {
        currentInputCornerIndex[dim] = inputRegion.GetIndex(dim) + inputRegion.GetSize(dim) + 0.5;
      }
      else
      {
        currentInputCornerIndex[dim] = inputRegion.GetIndex(dim) - 0.5;
      }

      localCount /= 2;
    }

    using InputPointType = Point<SpacePrecisionType, InputImageType::ImageDimension>;
    using OutputPointType = Point<SpacePrecisionType, OutputImageType::ImageDimension>;
    InputPointType  inputPoint;
    OutputPointType outputPoint;
    inputImage->TransformContinuousIndexToPhysicalPoint(currentInputCornerIndex, inputPoint);
    if (transform != nullptr)
    {
      outputPoint = transform->TransformPoint(inputPoint);
    }
    else
    {
      // if InputDimension < OutputDimension then embed point in Output space.
      // else if InputDimension == OutputDimension copy the points.
      // else if InputDimension > OutputDimension project the point to first N-Dimensions of Output space.
      outputPoint.Fill(0.0);
      for (unsigned d = 0; d < std::min(inputPoint.GetPointDimension(), outputPoint.GetPointDimension()); ++d)
      {
        outputPoint[d] = inputPoint[d];
      }
    }
    outputImage->TransformPhysicalPointToContinuousIndex(outputPoint, outputCorners[count]);
  }

  // Compute a rectangular region from the vector of corner indexes
  for (unsigned int dim = 0; dim < OutputImageType::ImageDimension; ++dim)
  {
    // Initialize index to the highest possible value
    outputRegion.SetIndex(dim, NumericTraits<IndexValueType>::max());

    // For each dimension, set the output index to the minimum
    // of the corners' indexes, and the output size to their maximum
    for (unsigned int count = 0; count < numberOfInputCorners; ++count)
    {
      auto continuousIndexFloor = Math::Floor<IndexValueType>(outputCorners[count][dim]);
      if (continuousIndexFloor < outputRegion.GetIndex(dim))
      {
        outputRegion.SetIndex(dim, continuousIndexFloor);
      }
      auto continuousIndexCeil = Math::Ceil<IndexValueType>(outputCorners[count][dim]);
      if (continuousIndexCeil > static_cast<IndexValueType>(outputRegion.GetSize(dim)))
      {
        outputRegion.SetSize(dim, continuousIndexCeil);
      }
    }

    // The size is actually the difference between maximum and minimum index,
    // so subtract the index
    outputRegion.SetSize(dim, outputRegion.GetSize(dim) - outputRegion.GetIndex(dim));
  }

  // Make sure this region remains contained in the LargestPossibleRegion
  outputRegion.Crop(outputImage->GetLargestPossibleRegion());
  return outputRegion;
}

} // end namespace itk

#endif
