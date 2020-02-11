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

#ifndef itkBufferedImageNeighborhoodPixelAccessPolicy_h
#define itkBufferedImageNeighborhoodPixelAccessPolicy_h

#include "itkIndex.h"
#include "itkOffset.h"
#include "itkSize.h"

#include <cassert>

namespace itk
{
namespace Experimental
{

/**
 * \class BufferedImageNeighborhoodPixelAccessPolicy
 * ImageNeighborhoodPixelAccessPolicy class for ShapedImageNeighborhoodRange.
 * Allows getting and setting the value of a pixel, located in a specified
 * neighborhood location, at a specified offset. Assumes that the pixel index
 * is within the buffered region.
 *
 * \note This policy offers faster pixel access than other pixel access policies,
 * because it does not do any bounds checking or border extrapolation. An
 * attempt to access pixels outside the buffered region will have undefined behavior.
 *
 * \author Niels Dekker, LKEB, Leiden University Medical Center
 *
 * \see ShapedNeighborhoodIterator
 * \see ConstantBoundaryImageNeighborhoodPixelAccessPolicy
 * \see ZeroFluxNeumannImageNeighborhoodPixelAccessPolicy
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <typename TImage>
class BufferedImageNeighborhoodPixelAccessPolicy final
{
private:
  using NeighborhoodAccessorFunctorType = typename TImage::NeighborhoodAccessorFunctorType;
  using PixelType = typename TImage::PixelType;
  using InternalPixelType = typename TImage::InternalPixelType;

  using ImageDimensionType = typename TImage::ImageDimensionType;
  static constexpr ImageDimensionType ImageDimension = TImage::ImageDimension;

  using IndexType = Index<ImageDimension>;
  using OffsetType = Offset<ImageDimension>;
  using ImageSizeType = Size<ImageDimension>;

  // Index value to the image buffer, indexing the current pixel.
  const IndexValueType m_PixelIndexValue;

  // A reference to the accessor of the image.
  const NeighborhoodAccessorFunctorType & m_NeighborhoodAccessor;


  // Private helper function. Calculates and returns the index value of the
  // current pixel within the image buffer.
  static IndexValueType
  CalculatePixelIndexValue(const ImageSizeType &
#if !defined(NDEBUG)
                             // Parameter that is only needed for debug assert.
                             imageSize
#endif
                           ,
                           const OffsetType & offsetTable,
                           const IndexType &  pixelIndex) ITK_NOEXCEPT
  {
    IndexValueType result = 0;

    for (ImageDimensionType i = 0; i < ImageDimension; ++i)
    {
      const auto pixelIndexValue = pixelIndex[i];
      assert((pixelIndexValue >= 0) && (static_cast<SizeValueType>(pixelIndexValue) < imageSize[i]));
      result += pixelIndexValue * offsetTable[i];
    }
    return result;
  }

public:
  // Deleted member functions:
  BufferedImageNeighborhoodPixelAccessPolicy() = delete;
  BufferedImageNeighborhoodPixelAccessPolicy &
  operator=(const BufferedImageNeighborhoodPixelAccessPolicy &) = delete;

  // Explicitly-defaulted functions:
  ~BufferedImageNeighborhoodPixelAccessPolicy() = default;
  BufferedImageNeighborhoodPixelAccessPolicy(const BufferedImageNeighborhoodPixelAccessPolicy &) ITK_NOEXCEPT = default;

  /** Constructor called directly by the pixel proxy of ShapedImageNeighborhoodRange.
   * \note The parameter `pixelIndex` is assumed to be in the buffered region. */
  BufferedImageNeighborhoodPixelAccessPolicy(const ImageSizeType &                   imageSize,
                                             const OffsetType &                      offsetTable,
                                             const NeighborhoodAccessorFunctorType & neighborhoodAccessor,
                                             const IndexType &                       pixelIndex) ITK_NOEXCEPT
    : m_PixelIndexValue{ CalculatePixelIndexValue(imageSize, offsetTable, pixelIndex) }
    , m_NeighborhoodAccessor(neighborhoodAccessor)
  {}

  /** Retrieves the pixel value from the image buffer, at the current
   * index value.  */
  PixelType
  GetPixelValue(const InternalPixelType * const imageBufferPointer) const ITK_NOEXCEPT
  {
    return m_NeighborhoodAccessor.Get(imageBufferPointer + m_PixelIndexValue);
  }

  /** Sets the value of the image buffer at the current index value to the
   * specified value.  */
  void
  SetPixelValue(InternalPixelType * const imageBufferPointer, const PixelType & pixelValue) const ITK_NOEXCEPT
  {
    m_NeighborhoodAccessor.Set(imageBufferPointer + m_PixelIndexValue, pixelValue);
  }
};

} // namespace Experimental
} // namespace itk

#endif
