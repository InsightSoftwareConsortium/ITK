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

#ifndef itkRectangularImageNeighborhoodShape_h
#define itkRectangularImageNeighborhoodShape_h

#include <algorithm> // For transform.
#include <cassert>

#include "itkOffset.h"
#include "itkSize.h"

namespace itk
{
namespace Experimental
{

/**
 * \class RectangularImageNeighborhoodShape
 * Rectangular (or hyperrectangular) image-neighborhood shape.
 * Eases creating a sequence of offsets for ShapedImageNeighborhoodRange.
 * Can also be used for ShapedNeighborhoodIterator.
 *
 * The following example creates a 3 x 5 rectangular neighborhood around
 * pixel location [10, 20], and generates the offsets for a neighborhood range:
 \code
  const Index<> location = { 10, 20 };
  const Size<> radius = { { 1, 2 } };
  const RectangularImageNeighborhoodShape<2> shape{ radius };
  const std::vector<Offset<>> offsets = GenerateImageNeighborhoodOffsets(shape);
  ShapedImageNeighborhoodRange<ImageType> neighborhoodRange{ *image, location, offsets };
 \endcode
 *
 * \see ShapedNeighborhoodIterator
 * \see ShapedImageNeighborhoodRange
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <unsigned int VImageDimension>
class RectangularImageNeighborhoodShape
{
public:
  static constexpr unsigned int ImageDimension = VImageDimension;

  /** Constructs a hyperrectangular shape whose size is specified by the radius */
  constexpr explicit RectangularImageNeighborhoodShape(const Size<ImageDimension> & radius) ITK_NOEXCEPT
    : m_Radius(radius)
    , m_NumberOfOffsets(CalculateNumberOfOffsets(ImageDimension))
  {}


  /** Returns the number of offsets needed to represent this shape. */
  constexpr std::size_t
  GetNumberOfOffsets() const ITK_NOEXCEPT
  {
    return m_NumberOfOffsets;
  }


  /** Fills the specified buffer with the offsets for a neighborhood of this shape. */
  void
  FillOffsets(Offset<ImageDimension> * const offsets) const ITK_NOEXCEPT
  {
    if (m_NumberOfOffsets > 0)
    {
      assert(offsets != nullptr);
      Offset<ImageDimension> offset;

      std::transform(m_Radius.begin(), m_Radius.end(), offset.begin(), [](const SizeValueType radiusValue) {
        return -static_cast<OffsetValueType>(radiusValue);
      });

      for (std::size_t i = 0; i < m_NumberOfOffsets; ++i)
      {
        offsets[i] = offset;

        for (unsigned dimensionIndex = 0; dimensionIndex < ImageDimension; ++dimensionIndex)
        {
          OffsetValueType & offsetValue = offset[dimensionIndex];

          ++offsetValue;

          if (offsetValue <= static_cast<OffsetValueType>(m_Radius[dimensionIndex]))
          {
            break;
          }
          offsetValue = -static_cast<OffsetValueType>(m_Radius[dimensionIndex]);
        }
      }
    }
  }

private:
  // The radius of the neighborhood along each direction.
  Size<ImageDimension> m_Radius;

  // The number of offsets needed to represent this shape.
  std::size_t m_NumberOfOffsets;


  // Private helper function to calculate the number of Offsets by a recursive
  // function call. Recursion is necessary for C++11 constexpr.
  constexpr std::size_t
  CalculateNumberOfOffsets(const unsigned dimension) const ITK_NOEXCEPT
  {
    return (dimension == 0)
             ? 1
             : (2 * m_Radius.m_InternalArray[dimension - 1] + 1) * CalculateNumberOfOffsets(dimension - 1);
  }
};

} // namespace Experimental
} // namespace itk

#endif
