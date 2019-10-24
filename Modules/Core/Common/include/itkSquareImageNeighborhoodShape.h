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

#ifndef itkSquareImageNeighborhoodShape_h
#define itkSquareImageNeighborhoodShape_h

#include <algorithm> // For transform.
#include <cassert>

#include "itkOffset.h"
#include "itkSize.h"

namespace itk
{
namespace Experimental
{

/**
 * \class SquareImageNeighborhoodShape
 * Square (or n-dimensional hypercubic) image-neighborhood shape.
 * Eases creating a sequence of offsets for ShapedImageNeighborhoodRange.
 * Can also be used for ShapedNeighborhoodIterator.
 *
 * The following example creates a 7 x 7 square neighborhood around
 * pixel location [10, 20], and generates the offsets for a neighborhood range:
 \code
  const Index<> location = { 10, 20 };
  const std::size_t radius = 3;
  const SquareImageNeighborhoodShape<2> shape{ radius };
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
class SquareImageNeighborhoodShape
{
public:
  static constexpr unsigned int ImageDimension = VImageDimension;

  /** Constructs a square (or hypercubic) shape whose size is specified by the radius */
  constexpr explicit SquareImageNeighborhoodShape(const std::size_t radius) ITK_NOEXCEPT
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

      offset.Fill(-static_cast<OffsetValueType>(m_Radius));

      for (std::size_t i = 0; i < m_NumberOfOffsets; ++i)
      {
        offsets[i] = offset;

        for (unsigned dimensionIndex = 0; dimensionIndex < ImageDimension; ++dimensionIndex)
        {
          OffsetValueType & offsetValue = offset[dimensionIndex];

          ++offsetValue;

          if (offsetValue <= static_cast<OffsetValueType>(m_Radius))
          {
            break;
          }
          offsetValue = -static_cast<OffsetValueType>(m_Radius);
        }
      }
    }
  }

private:
  // The radius of the neighborhood along each direction.
  std::size_t m_Radius;

  // The number of offsets needed to represent this shape.
  std::size_t m_NumberOfOffsets;


  // Private helper function to calculate the number of Offsets by a recursive
  // function call. Recursion is necessary for C++11 constexpr.
  constexpr std::size_t
  CalculateNumberOfOffsets(const unsigned dimension) const ITK_NOEXCEPT
  {
    return (dimension == 0) ? 1 : (2 * m_Radius + 1) * CalculateNumberOfOffsets(dimension - 1);
  }
};

} // namespace Experimental
} // namespace itk

#endif
