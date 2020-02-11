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

// First include the header file to be tested:
#include "itkImageNeighborhoodOffsets.h"

#include "itkOffset.h"
#include "itkSize.h"

#include <array>
#include <vector>

#include <gtest/gtest.h>

namespace
{

// Empty shape, just for test purposes. Compatible with the template argument
// of GenerateImageNeighborhoodOffsets<ImageNeighborhoodShape>(shape).
template <unsigned int VImageDimension>
class EmptyImageNeighborhoodShape
{
public:
  static constexpr unsigned int ImageDimension = VImageDimension;

  // Returns the number of offsets needed to represent this shape.
  constexpr std::size_t
  GetNumberOfOffsets() const ITK_NOEXCEPT
  {
    return 0;
  }

  // Fills the specified buffer with the offsets for this shape.
  void
  FillOffsets(itk::Offset<ImageDimension> *) const ITK_NOEXCEPT
  {
    // The shape is empty, so just do nothing!
  }
};

} // namespace


TEST(ImageNeighborhoodOffsets, GenerateImageNeighborhoodOffsetsReturnsEmptyVectorForEmptyShape)
{
  constexpr unsigned int                            ImageDimension = 2;
  const EmptyImageNeighborhoodShape<ImageDimension> shape = {};
  const std::vector<itk::Offset<>> offsets = itk::Experimental::GenerateImageNeighborhoodOffsets(shape);

  EXPECT_EQ(offsets, std::vector<itk::Offset<>>());
}


TEST(ImageNeighborhoodOffsets, GenerateRectangularImageNeighborhoodOffsetsReturnsOneOffsetForDefaultRadius)
{
  const itk::Size<>                radius = { {} };
  const std::vector<itk::Offset<>> offsets = itk::Experimental::GenerateRectangularImageNeighborhoodOffsets(radius);

  EXPECT_EQ(offsets, std::vector<itk::Offset<>>(1));
}


TEST(ImageNeighborhoodOffsets, GenerateRectangularImageNeighborhoodOffsetsForSmallestHorizontalNeigborhood)
{
  const itk::Size<>                radius = { { 1, 0 } };
  const std::vector<itk::Offset<>> offsets = itk::Experimental::GenerateRectangularImageNeighborhoodOffsets(radius);

  EXPECT_EQ(offsets, (std::vector<itk::Offset<>>{ { { -1, 0 } }, { { 0, 0 } }, { { 1, 0 } } }));
}


TEST(ImageNeighborhoodOffsets, GenerateRectangularImageNeighborhoodOffsetsForSmallestVerticalNeigborhood)
{
  const itk::Size<>                radius = { { 0, 1 } };
  const std::vector<itk::Offset<>> offsets = itk::Experimental::GenerateRectangularImageNeighborhoodOffsets(radius);

  EXPECT_EQ(offsets, (std::vector<itk::Offset<>>{ { { 0, -1 } }, { { 0, 0 } }, { { 0, 1 } } }));
}
