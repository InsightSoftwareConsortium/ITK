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

// First include the header file to be tested:
#include "itkImageNeighborhoodOffsets.h"

#include "itkOffset.h"
#include "itkSize.h"

#include <array>
#include <vector>

#include <gtest/gtest.h>


TEST(ImageNeighborhoodOffsets, GenerateHyperrectangularImageNeighborhoodOffsets_returns_one_offset_for_default_radius)
{
  const itk::Size<> radius = {};
  const std::vector<itk::Offset<>> offsets = itk::Experimental::GenerateHyperrectangularImageNeighborhoodOffsets(radius);

  EXPECT_EQ(offsets, std::vector<itk::Offset<>>(1));
}


TEST(ImageNeighborhoodOffsets, GenerateHyperrectangularImageNeighborhoodOffsets_for_smallest_horizontal_neigborhood)
{
  const itk::Size<> radius = { { 1, 0 } };
  const std::vector<itk::Offset<>> offsets = itk::Experimental::GenerateHyperrectangularImageNeighborhoodOffsets(radius);

  EXPECT_EQ(offsets, (std::vector<itk::Offset<>>{ { { -1, 0 }}, { { 0, 0 } }, { { 1, 0 } } }));
}


TEST(ImageNeighborhoodOffsets, GenerateHyperrectangularImageNeighborhoodOffsets_for_smallest_vertical_neigborhood)
{
  const itk::Size<> radius = { { 0, 1 } };
  const std::vector<itk::Offset<>> offsets = itk::Experimental::GenerateHyperrectangularImageNeighborhoodOffsets(radius);

  EXPECT_EQ(offsets, (std::vector<itk::Offset<>>{ { { 0, -1 }}, { { 0, 0 } }, { { 0, 1 } } }));
}


TEST(ImageNeighborhoodOffsets, FillHyperrectangularImageNeighborhoodOffsets_allows_using_std_array)
{
  const itk::Size<> radius = { { 0, 1 } };
  std::array<itk::Offset<>, 3> offsets;

  itk::Experimental::FillHyperrectangularImageNeighborhoodOffsets(offsets.data(), offsets.size(), radius);

  EXPECT_TRUE(std::equal(offsets.cbegin(), offsets.cend(),
    itk::Experimental::GenerateHyperrectangularImageNeighborhoodOffsets(radius).cbegin()));
}
