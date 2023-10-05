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

// First include the header file to be tested:
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkImage.h"
#include "itkImageBufferRange.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"

#include <gtest/gtest.h> // For iota.

#include <numeric>


// Tests that ImageRandomConstIteratorWithIndex is deterministic, when the initial seed and the next seed of the global
// MersenneTwisterRandomVariateGenerator are reset before creating the iterator.
TEST(ImageRandomConstIteratorWithIndex, IsDeterministicWhenGlobalRandomSeedsAreReset)
{
  using PixelType = int;
  const unsigned int Dimension{ 2 };
  using ImageType = itk::Image<PixelType, Dimension>;

  // Create just a small test image, but still having enough pixels for the iterator to pick different samples from.
  const auto                        image = ImageType::New();
  const itk::ImageRegion<Dimension> imageRegion(ImageType::SizeType::Filled(3));
  image->SetRegions(imageRegion);
  image->Allocate();

  // Make sure that each pixel has a different value.
  const itk::ImageBufferRange imageBufferRange(*image);
  std::iota(imageBufferRange.begin(), imageBufferRange.end(), 0);

  using itk::Statistics::MersenneTwisterRandomVariateGenerator;

  for (const MersenneTwisterRandomVariateGenerator::IntegerType initialSeed : { 0, 1 })
  {
    const auto generateSamples = [initialSeed, image]() {
      // Reset both the initial seed and the next seed of the global random variate generator.
      MersenneTwisterRandomVariateGenerator::GetInstance()->SetSeed(initialSeed);
      MersenneTwisterRandomVariateGenerator::ResetNextSeed();

      itk::ImageRandomConstIteratorWithIndex<ImageType> iterator(image, image->GetRequestedRegion());

      // Request just a small number of samples, for performance reasons. Note that even for this small number of
      // samples, it is important to have the global seeds reset, in order to have a deterministic iteration.
      iterator.SetNumberOfSamples(3);

      std::vector<std::pair<PixelType, ImageType::IndexType>> samples{};

      while (!iterator.IsAtEnd())
      {
        samples.push_back({ iterator.Get(), iterator.GetIndex() });
        ++iterator;
      }
      return samples;
    };

    // Expect the same samples when generating them twice with the same initial seed.
    EXPECT_EQ(generateSamples(), generateSamples());
  }
}
