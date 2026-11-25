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

#include "itkImageToNeighborhoodSampleAdaptor.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkUniformRandomSpatialNeighborSubsampler.h"
#include "itkGTest.h"

#include <chrono>
#include <future>
#include <thread>

namespace
{
using ImageType = itk::Image<float, 2>;
using RegionType = ImageType::RegionType;
using BoundaryCondition = itk::ZeroFluxNeumannBoundaryCondition<ImageType>;
using AdaptorType = itk::Statistics::ImageToNeighborhoodSampleAdaptor<ImageType, BoundaryCondition>;
using SamplerType = itk::Statistics::UniformRandomSpatialNeighborSubsampler<AdaptorType, RegionType>;
} // namespace

TEST(UniformRandomSpatialNeighborSubsampler, SearchTerminatesWhenOnlyQueryPointIsSelectable)
{
  constexpr ImageType::SizeType::value_type regionSizeVal = 5;
  constexpr auto                            sz = ImageType::SizeType::Filled(regionSizeVal);
  const RegionType                          region{ sz };

  auto image = ImageType::New();
  image->SetRegions(region);
  image->AllocateInitialized();

  auto sample = AdaptorType::New();
  sample->SetImage(image);

  auto sampler = SamplerType::New();
  sampler->SetSample(sample);
  sampler->SetSampleRegion(region);
  sampler->SetRadius(0);
  sampler->CanSelectQueryOff();
  sampler->SetNumberOfResultsRequested(1);

  const SamplerType::InstanceIdentifier query = 12; // interior point (2, 2)
  SamplerType::SubsamplePointer         results = SamplerType::SubsampleType::New();

  auto              done = std::make_shared<std::promise<void>>();
  std::future<void> future = done->get_future();
  std::thread       worker([sampler, query, results, done]() mutable {
    sampler->Search(query, results);
    done->set_value();
  });

  const bool finished = (future.wait_for(std::chrono::seconds(30)) == std::future_status::ready);
  if (finished)
  {
    worker.join();
  }
  else
  {
    worker.detach();
  }
  ASSERT_TRUE(finished) << "Search did not terminate for a single-point search region";
  EXPECT_EQ(results->GetTotalFrequency(), 0u);
}
