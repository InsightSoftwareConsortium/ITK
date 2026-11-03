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

#include "itkConfigure.h"
#include "itkGTest.h"
#include "itkImageRegion.h"

#if defined(ITK_USE_TBB)
#  include "itkNumericTraits.h"
#  include "itkProcessObject.h"
#  include "itkTBBMultiThreader.h"

#  include <algorithm>
#  include <atomic>
#  include <mutex>
#  include <vector>

namespace
{
class AbortableObject : public itk::ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AbortableObject);
  using Self = AbortableObject;
  using Superclass = itk::ProcessObject;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

protected:
  AbortableObject() = default;
};

template <unsigned int VDimension>
struct ChunkStatistics
{
  std::vector<unsigned int>                 visits;
  std::vector<itk::ImageRegion<VDimension>> chunks;
  itk::SizeValueType                        smallest{ itk::NumericTraits<itk::SizeValueType>::max() };
};

template <unsigned int VDimension>
ChunkStatistics<VDimension>
SplitRegion(itk::TBBMultiThreader & threader, const itk::ImageRegion<VDimension> & region)
{
  ChunkStatistics<VDimension> statistics;
  statistics.visits.resize(region.GetNumberOfPixels());
  std::mutex mutex;
  static_cast<itk::MultiThreaderBase &>(threader).template ParallelizeImageRegion<VDimension>(
    region,
    [&](const itk::ImageRegion<VDimension> & chunk) {
      const std::lock_guard<std::mutex> lock(mutex);
      statistics.chunks.push_back(chunk);
      statistics.smallest = std::min(statistics.smallest, chunk.GetNumberOfPixels());
      for (itk::SizeValueType offset = 0; offset < chunk.GetNumberOfPixels(); ++offset)
      {
        itk::SizeValueType remainder = offset;
        itk::SizeValueType linear = 0;
        itk::SizeValueType stride = 1;
        for (unsigned int d = 0; d < VDimension; ++d)
        {
          const auto index =
            chunk.GetIndex(d) - region.GetIndex(d) + static_cast<itk::IndexValueType>(remainder % chunk.GetSize(d));
          remainder /= chunk.GetSize(d);
          linear += static_cast<itk::SizeValueType>(index) * stride;
          stride *= region.GetSize(d);
        }
        ++statistics.visits[linear];
      }
    },
    nullptr);
  return statistics;
}

template <unsigned int VDimension>
void
ExpectEveryPixelVisitedOnce(const ChunkStatistics<VDimension> & statistics)
{
  for (const auto visits : statistics.visits)
  {
    ASSERT_EQ(visits, 1u);
  }
}
} // namespace

TEST(TBBMultiThreader, RowsStayWhole)
{
  auto threader = itk::TBBMultiThreader::New();
  threader->SetMaximumNumberOfThreads(8);
  threader->SetNumberOfWorkUnits(2);

  const itk::ImageRegion<2> region({ { 0, 0 } }, { { 4096, 2 } });
  const auto                statistics = SplitRegion(*threader, region);

  EXPECT_EQ(statistics.chunks.size(), 2u);
  for (const auto & chunk : statistics.chunks)
  {
    EXPECT_EQ(chunk.GetSize(0), region.GetSize(0));
  }
  ExpectEveryPixelVisitedOnce(statistics);
}

TEST(TBBMultiThreader, ChunksAreAboutOnePerWorkUnit)
{
  auto threader = itk::TBBMultiThreader::New();
  threader->SetMaximumNumberOfThreads(8);
  threader->SetNumberOfWorkUnits(4);

  const itk::ImageRegion<3> region({ { 3, -2, 5 } }, { { 64, 64, 64 } });
  const auto                statistics = SplitRegion(*threader, region);

  EXPECT_EQ(statistics.chunks.size(), 4u);
  EXPECT_EQ(statistics.smallest, region.GetNumberOfPixels() / 4);
  ExpectEveryPixelVisitedOnce(statistics);
}

TEST(TBBMultiThreader, UnevenRegionVisitsEveryPixelOnce)
{
  auto threader = itk::TBBMultiThreader::New();
  threader->SetMaximumNumberOfThreads(8);
  threader->SetNumberOfWorkUnits(32);

  const itk::ImageRegion<3> region({ { 3, -2, 5 } }, { { 127, 95, 81 } });
  const auto                statistics = SplitRegion(*threader, region);

  EXPECT_GT(statistics.chunks.size(), 1u);
  EXPECT_LE(statistics.chunks.size(), 2u * 32u);
  ExpectEveryPixelVisitedOnce(statistics);
}

TEST(TBBMultiThreader, OneDimensionalRegionSplitsToWorkUnits)
{
  auto threader = itk::TBBMultiThreader::New();
  threader->SetMaximumNumberOfThreads(8);
  threader->SetNumberOfWorkUnits(4);

  const itk::ImageRegion<1> region({ { 0 } }, { { 4 } });
  const auto                statistics = SplitRegion(*threader, region);

  EXPECT_EQ(statistics.chunks.size(), 4u);
  ExpectEveryPixelVisitedOnce(statistics);
}

TEST(TBBMultiThreader, ParallelizeArrayVisitsEveryIndexOnce)
{
  auto threader = itk::TBBMultiThreader::New();
  threader->SetMaximumNumberOfThreads(8);
  threader->SetNumberOfWorkUnits(16);

  for (const itk::SizeValueType count :
       { itk::SizeValueType{ 2 }, itk::SizeValueType{ 17 }, itk::SizeValueType{ 100003 } })
  {
    std::vector<std::atomic<unsigned int>> visits(count);
    threader->ParallelizeArray(7, 7 + count, [&visits](itk::SizeValueType i) { ++visits[i - 7]; }, nullptr);
    for (const auto & v : visits)
    {
      ASSERT_EQ(v.load(), 1u);
    }
  }
}

TEST(TBBMultiThreader, ParallelizeArrayAbortsWithinAChunk)
{
  auto threader = itk::TBBMultiThreader::New();
  threader->SetMaximumNumberOfThreads(1);
  threader->SetNumberOfWorkUnits(1);
  auto object = AbortableObject::New();

  constexpr itk::SizeValueType count = 100000;
  itk::SizeValueType           calls = 0;
  EXPECT_THROW(threader->ParallelizeArray(
                 0,
                 count,
                 [&](itk::SizeValueType i) {
                   ++calls;
                   if (i == 10)
                   {
                     object->AbortGenerateDataOn();
                   }
                 },
                 object),
               itk::ProcessAborted);
  EXPECT_LT(calls, count / 2);
}
#endif
