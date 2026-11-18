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
#include "itkPoolMultiThreader.h"
#include "itkCommand.h"
#include "itkImage.h"
#include "itkImageSource.h"
#include "itkImageSourceCommon.h"
#include <gtest/gtest.h>

#include <algorithm>
#include <atomic>
#include <chrono>
#include <stdexcept>
#include <thread>
#include <vector>

namespace
{
constexpr unsigned int numberOfWorkUnits = 8;

class ScopedGlobalDefaultThreader
{
public:
  explicit ScopedGlobalDefaultThreader(const itk::MultiThreaderBase::ThreaderEnum threader)
    : m_Previous(itk::MultiThreaderBase::GetGlobalDefaultThreader())
  {
    itk::MultiThreaderBase::SetGlobalDefaultThreader(threader);
  }

  ~ScopedGlobalDefaultThreader() { itk::MultiThreaderBase::SetGlobalDefaultThreader(m_Previous); }

  ScopedGlobalDefaultThreader(const ScopedGlobalDefaultThreader &) = delete;
  ScopedGlobalDefaultThreader &
  operator=(const ScopedGlobalDefaultThreader &) = delete;

private:
  itk::MultiThreaderBase::ThreaderEnum m_Previous;
};


// The work units of a ParallelizeImageRegion call may refer to the caller's index and size arrays, so they must all
// have finished when the call ends, also when it ends with an exception.
void
ExpectAllWorkUnitsToFinishBeforeThrowing(const bool throwInCallersShare)
{
  const auto threader = itk::PoolMultiThreader::New();
  threader->SetNumberOfWorkUnits(numberOfWorkUnits);

  std::vector<itk::IndexValueType> index{ 10, 20 };
  std::vector<itk::SizeValueType>  size{ 64, 64 };

  const itk::ImageIORegion region(2, index.data(), size.data());
  const auto               expectedUnits =
    itk::ImageSourceCommon::GetGlobalDefaultSplitter()->GetNumberOfSplits(region, numberOfWorkUnits);
  ASSERT_GT(expectedUnits, 1u);

  const auto        callersThread = std::this_thread::get_id();
  std::atomic<int>  started{ 0 };
  std::atomic<int>  finished{ 0 };
  std::atomic<int>  outOfBounds{ 0 };
  std::atomic<bool> thrown{ false };

  const auto funcP = [&](const itk::IndexValueType * i, const itk::SizeValueType * s) {
    ++started;
    if (i[0] < 10 || i[0] + static_cast<itk::IndexValueType>(s[0]) > 74 || i[1] < 20 ||
        i[1] + static_cast<itk::IndexValueType>(s[1]) > 84)
    {
      ++outOfBounds;
    }
    const bool onCallersThread = std::this_thread::get_id() == callersThread;
    if (onCallersThread == throwInCallersShare && !thrown.exchange(true))
    {
      throw std::runtime_error("Expected exception");
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(30));
    ++finished;
  };

  EXPECT_THROW(threader->ParallelizeImageRegion(2, index.data(), size.data(), funcP, nullptr), std::runtime_error);

  // Whatever the work units may have referred to is not needed anymore.
  std::fill(index.begin(), index.end(), itk::IndexValueType{ -1000 });
  std::fill(size.begin(), size.end(), itk::SizeValueType{ 1 });

  EXPECT_EQ(started, static_cast<int>(expectedUnits));
  EXPECT_EQ(finished, static_cast<int>(expectedUnits) - 1); // all but the one that threw
  EXPECT_EQ(outOfBounds, 0);
}


// The work units of this source are slow, except the one on the thread that calls Update(). The work unit that is
// waited for first is the slowest.
class SlowImageSource : public itk::ImageSource<itk::Image<float, 2>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SlowImageSource);

  using Self = SlowImageSource;
  using Superclass = itk::ImageSource<itk::Image<float, 2>>;
  using Pointer = itk::SmartPointer<Self>;

  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(SlowImageSource);

  [[nodiscard]] int
  GetNumberOfStartedWorkUnits() const
  {
    return m_Started;
  }

  [[nodiscard]] int
  GetNumberOfFinishedWorkUnits() const
  {
    return m_Finished;
  }

protected:
  SlowImageSource() = default;
  ~SlowImageSource() override = default;

  void
  GenerateOutputInformation() override
  {
    this->GetOutput()->SetLargestPossibleRegion(OutputImageRegionType(itk::Size<2>{ { 64, 64 } }));
  }

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override
  {
    ++m_Started;
    if (std::this_thread::get_id() != m_UpdateThread)
    {
      const auto remainingRows = 64 - outputRegionForThread.GetIndex(1);
      std::this_thread::sleep_for(std::chrono::milliseconds(20 + 4 * remainingRows));
    }
    ++m_Finished;
  }

private:
  const std::thread::id m_UpdateThread{ std::this_thread::get_id() };
  std::atomic<int>      m_Started{ 0 };
  std::atomic<int>      m_Finished{ 0 };
};


struct ObserverData
{
  const SlowImageSource * Source;
  std::atomic<int>        EventsWhileRunning{ 0 };
  std::atomic<bool>       Thrown{ false };
};


// The first such event may be this thread's own progress report. The later ones are reported while waiting.
void
ThrowOnSecondEventWhileWorkUnitsAreRunning(itk::Object *, const itk::EventObject &, void * clientData)
{
  auto & data = *static_cast<ObserverData *>(clientData);
  if (data.Source->GetNumberOfFinishedWorkUnits() < data.Source->GetNumberOfStartedWorkUnits() &&
      ++data.EventsWhileRunning == 2)
  {
    data.Thrown = true;
    throw std::runtime_error("Expected exception from a progress observer");
  }
}
} // namespace


TEST(PoolMultiThreader, ParallelizeImageRegionWaitsForAllWorkUnitsWhenThisThreadsShareThrows)
{
  ExpectAllWorkUnitsToFinishBeforeThrowing(true);
}


TEST(PoolMultiThreader, ParallelizeImageRegionWaitsForAllWorkUnitsWhenAnotherWorkUnitThrows)
{
  ExpectAllWorkUnitsToFinishBeforeThrowing(false);
}


TEST(PoolMultiThreader, ParallelizeImageRegionKeepsWaitingWhenReportingProgressThrows)
{
  const ScopedGlobalDefaultThreader scopedThreader(itk::MultiThreaderBase::ThreaderEnum::Pool);

  const auto source = SlowImageSource::New();
  source->SetNumberOfWorkUnits(numberOfWorkUnits);

  ObserverData observerData{ source.GetPointer() };
  const auto   command = itk::CStyleCommand::New();
  command->SetCallback(ThrowOnSecondEventWhileWorkUnitsAreRunning);
  command->SetClientData(&observerData);
  source->AddObserver(itk::ProgressEvent(), command);

  EXPECT_THROW(source->Update(), std::runtime_error);

  EXPECT_TRUE(observerData.Thrown);
  EXPECT_EQ(source->GetNumberOfFinishedWorkUnits(), source->GetNumberOfStartedWorkUnits());
  EXPECT_EQ(source->GetNumberOfStartedWorkUnits(), static_cast<int>(numberOfWorkUnits));
}
