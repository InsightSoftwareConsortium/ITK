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

#include "itkTimeStamp.h"
#include "itkMultiThreaderBase.h"
#include "itkGTest.h"

#include <iostream>
#include <memory>
#include <type_traits>

static_assert(std::is_nothrow_default_constructible_v<itk::TimeStamp>, "Check TimeStamp default-constructibility");
static_assert(std::is_trivially_copy_constructible_v<itk::TimeStamp>, "Check TimeStamp copy-constructibility");
static_assert(std::is_trivially_copy_assignable_v<itk::TimeStamp>, "Check TimeStamp copy-assignability");
static_assert(std::is_trivially_destructible_v<itk::TimeStamp>, "Check TimeStamp destructibility");

// A helper struct for the test, the idea is to have one timestamp per thread.
// To ease the writing of the test, we use MultiThreaderBase::SingleMethodExecute
// with an array of timestamps in the shared data.
struct TimeStampTestHelper
{
  std::vector<itk::TimeStamp> timestamps;
  std::vector<unsigned long>  counters;
};

ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
modified_function(void * ptr)
{
  using WorkUnitInfoType = itk::MultiThreaderBase::WorkUnitInfo;

  auto * infoStruct = static_cast<WorkUnitInfoType *>(ptr);

  const itk::ThreadIdType workUnitID = infoStruct->WorkUnitID;

  auto * helper = static_cast<TimeStampTestHelper *>(infoStruct->UserData);

  helper->timestamps[workUnitID].Modified();
  helper->counters[workUnitID]++;

  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

TEST(TimeStamp, TypeTraits)
{
  // static_asserts above verify these at compile time; this test documents them
  EXPECT_TRUE(std::is_nothrow_default_constructible_v<itk::TimeStamp>);
  EXPECT_TRUE(std::is_trivially_copy_constructible_v<itk::TimeStamp>);
  EXPECT_TRUE(std::is_trivially_copy_assignable_v<itk::TimeStamp>);
  EXPECT_TRUE(std::is_trivially_destructible_v<itk::TimeStamp>);
}

TEST(TimeStamp, ThreadSafeMonotonicity)
{
  TimeStampTestHelper helper;

  // Set up the multithreader
  const itk::MultiThreaderBase::Pointer multithreader = itk::MultiThreaderBase::New();
  multithreader->SetNumberOfWorkUnits(itk::ITK_MAX_THREADS + 10); // will be clamped
  multithreader->SetSingleMethod(modified_function, &helper);

  // Test that the number of threads has actually been clamped
  const itk::ThreadIdType numberOfThreads = multithreader->GetMaximumNumberOfThreads();
  EXPECT_LE(numberOfThreads, itk::ITK_MAX_THREADS);

  const itk::ThreadIdType numberOfWorkUnits = multithreader->GetNumberOfWorkUnits();

  // Set up the helper class
  helper.counters.resize(numberOfWorkUnits);
  helper.timestamps.resize(numberOfWorkUnits);
  for (itk::ThreadIdType k = 0; k < numberOfWorkUnits; ++k)
  {
    helper.counters[k] = 0;
  }

  // Declare an array to test whether all modified times have been used
  const auto istimestamped = std::make_unique<bool[]>(numberOfWorkUnits);

  // Call Modified once on any object to make it up-to-date
  multithreader->Modified();

  const itk::ModifiedTimeType init_mtime = multithreader->GetMTime();
  std::cout << "init_mtime: " << init_mtime << std::endl;

  itk::ModifiedTimeType prev_mtime = init_mtime;

  constexpr unsigned int num_exp{ 500 };

  for (unsigned int i = 0; i < num_exp; ++i)
  {
    multithreader->SingleMethodExecute();

    itk::ModifiedTimeType min_mtime = helper.timestamps[0].GetMTime();
    itk::ModifiedTimeType max_mtime = helper.timestamps[0].GetMTime();
    for (itk::ThreadIdType k = 0; k < numberOfWorkUnits; ++k)
    {
      const itk::ModifiedTimeType & mtime = helper.timestamps[k].GetMTime();
      if (mtime > max_mtime)
      {
        max_mtime = mtime;
      }
      else if (mtime < min_mtime)
      {
        min_mtime = mtime;
      }

      // initialize the array to false
      istimestamped[k] = false;
    }

    // Each work unit should have gotten a unique, consecutive modified time
    EXPECT_EQ(max_mtime - prev_mtime, numberOfWorkUnits) << "Iteration " << i;
    EXPECT_EQ(min_mtime, prev_mtime + 1) << "Iteration " << i;

    for (itk::ThreadIdType k = 0; k < numberOfWorkUnits; ++k)
    {
      // Test whether all modified times have been used exactly once
      const itk::ModifiedTimeType index = helper.timestamps[k].GetMTime() - min_mtime;

      EXPECT_FALSE(istimestamped[index]) << helper.timestamps[k].GetMTime() << " was used twice as a timestamp!";
      istimestamped[index] = true;

      // Test the counters: each thread should have been called i+1 times
      EXPECT_EQ(helper.counters[k], i + 1) << "counter[" << k << "] at iteration " << i;
    }

    prev_mtime = max_mtime;
  }
}
