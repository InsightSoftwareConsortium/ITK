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

#include "itkTreeReduceAlgorithm.h"
#include "itkGTest.h"

#include <algorithm>
#include <map>
#include <numeric>
#include <thread>
#include <vector>

namespace
{

void
IntMerge(int & target, int & source)
{
  target += source;
}

void
FloatMerge(float & target, float & source)
{
  target += source;
}

void
MapMerge(std::map<int, int> & target, std::map<int, int> & source)
{
  for (auto & [key, val] : source)
  {
    target[key] += val;
  }
}

} // namespace

// ---------------------------------------------------------------------------
// Basic object methods
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, BasicObjectMethods)
{
  using ReducerType = itk::TreeReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(4);

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(reducer, TreeReduceAlgorithm, ReduceAlgorithm);
}

// ---------------------------------------------------------------------------
// GetResult before any Merge returns default-constructed value
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, GetResultBeforeMerge)
{
  using ReducerType = itk::TreeReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(4);

  EXPECT_EQ(reducer->GetResult(), int{});
}

// ---------------------------------------------------------------------------
// Single chunk (N=1): no merge needed, value is the result
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, SingleChunk)
{
  using ReducerType = itk::TreeReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(1);

  int v = 42;
  reducer->Merge(0, std::move(v));

  EXPECT_EQ(reducer->GetResult(), 42);
}

// ---------------------------------------------------------------------------
// Two chunks: N=2 (already a power of two)
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, TwoChunks)
{
  using ReducerType = itk::TreeReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(2);

  int v0 = 10, v1 = 32;
  reducer->Merge(0, std::move(v0));
  reducer->Merge(1, std::move(v1));

  EXPECT_EQ(reducer->GetResult(), 42);
}

// ---------------------------------------------------------------------------
// Four chunks (power of two): chunks submitted in ascending order
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, FourChunksInOrder)
{
  using ReducerType = itk::TreeReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(4);

  for (int i = 0; i < 4; ++i)
  {
    reducer->Merge(static_cast<itk::SizeValueType>(i), std::move(i));
  }
  // 0+1+2+3 = 6
  EXPECT_EQ(reducer->GetResult(), 6);
}

// ---------------------------------------------------------------------------
// Four chunks: submitted in reverse order — same result (deterministic)
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, FourChunksReversed)
{
  using ReducerType = itk::TreeReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(4);

  for (int i = 3; i >= 0; --i)
  {
    reducer->Merge(static_cast<itk::SizeValueType>(i), std::move(i));
  }
  EXPECT_EQ(reducer->GetResult(), 6);
}

// ---------------------------------------------------------------------------
// Non-power-of-two: N=5 (padded to 8)
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, NonPowerOfTwo_N5)
{
  using ReducerType = itk::TreeReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(5);

  for (int i = 0; i < 5; ++i)
  {
    reducer->Merge(static_cast<itk::SizeValueType>(i), std::move(i));
  }
  // 0+1+2+3+4 = 10
  EXPECT_EQ(reducer->GetResult(), 10);
}

// ---------------------------------------------------------------------------
// Various non-power-of-two sizes
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, NonPowerOfTwo_Various)
{
  for (int n : { 3, 5, 6, 7, 9, 10, 13, 15 })
  {
    using ReducerType = itk::TreeReduceAlgorithm<int>;
    auto reducer = ReducerType::New();
    reducer->SetMergeFunction(IntMerge);
    reducer->SetNumberOfWorkUnits(static_cast<itk::SizeValueType>(n));

    const int expected = n * (n - 1) / 2;
    for (int i = 0; i < n; ++i)
    {
      reducer->Merge(static_cast<itk::SizeValueType>(i), std::move(i));
    }
    EXPECT_EQ(reducer->GetResult(), expected) << "N=" << n;
  }
}

// ---------------------------------------------------------------------------
// Clear and reuse with the same N
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, ClearAndReuse)
{
  using ReducerType = itk::TreeReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(4);

  for (int i = 0; i < 4; ++i)
  {
    reducer->Merge(static_cast<itk::SizeValueType>(i), std::move(i));
  }
  EXPECT_EQ(reducer->GetResult(), 6);

  reducer->Clear();
  EXPECT_EQ(reducer->GetResult(), int{});

  for (int i = 0; i < 4; ++i)
  {
    int v = i * 10;
    reducer->Merge(static_cast<itk::SizeValueType>(i), std::move(v));
  }
  // 0+10+20+30 = 60
  EXPECT_EQ(reducer->GetResult(), 60);
}

// ---------------------------------------------------------------------------
// Calling Merge(T&&) without chunk ID must throw
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, MergeWithoutIdThrows)
{
  using ReducerType = itk::TreeReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(2);

  int v = 5;
  EXPECT_THROW(reducer->Merge(std::move(v)), itk::ExceptionObject);
}

// ---------------------------------------------------------------------------
// Concurrent merges: N threads each contribute a known integer.
// Result must equal the expected sum.
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, ConcurrentMergesInt)
{
  constexpr int numChunks = 16;

  using ReducerType = itk::TreeReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(numChunks);

  std::vector<std::thread> threads;
  threads.reserve(numChunks);
  for (int i = 0; i < numChunks; ++i)
  {
    threads.emplace_back([&reducer, i]() {
      int v = i;
      reducer->Merge(static_cast<itk::SizeValueType>(i), std::move(v));
    });
  }
  for (auto & t : threads)
  {
    t.join();
  }

  const int expected = numChunks * (numChunks - 1) / 2;
  EXPECT_EQ(reducer->GetResult(), expected);
}

// ---------------------------------------------------------------------------
// Determinism: floating-point sum is bit-identical across many repeated runs
// with concurrent threads, regardless of thread scheduling.
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, DeterministicFloat)
{
  constexpr int            numChunks = 8;
  const std::vector<float> values = { 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.8f };

  using ReducerType = itk::TreeReduceAlgorithm<float>;

  // Compute the reference result from the first run.
  float referenceResult{};
  {
    auto reducer = ReducerType::New();
    reducer->SetMergeFunction(FloatMerge);
    reducer->SetNumberOfWorkUnits(numChunks);
    for (int i = 0; i < numChunks; ++i)
    {
      float v = values[static_cast<std::size_t>(i)];
      reducer->Merge(static_cast<itk::SizeValueType>(i), std::move(v));
    }
    referenceResult = reducer->GetResult();
  }

  // Repeat with concurrent threads many times and verify exact equality.
  constexpr int numRuns = 20;
  for (int run = 0; run < numRuns; ++run)
  {
    auto reducer = ReducerType::New();
    reducer->SetMergeFunction(FloatMerge);
    reducer->SetNumberOfWorkUnits(numChunks);

    std::vector<std::thread> threads;
    threads.reserve(numChunks);
    for (int i = 0; i < numChunks; ++i)
    {
      threads.emplace_back([&reducer, &values, i]() {
        float v = values[static_cast<std::size_t>(i)];
        reducer->Merge(static_cast<itk::SizeValueType>(i), std::move(v));
      });
    }
    for (auto & t : threads)
    {
      t.join();
    }

    EXPECT_EQ(reducer->GetResult(), referenceResult) << "Run " << run << " produced a different result";
  }
}

// ---------------------------------------------------------------------------
// Map reduction: each chunk contributes unique keys; verify all keys present
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, ConcurrentMergesMap)
{
  constexpr int numChunks = 12;

  using MapType = std::map<int, int>;
  using ReducerType = itk::TreeReduceAlgorithm<MapType>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(MapMerge);
  reducer->SetNumberOfWorkUnits(numChunks);

  std::vector<std::thread> threads;
  threads.reserve(numChunks);
  for (int i = 0; i < numChunks; ++i)
  {
    threads.emplace_back([&reducer, i]() {
      MapType local;
      local[i] = i * 10;
      reducer->Merge(static_cast<itk::SizeValueType>(i), std::move(local));
    });
  }
  for (auto & t : threads)
  {
    t.join();
  }

  const MapType & result = reducer->GetResult();
  ASSERT_EQ(static_cast<int>(result.size()), numChunks);
  for (int i = 0; i < numChunks; ++i)
  {
    auto it = result.find(i);
    ASSERT_NE(it, result.end()) << "Key " << i << " missing";
    EXPECT_EQ(it->second, i * 10) << "Wrong value for key " << i;
  }
}

// ---------------------------------------------------------------------------
// NumberOfWorkUnits getter reflects the set value
// ---------------------------------------------------------------------------

TEST(TreeReduceAlgorithm, NumberOfWorkUnits)
{
  using ReducerType = itk::TreeReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);

  EXPECT_EQ(reducer->GetNumberOfWorkUnits(), itk::SizeValueType{ 0 });

  reducer->SetNumberOfWorkUnits(8);
  EXPECT_EQ(reducer->GetNumberOfWorkUnits(), itk::SizeValueType{ 8 });

  // Clear does NOT reset the work unit count.
  reducer->Merge(0, std::move(int{ 1 }));
  reducer->Clear();
  EXPECT_EQ(reducer->GetNumberOfWorkUnits(), itk::SizeValueType{ 8 });
}
