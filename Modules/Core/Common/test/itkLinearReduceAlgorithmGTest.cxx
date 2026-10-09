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

#include "itkLinearReduceAlgorithm.h"
#include "itkGTest.h"

#include <map>
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

TEST(LinearReduceAlgorithm, BasicObjectMethods)
{
  using ReducerType = itk::LinearReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(4);

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(reducer, LinearReduceAlgorithm, ReduceAlgorithm);
}

// ---------------------------------------------------------------------------
// GetResult before any Merge returns default-constructed value
// ---------------------------------------------------------------------------

TEST(LinearReduceAlgorithm, GetResultBeforeMerge)
{
  using ReducerType = itk::LinearReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(4);

  EXPECT_EQ(reducer->GetResult(), int{});
}

// ---------------------------------------------------------------------------
// Single chunk (N=1): no merge needed, value is the result
// ---------------------------------------------------------------------------

TEST(LinearReduceAlgorithm, SingleChunk)
{
  using ReducerType = itk::LinearReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(1);

  int v = 42;
  reducer->Merge(0, std::move(v));

  EXPECT_EQ(reducer->GetResult(), 42);
}

// ---------------------------------------------------------------------------
// Chunks submitted in ascending order
// ---------------------------------------------------------------------------

TEST(LinearReduceAlgorithm, ChunksInOrder)
{
  using ReducerType = itk::LinearReduceAlgorithm<int>;
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
// Chunks submitted in reverse order — same result (deterministic)
// ---------------------------------------------------------------------------

TEST(LinearReduceAlgorithm, ChunksReversed)
{
  using ReducerType = itk::LinearReduceAlgorithm<int>;
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
// Chunks submitted out of order
// ---------------------------------------------------------------------------

TEST(LinearReduceAlgorithm, ChunksOutOfOrder)
{
  using ReducerType = itk::LinearReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(5);

  for (int i : { 2, 0, 4, 1, 3 })
  {
    reducer->Merge(static_cast<itk::SizeValueType>(i), std::move(i));
  }
  // 0+1+2+3+4 = 10
  EXPECT_EQ(reducer->GetResult(), 10);
}

// ---------------------------------------------------------------------------
// Arbitrary N (no power-of-two restriction)
// ---------------------------------------------------------------------------

TEST(LinearReduceAlgorithm, ArbitrarySizes)
{
  for (int n : { 1, 2, 3, 5, 6, 7, 9, 10, 13, 15 })
  {
    using ReducerType = itk::LinearReduceAlgorithm<int>;
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

TEST(LinearReduceAlgorithm, ClearAndReuse)
{
  using ReducerType = itk::LinearReduceAlgorithm<int>;
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

TEST(LinearReduceAlgorithm, MergeWithoutIdThrows)
{
  using ReducerType = itk::LinearReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(2);

  int v = 5;
  EXPECT_THROW(reducer->Merge(std::move(v)), itk::ExceptionObject);
}

// ---------------------------------------------------------------------------
// Merge called before SetNumberOfWorkUnits must throw
// ---------------------------------------------------------------------------

TEST(LinearReduceAlgorithm, MergeBeforeSetNumberOfWorkUnitsThrows)
{
  using ReducerType = itk::LinearReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);

  int v = 5;
  EXPECT_THROW(reducer->Merge(0, std::move(v)), itk::ExceptionObject);
}

// ---------------------------------------------------------------------------
// chunkId out of range must throw
// ---------------------------------------------------------------------------

TEST(LinearReduceAlgorithm, ChunkIdOutOfRangeThrows)
{
  using ReducerType = itk::LinearReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(2);

  int v = 5;
  EXPECT_THROW(reducer->Merge(2, std::move(v)), itk::ExceptionObject);
}

// ---------------------------------------------------------------------------
// Concurrent merges: N threads each contribute a known integer.
// Result must equal the expected sum, regardless of arrival order.
// ---------------------------------------------------------------------------

TEST(LinearReduceAlgorithm, ConcurrentMergesInt)
{
  constexpr int numChunks = 16;

  using ReducerType = itk::LinearReduceAlgorithm<int>;
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
// with concurrent threads, since the merge order is fixed by chunk ID.
// ---------------------------------------------------------------------------

TEST(LinearReduceAlgorithm, DeterministicFloat)
{
  constexpr int            numChunks = 8;
  const std::vector<float> values = { 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.8f };

  using ReducerType = itk::LinearReduceAlgorithm<float>;

  // Compute the reference result from a single-threaded run.
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

TEST(LinearReduceAlgorithm, ConcurrentMergesMap)
{
  constexpr int numChunks = 12;

  using MapType = std::map<int, int>;
  using ReducerType = itk::LinearReduceAlgorithm<MapType>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(MapMerge);
  reducer->SetNumberOfWorkUnits(numChunks);

  std::vector<std::thread> threads;
  threads.reserve(numChunks);
  for (int i = 0; i < numChunks; ++i)
  {
    threads.emplace_back([&reducer, i]() {
      MapType m{ { i, i * i } };
      reducer->Merge(static_cast<itk::SizeValueType>(i), std::move(m));
    });
  }
  for (auto & t : threads)
  {
    t.join();
  }

  const MapType & result = reducer->GetResult();
  ASSERT_EQ(result.size(), static_cast<std::size_t>(numChunks));
  for (int i = 0; i < numChunks; ++i)
  {
    EXPECT_EQ(result.at(i), i * i);
  }
}
