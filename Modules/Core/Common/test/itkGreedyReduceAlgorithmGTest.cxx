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

#include "itkGreedyReduceAlgorithm.h"
#include "itkGTest.h"

#include <map>
#include <numeric>
#include <thread>
#include <vector>

namespace
{

// Merge function for int: add source into target.
void
IntMerge(int & target, int & source)
{
  target += source;
}

// Merge function for std::map<int,int>: accumulate values per key.
void
MapMerge(std::map<int, int> & target, std::map<int, int> & source)
{
  for (auto & [key, value] : source)
  {
    target[key] += value;
  }
}

// Merge function for std::vector<int>: concatenate source into target.
void
VectorMerge(std::vector<int> & target, std::vector<int> & source)
{
  target.insert(target.end(), source.begin(), source.end());
}

} // namespace

// ---------------------------------------------------------------------------
// Basic object methods (Print, GetNameOfClass)
// ---------------------------------------------------------------------------

TEST(GreedyReduceAlgorithm, BasicObjectMethods)
{
  using ReducerType = itk::GreedyReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);
  reducer->SetNumberOfWorkUnits(4);

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(reducer, GreedyReduceAlgorithm, ReduceAlgorithm);
}

// ---------------------------------------------------------------------------
// GetResult before any Merge returns default-constructed value
// ---------------------------------------------------------------------------

TEST(GreedyReduceAlgorithm, GetResultBeforeMerge)
{
  using ReducerType = itk::GreedyReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);

  EXPECT_EQ(reducer->GetResult(), int{});
}

TEST(GreedyReduceAlgorithm, GetResultBeforeMergeMap)
{
  using MapType = std::map<int, int>;
  using ReducerType = itk::GreedyReduceAlgorithm<MapType>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(MapMerge);

  EXPECT_TRUE(reducer->GetResult().empty());
}

// ---------------------------------------------------------------------------
// Single Merge stores the value as-is
// ---------------------------------------------------------------------------

TEST(GreedyReduceAlgorithm, SingleMerge)
{
  using ReducerType = itk::GreedyReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);

  int value = 42;
  reducer->Merge(std::move(value));

  EXPECT_EQ(reducer->GetResult(), 42);
}

// ---------------------------------------------------------------------------
// Sequential merges accumulate correctly
// ---------------------------------------------------------------------------

TEST(GreedyReduceAlgorithm, SequentialMerges)
{
  using ReducerType = itk::GreedyReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);

  constexpr int N = 10;
  for (int i = 1; i <= N; ++i)
  {
    int v = i;
    reducer->Merge(std::move(v));
  }

  // sum 1..10 = 55
  EXPECT_EQ(reducer->GetResult(), 55);
}

// ---------------------------------------------------------------------------
// Clear resets state; subsequent merges start fresh
// ---------------------------------------------------------------------------

TEST(GreedyReduceAlgorithm, ClearResetsState)
{
  using ReducerType = itk::GreedyReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);

  int v1 = 100;
  reducer->Merge(std::move(v1));
  EXPECT_EQ(reducer->GetResult(), 100);

  reducer->Clear();
  EXPECT_EQ(reducer->GetResult(), int{});

  int v2 = 7;
  reducer->Merge(std::move(v2));
  EXPECT_EQ(reducer->GetResult(), 7);
}

// ---------------------------------------------------------------------------
// Custom merge function: take the maximum instead of summing
// ---------------------------------------------------------------------------

TEST(GreedyReduceAlgorithm, CustomMergeFunction)
{
  using ReducerType = itk::GreedyReduceAlgorithm<int>;
  auto reducer = ReducerType::New();

  // Merge function that keeps the maximum
  reducer->SetMergeFunction([](int & target, int & source) { target = std::max(target, source); });

  for (int v : { 5, 3, 9, 1, 7 })
  {
    reducer->Merge(std::move(v));
  }

  EXPECT_EQ(reducer->GetResult(), 9);
}

// ---------------------------------------------------------------------------
// Concurrent merges (integer): sum N threads * value == expected total
// ---------------------------------------------------------------------------

TEST(GreedyReduceAlgorithm, ConcurrentMergesInt)
{
  using ReducerType = itk::GreedyReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);

  constexpr int numThreads = 100;
  constexpr int valuePerThread = 1;
  reducer->SetNumberOfWorkUnits(numThreads);

  std::vector<std::thread> threads;
  threads.reserve(numThreads);

  for (int i = 0; i < numThreads; ++i)
  {
    threads.emplace_back([&]() {
      int v = valuePerThread;
      reducer->Merge(std::move(v));
    });
  }

  for (auto & t : threads)
  {
    t.join();
  }

  EXPECT_EQ(reducer->GetResult(), numThreads * valuePerThread);
}

// ---------------------------------------------------------------------------
// Concurrent merges (map): each thread contributes unique keys
// ---------------------------------------------------------------------------

TEST(GreedyReduceAlgorithm, ConcurrentMergesMap)
{
  using MapType = std::map<int, int>;
  using ReducerType = itk::GreedyReduceAlgorithm<MapType>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(MapMerge);

  constexpr int numThreads = 50;
  // Thread i inserts key=i with value=i
  reducer->SetNumberOfWorkUnits(numThreads);

  std::vector<std::thread> threads;
  threads.reserve(numThreads);

  for (int i = 0; i < numThreads; ++i)
  {
    threads.emplace_back([&, i]() {
      MapType local;
      local[i] = i;
      reducer->Merge(std::move(local));
    });
  }

  for (auto & t : threads)
  {
    t.join();
  }

  const MapType & result = reducer->GetResult();
  ASSERT_EQ(static_cast<int>(result.size()), numThreads);
  for (int i = 0; i < numThreads; ++i)
  {
    auto it = result.find(i);
    ASSERT_NE(it, result.end()) << "Key " << i << " missing from result";
    EXPECT_EQ(it->second, i) << "Wrong value for key " << i;
  }
}

// ---------------------------------------------------------------------------
// Concurrent merges (map, shared keys): values are correctly summed
// ---------------------------------------------------------------------------

TEST(GreedyReduceAlgorithm, ConcurrentMergesMapSharedKeys)
{
  using MapType = std::map<int, int>;
  using ReducerType = itk::GreedyReduceAlgorithm<MapType>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(MapMerge);

  constexpr int numThreads = 50;
  constexpr int numKeys = 5;
  // Every thread contributes 1 to each of the numKeys keys.
  reducer->SetNumberOfWorkUnits(numThreads);

  std::vector<std::thread> threads;
  threads.reserve(numThreads);

  for (int i = 0; i < numThreads; ++i)
  {
    threads.emplace_back([&]() {
      MapType local;
      for (int k = 0; k < numKeys; ++k)
      {
        local[k] = 1;
      }
      reducer->Merge(std::move(local));
    });
  }

  for (auto & t : threads)
  {
    t.join();
  }

  const MapType & result = reducer->GetResult();
  ASSERT_EQ(static_cast<int>(result.size()), numKeys);
  for (int k = 0; k < numKeys; ++k)
  {
    auto it = result.find(k);
    ASSERT_NE(it, result.end()) << "Key " << k << " missing";
    EXPECT_EQ(it->second, numThreads) << "Wrong count for key " << k;
  }
}

// ---------------------------------------------------------------------------
// Concurrent merges (vector): all elements are present after reduction
// ---------------------------------------------------------------------------

TEST(GreedyReduceAlgorithm, ConcurrentMergesVector)
{
  using VecType = std::vector<int>;
  using ReducerType = itk::GreedyReduceAlgorithm<VecType>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(VectorMerge);

  constexpr int numThreads = 40;
  reducer->SetNumberOfWorkUnits(numThreads);

  std::vector<std::thread> threads;
  threads.reserve(numThreads);

  for (int i = 0; i < numThreads; ++i)
  {
    threads.emplace_back([&, i]() {
      VecType local = { i };
      reducer->Merge(std::move(local));
    });
  }

  for (auto & t : threads)
  {
    t.join();
  }

  const VecType & result = reducer->GetResult();
  ASSERT_EQ(static_cast<int>(result.size()), numThreads);

  // All thread indices 0..numThreads-1 must appear exactly once
  VecType sorted = result;
  std::sort(sorted.begin(), sorted.end());
  for (int i = 0; i < numThreads; ++i)
  {
    EXPECT_EQ(sorted[i], i);
  }
}

// ---------------------------------------------------------------------------
// NumberOfWorkUnits is stored and retrieved correctly
// ---------------------------------------------------------------------------

TEST(GreedyReduceAlgorithm, NumberOfWorkUnits)
{
  using ReducerType = itk::GreedyReduceAlgorithm<int>;
  auto reducer = ReducerType::New();
  reducer->SetMergeFunction(IntMerge);

  EXPECT_EQ(reducer->GetNumberOfWorkUnits(), itk::SizeValueType{ 0 });

  reducer->SetNumberOfWorkUnits(8);
  EXPECT_EQ(reducer->GetNumberOfWorkUnits(), itk::SizeValueType{ 8 });

  // Clear resets work unit count to zero
  reducer->Clear();
  EXPECT_EQ(reducer->GetNumberOfWorkUnits(), itk::SizeValueType{ 0 });
}

// ---------------------------------------------------------------------------
// MergeFunction getter returns the stored function
// ---------------------------------------------------------------------------

TEST(GreedyReduceAlgorithm, MergeFunctionRoundTrip)
{
  using ReducerType = itk::GreedyReduceAlgorithm<int>;
  auto reducer = ReducerType::New();

  EXPECT_FALSE(reducer->GetMergeFunction());

  reducer->SetMergeFunction(IntMerge);
  EXPECT_TRUE(reducer->GetMergeFunction());
}
