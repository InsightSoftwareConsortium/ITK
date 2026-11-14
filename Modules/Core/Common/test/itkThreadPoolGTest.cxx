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
#include "itkThreadPool.h"
#include <gtest/gtest.h>

#include <atomic>
#include <chrono>
#include <future>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

TEST(ThreadPool, AddWorkRunsVoidTasksWithArguments)
{
  const auto       pool = itk::ThreadPool::GetInstance();
  std::atomic<int> sum{ 0 };

  std::vector<std::future<void>> futures;
  for (int i = 0; i < 100; ++i)
  {
    futures.push_back(pool->AddWork([&sum](int a, int b) { sum += a * b; }, i, 2));
  }
  for (auto & future : futures)
  {
    future.get();
  }
  EXPECT_EQ(sum, 2 * (99 * 100 / 2));
}

TEST(ThreadPool, AddWorkReturnsValuesThroughTheFuture)
{
  const auto pool = itk::ThreadPool::GetInstance();

  auto intFuture = pool->AddWork([](int param) { return param + 1; }, 7);
  auto stringFuture = pool->AddWork([](const std::string & s) { return s + "!"; }, std::string("hello"));
  EXPECT_EQ(intFuture.get(), 8);
  EXPECT_EQ(stringFuture.get(), "hello!");
}

TEST(ThreadPool, AddWorkCopiesArrayArguments)
{
  const auto pool = itk::ThreadPool::GetInstance();

  char buffer[4] = "abc";

  // The tasks wait for the gate, so the buffer is changed after submission but before they run.
  std::promise<void>       gate;
  std::shared_future<void> open = gate.get_future().share();
  auto                     referenceFuture = pool->AddWork(
    [open](const char(&array)[4]) {
      open.wait();
      return std::string(array);
    },
    buffer);
  auto pointerFuture = pool->AddWork(
    [open](const char * pointer) {
      open.wait();
      return std::string(pointer);
    },
    buffer);
  buffer[0] = 'X';
  gate.set_value();

  EXPECT_EQ(referenceFuture.get(), "abc");
  EXPECT_EQ(pointerFuture.get(), "abc");
}

TEST(ThreadPool, AddWorkPropagatesExceptionsThroughTheFuture)
{
  const auto pool = itk::ThreadPool::GetInstance();

  auto voidFuture = pool->AddWork([] { throw std::runtime_error("void"); });
  auto valueFuture = pool->AddWork([]() -> int { throw std::runtime_error("value"); });
  EXPECT_THROW(voidFuture.get(), std::runtime_error);
  EXPECT_THROW(valueFuture.get(), std::runtime_error);
}

TEST(ThreadPool, AddWorkBatchInvokesEveryIndexExactlyOnce)
{
  const auto pool = itk::ThreadPool::GetInstance();
  for (const itk::SizeValueType count : { 0, 1, 2, 3, 7, 64, 1000, 20000 })
  {
    std::vector<std::atomic<int>> hits(count);
    auto                          futures = pool->AddWorkBatch(count, [&hits](itk::SizeValueType i) { ++hits[i]; });
    ASSERT_EQ(futures.size(), count);
    for (auto & future : futures)
    {
      future.get();
    }
    for (itk::SizeValueType i = 0; i < count; ++i)
    {
      ASSERT_EQ(hits[i], 1) << "count " << count << " index " << i;
    }
  }
}

TEST(ThreadPool, AddWorkBatchFuturesAreInIndexOrderAndCarryExceptions)
{
  const auto     pool = itk::ThreadPool::GetInstance();
  constexpr auto count = 100;
  auto           futures = pool->AddWorkBatch(count, [](itk::SizeValueType i) {
    if (i % 10 == 3)
    {
      throw std::runtime_error("expected");
    }
  });
  for (itk::SizeValueType i = 0; i < count; ++i)
  {
    if (i % 10 == 3)
    {
      EXPECT_THROW(futures[i].get(), std::runtime_error) << i;
    }
    else
    {
      EXPECT_NO_THROW(futures[i].get()) << i;
    }
  }
}

TEST(ThreadPool, AddWorkBatchKeepsFunctionAliveAfterTheCallReturns)
{
  const auto                     pool = itk::ThreadPool::GetInstance();
  std::atomic<int>               sum{ 0 };
  std::vector<std::future<void>> futures;
  {
    const std::vector<int> values(50, 2); // destroyed before the jobs necessarily run
    futures = pool->AddWorkBatch(values.size(), [values, &sum](itk::SizeValueType i) { sum += values[i]; });
  }
  for (auto & future : futures)
  {
    future.get();
  }
  EXPECT_EQ(sum, 100);
}

TEST(ThreadPool, AddWorkBatchFromSeveralThreadsAndWithAddWork)
{
  const auto               pool = itk::ThreadPool::GetInstance();
  constexpr int            callers = 8;
  constexpr int            repetitions = 100;
  constexpr int            count = 50;
  std::atomic<int>         total{ 0 };
  std::vector<std::thread> threads;
  for (int c = 0; c < callers; ++c)
  {
    threads.emplace_back([&] {
      for (int rep = 0; rep < repetitions; ++rep)
      {
        auto futures = pool->AddWorkBatch(count, [&total](itk::SizeValueType) { ++total; });
        auto single = pool->AddWork([&total] { ++total; });
        single.get();
        for (auto & future : futures)
        {
          future.get();
        }
      }
    });
  }
  for (auto & thread : threads)
  {
    thread.join();
  }
  EXPECT_EQ(total, callers * repetitions * (count + 1));
}

TEST(ThreadPool, AddWorkBatchAfterThreadsWentToSleep)
{
  const auto pool = itk::ThreadPool::GetInstance();
  for (int rep = 0; rep < 20; ++rep)
  {
    std::this_thread::sleep_for(std::chrono::milliseconds(5)); // let the workers block on the condition variable
    std::atomic<int> total{ 0 };
    auto             futures = pool->AddWorkBatch(3, [&total](itk::SizeValueType) { ++total; });
    for (auto & future : futures)
    {
      future.get();
    }
    EXPECT_EQ(total, 3);
  }
}
