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
#include <future>
#include <stdexcept>
#include <string>
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
