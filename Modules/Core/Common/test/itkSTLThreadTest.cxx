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
#ifndef ITK_LEGACY_REMOVE
#  define ITK_LEGACY_TEST // so deprecation warnings are not triggered by this test
#endif
#include "itkPlatformMultiThreader.h"

namespace itkSTLThreadTestImpl
{
static int        done = 0;
static int        numberOfIterations = 10;
static std::mutex threadMutex;

static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
Runner(void *);
static int
Thread(int);
} // namespace itkSTLThreadTestImpl

int
itkSTLThreadTest(int argc, char * argv[])
{
  // Choose a number of threads.
  size_t numWorkUnits = 10;
  if (argc > 1)
  {
    int nt = std::stoi(argv[1]);
    if (nt > 1)
    {
      numWorkUnits = nt;
    }
  }

  // Choose a number of iterations (0 is infinite).
  if (argc > 2)
  {
    int ni = std::stoi(argv[2]);
    if (ni >= 0)
    {
      itkSTLThreadTestImpl::numberOfIterations = ni;
    }
  }

  // Enforce limit on number of threads.
  if (numWorkUnits > itk::ITK_MAX_THREADS)
  {
    numWorkUnits = itk::ITK_MAX_THREADS;
  }

  // Report what we'll do.
  std::cout << "Using " << numWorkUnits << " work units.\n";
  if (itkSTLThreadTestImpl::numberOfIterations)
  {
    std::cout << "Using " << itkSTLThreadTestImpl::numberOfIterations << " iterations.\n";
  }
  else
  {
    std::cout << "Using infinite iterations.\n";
  }

  // Create result array.  Assume failure.
  auto * results = new int[numWorkUnits];
  for (size_t i = 0; i < numWorkUnits; ++i)
  {
    results[i] = 0;
  }

  // Create and execute the threads.
  itk::PlatformMultiThreader::Pointer threader = itk::PlatformMultiThreader::New();
  threader->SetSingleMethod(itkSTLThreadTestImpl::Runner, results);
  threader->SetNumberOfWorkUnits(numWorkUnits);
  threader->SingleMethodExecute();

  // Report results.
  int result = 0;
  for (size_t i = 0; i < numWorkUnits; ++i)
  {
    if (!results[i])
    {
      std::cerr << "Work unit " << i << " failed." << std::endl;
      result = 1;
    }
  }
  delete[] results;

  // Test other methods for coverage.
  std::cout << "Done with primary test.  Testing more methods..." << std::endl;
  itk::MultiThreaderBase::SetGlobalMaximumNumberOfThreads(1);
  itk::MultiThreaderBase::SetGlobalDefaultNumberOfThreads(1);

  std::cout << "itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads(): "
            << itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads() << std::endl;

#if !defined(ITK_LEGACY_REMOVE)
  // test deprecated methods too!
  itk::ThreadIdType threadId = threader->SpawnThread(itkSTLThreadTestImpl::Runner, nullptr);
  itkSTLThreadTestImpl::threadMutex.lock();
  std::cout << "SpawnThread(itkSTLThreadTestImpl::Runner, results): " << threadId << std::endl;
  itkSTLThreadTestImpl::threadMutex.unlock();
  threader->TerminateThread(threadId);
  itkSTLThreadTestImpl::threadMutex.lock();
  std::cout << "Spawned thread terminated." << std::endl;
  itkSTLThreadTestImpl::threadMutex.unlock();
#endif

  return result;
}

namespace itkSTLThreadTestImpl
{

static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
Runner(void * infoIn)
{
  // Get the work unit id and result pointer and run the method for this work unit.
  auto *            info = static_cast<itk::PlatformMultiThreader::WorkUnitInfo *>(infoIn);
  itk::ThreadIdType tnum = info->WorkUnitID;
  auto *            results = static_cast<int *>(info->UserData);
  if (results)
  {
    results[tnum] = itkSTLThreadTestImpl::Thread(tnum);
  }
  else
  {
    itkSTLThreadTestImpl::Thread(tnum);
  }
  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

static int
Thread(int tnum)
{
  // Implementation in individual thread.  We don't care about
  // mutexing the output because it doesn't matter for the test.
  threadMutex.lock();
  std::cout << "Starting " << tnum << '\n';
  threadMutex.unlock();

  // Create a list with which to play.
  std::list<int> l;

  // Choose a size for each iteration for this thread.
  int count = 10000 + 100 * tnum;

  int iteration = 0;
  while (!done && !(numberOfIterations && (iteration >= numberOfIterations)))
  {
    // Output progress of this thread.
    threadMutex.lock();
    std::cout << tnum << ": " << iteration << '\n';
    threadMutex.unlock();

    // Fill the list.
    int j;
    for (j = 0; j < count; ++j)
    {
      l.push_back(j);
    }

    // Empty the list while making sure values match.  Threading
    // errors can cause mismatches here, which is the purpose of the
    // test.
    for (j = 0; j < count; ++j)
    {
      if (l.front() != j)
      {
        threadMutex.lock();
        std::cerr << "Mismatch in thread " << tnum << "!\n";
        done = 1;
        threadMutex.unlock();
      }
      l.pop_front();
    }

    ++iteration;
  }

  // Only get here on failure or iterations finished.
  if (numberOfIterations && (iteration >= numberOfIterations))
  {
    // Success.
    return EXIT_FAILURE;
  }
  else
  {
    // Failure.
    return EXIT_SUCCESS;
  }
}

} // namespace itkSTLThreadTestImpl
