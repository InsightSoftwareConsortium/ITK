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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkPoolMultiThreader.h"
#include "itkNumericTraits.h"
#include "itkProcessObject.h"
#include "itkImageSourceCommon.h"
#include <algorithm>
#include <exception>
#include <iostream>
#include <string>

namespace itk
{
namespace
{
std::chrono::milliseconds threadCompletionPollingInterval = std::chrono::milliseconds(10);

class ExceptionHandler
{
public:
  // This class follows the rule of zero

  template <typename TFunction>
  void
  TryAndCatch(const TFunction & function)
  {
    try
    {
      function();
    }
    catch (...)
    {
      if (m_FirstCaughtException == nullptr)
      {
        m_FirstCaughtException = std::current_exception();
      }
    }
  }

  bool
  HasCaughtException() const
  {
    return m_FirstCaughtException != nullptr;
  }

  void
  RethrowFirstCaughtException() const
  {
    if (m_FirstCaughtException != nullptr)
    {
      std::rethrow_exception(m_FirstCaughtException);
    }
  }

private:
  std::exception_ptr m_FirstCaughtException;
};

// Tasks refer to the caller's stack, so every submitted future must be
// waited for, even after a failure, before the caller returns or unwinds.
template <typename TWorkUnits>
void
WaitForWorkUnits(TWorkUnits &       workUnits,
                 const std::size_t  count,
                 ExceptionHandler & exceptionHandler,
                 ProcessObject *    filter,
                 ProgressReporter * reporter,
                 const bool         getResults = true)
{
  for (std::size_t i = 1; i < count; ++i)
  {
    auto &             future = workUnits[i].Future;
    std::future_status status;
    do
    {
      status = future.wait_for(threadCompletionPollingInterval);
      if (filter && status == std::future_status::timeout)
      {
        exceptionHandler.TryAndCatch([filter] { filter->IncrementProgress(0); });
      }
    } while (status != std::future_status::ready);
    exceptionHandler.TryAndCatch([&future, reporter, getResults] {
      if (getResults)
      {
        future.get();
      }
      if (reporter != nullptr)
      {
        reporter->CompletedPixel();
      }
    });
  }
}

// Read-only state shared by all tasks of one call. It is aligned and padded so
// that the submitting thread's stack writes (such as call return addresses)
// never invalidate the cache line the workers read the functor from.
struct alignas(128) SharedArrayState
{
  MultiThreaderBase::ArrayThreadingFunctorType Func;

  void
  Run(SizeValueType start, SizeValueType end) const
  {
    for (SizeValueType ii = start; ii < end; ++ii)
    {
      Func(ii);
    }
  }
};

struct alignas(128) SharedRegionState
{
  MultiThreaderBase::ThreadingFunctorType Func;
  ImageIORegion                           Region;
  const ImageRegionSplitterBase *         Splitter;
  ThreadIdType                            SplitCount;

  // Each task derives its own sub-region so the submitting thread need not build one per task.
  void
  Run(ThreadIdType i) const
  {
    ImageIORegion      iRegion = Region;
    const ThreadIdType total = Splitter->GetSplit(i, SplitCount, iRegion);
    if (i >= total)
    {
      itkGenericExceptionMacro("Could not get work unit "
                               << i << " even though we checked possible number of splits beforehand!");
    }
    Func(&iRegion.GetIndex()[0], &iRegion.GetSize()[0]);
  }
};
} // namespace


PoolMultiThreader::PoolMultiThreader()
  : m_ThreadPool(ThreadPool::GetInstance())
{
  ThreadIdType defaultThreads = std::max(1u, GetGlobalDefaultNumberOfThreads());
  if (defaultThreads > 1) // one work unit for only one thread
  {
    const ThreadIdType maximum = itk::NumericTraits<ThreadIdType>::max();
    const auto         workUnitPerThreadRatio = GetGlobalDefaultWorkUnitPerThreadRatio();
    // Prevent overflow when multiplying defaultThreads by workUnitPerThreadRatio
    if (defaultThreads > maximum / workUnitPerThreadRatio)
    {
      defaultThreads = maximum;
    }
    else
    {
      defaultThreads = defaultThreads * workUnitPerThreadRatio;
    }
  }

  m_NumberOfWorkUnits = defaultThreads;
  m_MaximumNumberOfThreads = m_ThreadPool->GetMaximumNumberOfThreads();
}

PoolMultiThreader::~PoolMultiThreader() = default;

void
PoolMultiThreader::SetSingleMethod(ThreadFunctionType f, void * data)
{
  m_SingleMethod = std::move(f);
  m_SingleData = data;
}

void
PoolMultiThreader::SetMaximumNumberOfThreads(ThreadIdType numberOfThreads)
{
  Superclass::SetMaximumNumberOfThreads(numberOfThreads);
  const ThreadIdType threadCount = m_ThreadPool->GetMaximumNumberOfThreads();
  if (threadCount < m_MaximumNumberOfThreads)
  {
    m_ThreadPool->AddThreads(m_MaximumNumberOfThreads - threadCount);
  }
  m_MaximumNumberOfThreads = m_ThreadPool->GetMaximumNumberOfThreads();
}

void
PoolMultiThreader::SetNumberOfWorkUnits(ThreadIdType numberOfWorkUnits)
{
  m_NumberOfWorkUnits = std::max<ThreadIdType>(1, numberOfWorkUnits);
}

void
PoolMultiThreader::EnsureWorkUnitInfoArraySize(std::size_t size)
{
  const std::size_t currentSize = m_WorkUnitInfoArray.size();
  if (size > currentSize)
  {
    m_WorkUnitInfoArray.resize(size);
    for (std::size_t i = currentSize; i < size; ++i)
    {
      m_WorkUnitInfoArray[i].WorkUnitID = static_cast<ThreadIdType>(i);
    }
  }
}

void
PoolMultiThreader::SingleMethodExecute()
{
  if (!m_SingleMethod)
  {
    itkExceptionStringMacro("No single method set!");
  }

  const ThreadIdType numberOfWorkUnits = m_NumberOfWorkUnits;
  this->EnsureWorkUnitInfoArraySize(numberOfWorkUnits);

  ExceptionHandler exceptionHandler;
  ThreadIdType     submitted = 1;
  exceptionHandler.TryAndCatch([this, numberOfWorkUnits, &submitted] {
    for (; submitted < numberOfWorkUnits; ++submitted)
    {
      InternalWorkUnitInfo & workUnit = m_WorkUnitInfoArray[submitted];
      workUnit.UserData = m_SingleData;
      workUnit.NumberOfWorkUnits = numberOfWorkUnits;
      workUnit.Future =
        m_ThreadPool->AddWork([method = &m_SingleMethod, threadInfo = &workUnit] { (*method)(threadInfo); });
    }
  });

  // Now, the parent thread calls this->SingleMethod() itself
  if (!exceptionHandler.HasCaughtException())
  {
    m_WorkUnitInfoArray[0].UserData = m_SingleData;
    m_WorkUnitInfoArray[0].NumberOfWorkUnits = numberOfWorkUnits;
    exceptionHandler.TryAndCatch([this] { m_SingleMethod(&m_WorkUnitInfoArray[0]); });
  }

  // The parent thread has finished SingleMethod()
  // so now it waits for each of the other work units to finish
  WaitForWorkUnits(m_WorkUnitInfoArray, submitted, exceptionHandler, nullptr, nullptr);

  exceptionHandler.RethrowFirstCaughtException();
}

void
PoolMultiThreader::ParallelizeArray(SizeValueType             firstIndex,
                                    SizeValueType             lastIndexPlus1,
                                    ArrayThreadingFunctorType aFunc,
                                    ProcessObject *           filter)
{
  if (!this->GetUpdateProgress())
  {
    filter = nullptr;
  }

  if (firstIndex + 1 < lastIndexPlus1)
  {
    const SizeValueType numberOfItems = lastIndexPlus1 - firstIndex;
    SizeValueType       chunkSize = numberOfItems / m_NumberOfWorkUnits;
    if (numberOfItems % m_NumberOfWorkUnits > 0)
    {
      ++chunkSize; // we want slightly bigger chunks to be processed first
    }
    const SizeValueType roundingLeftover = numberOfItems % chunkSize > 0 ? 1 : 0;
    const SizeValueType workUnitCount = numberOfItems / chunkSize + roundingLeftover;
    itkAssertOrThrowMacro(workUnitCount <= m_NumberOfWorkUnits, "Number of work units was somehow miscounted!");
    this->EnsureWorkUnitInfoArraySize(workUnitCount);

    // Tasks refer to this; it outlives them because all futures are waited for below.
    const SharedArrayState state{ std::move(aFunc) };

    ProgressReporter reporter(filter, 0, workUnitCount);

    ExceptionHandler exceptionHandler;
    SizeValueType    workUnit = 1;
    exceptionHandler.TryAndCatch([&] {
      for (SizeValueType i = firstIndex + chunkSize; i < lastIndexPlus1; i += chunkSize)
      {
        const SizeValueType end = std::min(i + chunkSize, lastIndexPlus1);
        m_WorkUnitInfoArray[workUnit].Future =
          m_ThreadPool->AddWork([statePtr = &state, i, end] { statePtr->Run(i, end); });
        ++workUnit;
      }
    });

    // execute this thread's share
    if (!exceptionHandler.HasCaughtException())
    {
      exceptionHandler.TryAndCatch([&state, firstIndex, chunkSize, &reporter] {
        state.Run(firstIndex, firstIndex + chunkSize);
        reporter.CompletedPixel();
      });
    }

    // now wait for the other computations to finish
    WaitForWorkUnits(m_WorkUnitInfoArray, workUnit, exceptionHandler, filter, &reporter, false);

    exceptionHandler.RethrowFirstCaughtException();
  }
  else if (firstIndex + 1 == lastIndexPlus1)
  {
    aFunc(firstIndex);
  }
  // else nothing needs to be executed
}

void
PoolMultiThreader::ParallelizeImageRegion(unsigned int         dimension,
                                          const IndexValueType index[],
                                          const SizeValueType  size[],
                                          ThreadingFunctorType funcP,
                                          ProcessObject *      filter)
{
  if (!this->GetUpdateProgress())
  {
    filter = nullptr;
  }

  if (m_NumberOfWorkUnits == 1) // no multi-threading wanted
  {
    ProgressReporter reporter(filter, 0, 1);
    funcP(index, size); // process whole region
    reporter.CompletedPixel();
  }
  else
  {
    ImageIORegion region(dimension);
    for (unsigned int d = 0; d < dimension; ++d)
    {
      region.SetIndex(d, index[d]);
      region.SetSize(d, size[d]);
    }
    if (region.GetNumberOfPixels() <= 1)
    {
      funcP(index, size); // process whole region
    }
    else
    {
      const ImageRegionSplitterBase * splitter = ImageSourceCommon::GetGlobalDefaultSplitter();
      const ThreadIdType              splitCount = splitter->GetNumberOfSplits(region, m_NumberOfWorkUnits);
      this->EnsureWorkUnitInfoArraySize(splitCount);
      ProgressReporter reporter(filter, 0, splitCount);
      itkAssertOrThrowMacro(splitCount <= m_NumberOfWorkUnits, "Split count is greater than number of work units!");

      // Tasks refer to this; it outlives them because all futures are waited for below.
      const SharedRegionState state{ std::move(funcP), std::move(region), splitter, splitCount };

      ExceptionHandler exceptionHandler;
      ThreadIdType     submitted = 1;
      exceptionHandler.TryAndCatch([&] {
        for (; submitted < splitCount; ++submitted)
        {
          m_WorkUnitInfoArray[submitted].Future =
            m_ThreadPool->AddWork([statePtr = &state, submitted] { statePtr->Run(submitted); });
        }
      });

      // execute this thread's share
      if (!exceptionHandler.HasCaughtException())
      {
        exceptionHandler.TryAndCatch([&state, &reporter] {
          state.Run(0);
          reporter.CompletedPixel();
        });
      }

      // now wait for the other computations to finish
      WaitForWorkUnits(m_WorkUnitInfoArray, submitted, exceptionHandler, filter, &reporter);

      exceptionHandler.RethrowFirstCaughtException();
    }
  }
}

void
PoolMultiThreader::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // namespace itk
