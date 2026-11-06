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

  this->EnsureWorkUnitInfoArraySize(m_NumberOfWorkUnits);

  ThreadIdType threadLoop = 0;
  for (threadLoop = 1; threadLoop < m_NumberOfWorkUnits; ++threadLoop)
  {
    m_WorkUnitInfoArray[threadLoop].UserData = m_SingleData;
    m_WorkUnitInfoArray[threadLoop].NumberOfWorkUnits = m_NumberOfWorkUnits;
    m_WorkUnitInfoArray[threadLoop].Future = m_ThreadPool->AddWork(
      [method = m_SingleMethod, threadInfo = &m_WorkUnitInfoArray[threadLoop]] { method(threadInfo); });
  }

  // Now, the parent thread calls this->SingleMethod() itself
  m_WorkUnitInfoArray[0].UserData = m_SingleData;
  m_WorkUnitInfoArray[0].NumberOfWorkUnits = m_NumberOfWorkUnits;
  ExceptionHandler exceptionHandler;
  exceptionHandler.TryAndCatch([this] { m_SingleMethod(&m_WorkUnitInfoArray[0]); });

  // The parent thread has finished SingleMethod()
  // so now it waits for each of the other work units to finish
  for (threadLoop = 1; threadLoop < m_NumberOfWorkUnits; ++threadLoop)
  {
    exceptionHandler.TryAndCatch([this, threadLoop] { m_WorkUnitInfoArray[threadLoop].Future.get(); });
  }

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
    this->EnsureWorkUnitInfoArraySize(numberOfItems / chunkSize + roundingLeftover);

    auto lambda = [aFunc](SizeValueType start, SizeValueType end) {
      for (SizeValueType ii = start; ii < end; ++ii)
      {
        aFunc(ii);
      }
    };

    SizeValueType workUnit = 1;
    for (SizeValueType i = firstIndex + chunkSize; i < lastIndexPlus1; i += chunkSize)
    {
      m_WorkUnitInfoArray[workUnit++].Future =
        m_ThreadPool->AddWork(lambda, i, std::min(i + chunkSize, lastIndexPlus1));
    }
    itkAssertOrThrowMacro(workUnit <= m_NumberOfWorkUnits, "Number of work units was somehow miscounted!");

    ProgressReporter reporter(filter, 0, workUnit);

    // execute this thread's share
    ExceptionHandler exceptionHandler;
    exceptionHandler.TryAndCatch([lambda, firstIndex, chunkSize, &reporter] {
      lambda(firstIndex, firstIndex + chunkSize);
      reporter.CompletedPixel();
    });

    // now wait for the other computations to finish
    for (SizeValueType i = 1; i < workUnit; ++i)
    {
      exceptionHandler.TryAndCatch([this, i, &reporter, &filter] {
        std::future_status status;
        do
        {
          status = m_WorkUnitInfoArray[i].Future.wait_for(threadCompletionPollingInterval);
          if (filter && status == std::future_status::timeout)
          {
            filter->IncrementProgress(0);
          }
        } while (status != std::future_status::ready);
        reporter.CompletedPixel();
      });
    }

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
      ImageIORegion iRegion;
      ThreadIdType  total = 0;
      for (ThreadIdType i = 1; i < splitCount; ++i)
      {
        iRegion = region;
        total = splitter->GetSplit(i, splitCount, iRegion);
        if (i < total)
        {
          m_WorkUnitInfoArray[i].Future =
            m_ThreadPool->AddWork([funcP, iRegion]() { funcP(&iRegion.GetIndex()[0], &iRegion.GetSize()[0]); });
        }
        else
        {
          itkExceptionMacro("Could not get work unit "
                            << i << " even though we checked possible number of splits beforehand!");
        }
      }
      iRegion = region;
      splitter->GetSplit(0, splitCount, iRegion);

      // execute this thread's share
      ExceptionHandler exceptionHandler;
      exceptionHandler.TryAndCatch([funcP, iRegion, &reporter] {
        funcP(&iRegion.GetIndex()[0], &iRegion.GetSize()[0]);
        reporter.CompletedPixel();
      });

      // now wait for the other computations to finish
      for (ThreadIdType i = 1; i < splitCount; ++i)
      {
        exceptionHandler.TryAndCatch([this, i, &reporter, &filter] {
          std::future_status status;
          do
          {
            status = m_WorkUnitInfoArray[i].Future.wait_for(threadCompletionPollingInterval);
            if (filter && status == std::future_status::timeout)
            {
              filter->IncrementProgress(0);
            }
          } while (status != std::future_status::ready);
          m_WorkUnitInfoArray[i].Future.get();
          reporter.CompletedPixel();
        });
      }

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
