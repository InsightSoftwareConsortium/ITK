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
  for (ThreadIdType i = 0; i < ITK_MAX_THREADS; ++i)
  {
    m_ThreadInfoArray[i].WorkUnitID = i;
  }

  ThreadIdType defaultThreads = std::max(1u, GetGlobalDefaultNumberOfThreads());
#if !defined(ITKV4_COMPATIBILITY)
  if (defaultThreads > 1) // one work unit for only one thread
  {
    defaultThreads *= 4;
  }
#endif
  m_NumberOfWorkUnits = std::min<ThreadIdType>(ITK_MAX_THREADS, defaultThreads);
  m_MaximumNumberOfThreads = m_ThreadPool->GetMaximumNumberOfThreads();
}

PoolMultiThreader::~PoolMultiThreader() = default;

void
PoolMultiThreader::SetSingleMethod(ThreadFunctionType f, void * data)
{
  m_SingleMethod = f;
  m_SingleData = data;
}

void
PoolMultiThreader::SetMaximumNumberOfThreads(ThreadIdType numberOfThreads)
{
  Superclass::SetMaximumNumberOfThreads(numberOfThreads);
  ThreadIdType threadCount = m_ThreadPool->GetMaximumNumberOfThreads();
  if (threadCount < m_MaximumNumberOfThreads)
  {
    m_ThreadPool->AddThreads(m_MaximumNumberOfThreads - threadCount);
  }
  m_MaximumNumberOfThreads = m_ThreadPool->GetMaximumNumberOfThreads();
}

void
PoolMultiThreader::SingleMethodExecute()
{
  ThreadIdType threadLoop = 0;

  if (!m_SingleMethod)
  {
    itkExceptionMacro(<< "No single method set!");
  }

  // obey the global maximum number of threads limit
  m_NumberOfWorkUnits = std::min(this->GetGlobalMaximumNumberOfThreads(), m_NumberOfWorkUnits);

  for (threadLoop = 1; threadLoop < m_NumberOfWorkUnits; ++threadLoop)
  {
    m_ThreadInfoArray[threadLoop].UserData = m_SingleData;
    m_ThreadInfoArray[threadLoop].NumberOfWorkUnits = m_NumberOfWorkUnits;
    m_ThreadInfoArray[threadLoop].Future = m_ThreadPool->AddWork(m_SingleMethod, &m_ThreadInfoArray[threadLoop]);
  }

  // Now, the parent thread calls this->SingleMethod() itself
  m_ThreadInfoArray[0].UserData = m_SingleData;
  m_ThreadInfoArray[0].NumberOfWorkUnits = m_NumberOfWorkUnits;
  ExceptionHandler exceptionHandler;
  exceptionHandler.TryAndCatch([this] { m_SingleMethod(&m_ThreadInfoArray[0]); });

  // The parent thread has finished SingleMethod()
  // so now it waits for each of the other work units to finish
  for (threadLoop = 1; threadLoop < m_NumberOfWorkUnits; ++threadLoop)
  {
    exceptionHandler.TryAndCatch([this, threadLoop] { m_ThreadInfoArray[threadLoop].Future.get(); });
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
    SizeValueType chunkSize = (lastIndexPlus1 - firstIndex) / m_NumberOfWorkUnits;
    if ((lastIndexPlus1 - firstIndex) % m_NumberOfWorkUnits > 0)
    {
      ++chunkSize; // we want slightly bigger chunks to be processed first
    }

    auto lambda = [aFunc](SizeValueType start, SizeValueType end) {
      for (SizeValueType ii = start; ii < end; ++ii)
      {
        aFunc(ii);
      }
      // make this lambda have the same signature as m_SingleMethod
      return ITK_THREAD_RETURN_DEFAULT_VALUE;
    };

    SizeValueType workUnit = 1;
    for (SizeValueType i = firstIndex + chunkSize; i < lastIndexPlus1; i += chunkSize)
    {
      m_ThreadInfoArray[workUnit++].Future = m_ThreadPool->AddWork(lambda, i, std::min(i + chunkSize, lastIndexPlus1));
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
          status = m_ThreadInfoArray[i].Future.wait_for(threadCompletionPollingInterval);
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
      ThreadIdType                    splitCount = splitter->GetNumberOfSplits(region, m_NumberOfWorkUnits);
      ProgressReporter                reporter(filter, 0, splitCount);
      itkAssertOrThrowMacro(splitCount <= m_NumberOfWorkUnits, "Split count is greater than number of work units!");
      ImageIORegion iRegion;
      ThreadIdType  total;
      for (ThreadIdType i = 1; i < splitCount; ++i)
      {
        iRegion = region;
        total = splitter->GetSplit(i, splitCount, iRegion);
        if (i < total)
        {
          m_ThreadInfoArray[i].Future = m_ThreadPool->AddWork([funcP, iRegion]() {
            funcP(&iRegion.GetIndex()[0], &iRegion.GetSize()[0]);
            // make this lambda have the same signature as m_SingleMethod
            return ITK_THREAD_RETURN_DEFAULT_VALUE;
          });
        }
        else
        {
          itkExceptionMacro("Could not get work unit "
                            << i << " even though we checked possible number of splits beforehand!");
        }
      }
      iRegion = region;
      total = splitter->GetSplit(0, splitCount, iRegion);

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
            status = m_ThreadInfoArray[i].Future.wait_for(threadCompletionPollingInterval);
            if (filter && status == std::future_status::timeout)
            {
              filter->IncrementProgress(0);
            }
          } while (status != std::future_status::ready);
          m_ThreadInfoArray[i].Future.get();
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
