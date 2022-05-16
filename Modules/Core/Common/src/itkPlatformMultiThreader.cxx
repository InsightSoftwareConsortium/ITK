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
#include "itkPlatformMultiThreader.h"
#include "itkNumericTraits.h"
#include <algorithm>
#include <iostream>
#include <string>
#include <algorithm>

#if defined(ITK_USE_PTHREADS)
#  include "itkPlatformMultiThreaderPosix.cxx"
#elif defined(ITK_USE_WIN32_THREADS)
#  include "itkPlatformMultiThreaderWindows.cxx"
#else
#  include "itkPlatformMultiThreaderSingle.cxx"
#endif

namespace itk
{

PlatformMultiThreader::PlatformMultiThreader()
{
  for (ThreadIdType i = 0; i < ITK_MAX_THREADS; ++i)
  {
    m_ThreadInfoArray[i].WorkUnitID = i;
    m_ThreadInfoArray[i].ActiveFlag = nullptr;
    m_ThreadInfoArray[i].ActiveFlagLock = nullptr;

#if !defined(ITK_LEGACY_REMOVE)
    m_MultipleMethod[i] = nullptr;
    m_MultipleData[i] = nullptr;
#endif

    m_SpawnedThreadActiveFlag[i] = 0;
    m_SpawnedThreadActiveFlagLock[i] = nullptr;
    m_SpawnedThreadInfoArray[i].WorkUnitID = i;
  }
}

PlatformMultiThreader::~PlatformMultiThreader() = default;

void
PlatformMultiThreader::SetMaximumNumberOfThreads(ThreadIdType numberOfThreads)
{
  Superclass::SetMaximumNumberOfThreads(numberOfThreads);
  Superclass::SetNumberOfWorkUnits(this->GetMaximumNumberOfThreads());
}

void
PlatformMultiThreader::SetNumberOfWorkUnits(ThreadIdType numberOfWorkUnits)
{
  this->SetMaximumNumberOfThreads(numberOfWorkUnits);
}

void
PlatformMultiThreader::SetSingleMethod(ThreadFunctionType f, void * data)
{
  m_SingleMethod = f;
  m_SingleData = data;
}

#if !defined(ITK_LEGACY_REMOVE)
// Set one of the user defined methods that will be run on NumberOfWorkUnits
// threads when MultipleMethodExecute is called. This method should be
// called with index = 0, 1, ..,  NumberOfWorkUnits-1 to set up all the
// required user defined methods
void
PlatformMultiThreader::SetMultipleMethod(ThreadIdType index, ThreadFunctionType f, void * data)
{
  // You can only set the method for 0 through NumberOfWorkUnits-1
  if (index >= m_NumberOfWorkUnits)
  {
    itkExceptionMacro(<< "Can't set method " << index << " with a thread count of " << m_NumberOfWorkUnits);
  }
  else
  {
    m_MultipleMethod[index] = f;
    m_MultipleData[index] = data;
  }
}
#endif

void
PlatformMultiThreader::SingleMethodExecute()
{
  ThreadIdType        thread_loop = 0;
  ThreadProcessIdType process_id[ITK_MAX_THREADS];

  if (!m_SingleMethod)
  {
    itkExceptionMacro(<< "No single method set!");
  }

  // obey the global maximum number of threads limit
  m_NumberOfWorkUnits = std::min(MultiThreaderBase::GetGlobalMaximumNumberOfThreads(), m_NumberOfWorkUnits);

  // Spawn a set of threads through the SingleMethodProxy. Exceptions
  // thrown from a thread will be caught by the SingleMethodProxy. A
  // naive mechanism is in place for determining whether a thread
  // threw an exception.
  //
  // Thanks to Hannu Helminen for suggestions on how to catch
  // exceptions thrown by threads.
  bool        exceptionOccurred = false;
  std::string exceptionDetails;
  try
  {
    for (thread_loop = 1; thread_loop < m_NumberOfWorkUnits; ++thread_loop)
    {
      m_ThreadInfoArray[thread_loop].UserData = m_SingleData;
      m_ThreadInfoArray[thread_loop].NumberOfWorkUnits = m_NumberOfWorkUnits;
      m_ThreadInfoArray[thread_loop].ThreadFunction = m_SingleMethod;

      process_id[thread_loop] = this->SpawnDispatchSingleMethodThread(&m_ThreadInfoArray[thread_loop]);
    }
  }
  catch (const std::exception & e)
  {
    // get the details of the exception to rethrow them
    exceptionDetails = e.what();
    // If creation of any thread failed, we must make sure that all
    // threads are correctly cleaned
    exceptionOccurred = true;
  }
  catch (...)
  {
    // If creation of any thread failed, we must make sure that all
    // threads are correctly cleaned
    exceptionOccurred = true;
  }

  // Now, the parent thread calls this->SingleMethod() itself
  //
  //
  try
  {
    m_ThreadInfoArray[0].UserData = m_SingleData;
    m_ThreadInfoArray[0].NumberOfWorkUnits = m_NumberOfWorkUnits;
    m_SingleMethod((void *)(&m_ThreadInfoArray[0]));
  }
  catch (const ProcessAborted &)
  {
    // Need cleanup and rethrow ProcessAborted
    // close down other threads
    for (thread_loop = 1; thread_loop < m_NumberOfWorkUnits; ++thread_loop)
    {
      try
      {

        this->SpawnWaitForSingleMethodThread(process_id[thread_loop]);
      }
      catch (...)
      {}
    }
    // rethrow
    throw;
  }
  catch (const std::exception & e)
  {
    // get the details of the exception to rethrow them
    exceptionDetails = e.what();
    // if this method fails, we must make sure all threads are
    // correctly cleaned
    exceptionOccurred = true;
  }
  catch (...)
  {
    // if this method fails, we must make sure all threads are
    // correctly cleaned
    exceptionOccurred = true;
  }
  // The parent thread has finished this->SingleMethod() - so now it
  // waits for each of the other processes to exit
  for (thread_loop = 1; thread_loop < m_NumberOfWorkUnits; ++thread_loop)
  {
    try
    {

      this->SpawnWaitForSingleMethodThread(process_id[thread_loop]);

      if (m_ThreadInfoArray[thread_loop].ThreadExitCode != WorkUnitInfo::ThreadExitCodeEnum::SUCCESS)
      {
        exceptionOccurred = true;
      }
    }
    catch (const std::exception & e)
    {
      // get the details of the exception to rethrow them
      exceptionDetails = e.what();
      exceptionOccurred = true;
    }
    catch (...)
    {
      exceptionOccurred = true;
    }
  }

  if (exceptionOccurred)
  {
    if (exceptionDetails.empty())
    {
      itkExceptionMacro("Exception occurred during SingleMethodExecute");
    }
    else
    {
      itkExceptionMacro(<< "Exception occurred during SingleMethodExecute" << std::endl << exceptionDetails);
    }
  }
}

// Print method for the multithreader
void
PlatformMultiThreader::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // namespace itk
