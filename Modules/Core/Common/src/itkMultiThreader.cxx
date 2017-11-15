/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkMultiThreader.h"
#include "itkNumericTraits.h"
#include <iostream>
#include <string>
#if !defined( ITK_LEGACY_FUTURE_REMOVE )
# include "vcl_algorithm.h"
#endif
#include <algorithm>

#if defined(ITK_USE_PTHREADS)
#include "itkMultiThreaderPThreads.cxx"
#elif defined(ITK_USE_WIN32_THREADS)
#include "itkMultiThreaderWinThreads.cxx"
#else
#include "itkMultiThreaderNoThreads.cxx"
#endif

namespace itk
{

// GlobalDefaultUseThreadPoolIsInitialized is used only in this
// file to ensure that the ITK_USE_THREADPOOL environmenal variable
// is only used as a fall back option.  If the SetGlobalDefaultUseThreadPool
// API is ever used by the developer, the developers choice is
// respected over the environmental variable.
static bool GlobalDefaultUseThreadPoolIsInitialized=false;
static SimpleFastMutexLock globalDefaultInitializerLock;

bool MultiThreader::m_GlobalDefaultUseThreadPool = false;

void MultiThreader::SetGlobalDefaultUseThreadPool( const bool GlobalDefaultUseThreadPool )
  {
  m_GlobalDefaultUseThreadPool = GlobalDefaultUseThreadPool;
  GlobalDefaultUseThreadPoolIsInitialized=true;
  }

bool MultiThreader::GetGlobalDefaultUseThreadPool( )
  {
  // This method must be concurrent thread safe

  if( !GlobalDefaultUseThreadPoolIsInitialized )
    {

    MutexLockHolder< SimpleFastMutexLock > lock(globalDefaultInitializerLock);

    // After we have the lock, double check the initialization
    // flag to ensure it hasn't been changed by another thread.

    if (!GlobalDefaultUseThreadPoolIsInitialized )
      {
      // look for runtime request to use thread pool
      std::string use_threadpool;

      if( itksys::SystemTools::GetEnv("ITK_USE_THREADPOOL",use_threadpool) )
        {

        use_threadpool = itksys::SystemTools::UpperCase(use_threadpool);

        // NOTE: GlobalDefaultUseThreadPoolIsInitialized=true after this call
        if(use_threadpool != "NO" && use_threadpool != "OFF" && use_threadpool != "FALSE")
          {
          MultiThreader::SetGlobalDefaultUseThreadPool( true );
          }
        else
          {
          MultiThreader::SetGlobalDefaultUseThreadPool( false );
          }
        }

      // always set that we are initialized
      GlobalDefaultUseThreadPoolIsInitialized=true;
      }
    }
  return m_GlobalDefaultUseThreadPool;
  }

// Initialize static member that controls global maximum number of threads.
ThreadIdType MultiThreader::m_GlobalMaximumNumberOfThreads = ITK_MAX_THREADS;

// Initialize static member that controls global default number of threads : 0
// => Not initialized.
ThreadIdType MultiThreader::m_GlobalDefaultNumberOfThreads = 0;

void MultiThreader::SetGlobalMaximumNumberOfThreads(ThreadIdType val)
{
  m_GlobalMaximumNumberOfThreads = val;

  // clamp between 1 and ITK_MAX_THREADS
  m_GlobalMaximumNumberOfThreads = std::min( m_GlobalMaximumNumberOfThreads,
                                             (ThreadIdType) ITK_MAX_THREADS );
  m_GlobalMaximumNumberOfThreads = std::max( m_GlobalMaximumNumberOfThreads,
                                             NumericTraits<ThreadIdType>::OneValue() );

  // If necessary reset the default to be used from now on.
  m_GlobalDefaultNumberOfThreads = std::min( m_GlobalDefaultNumberOfThreads,
                                             m_GlobalMaximumNumberOfThreads);
}

ThreadIdType MultiThreader::GetGlobalMaximumNumberOfThreads()
{
  return m_GlobalMaximumNumberOfThreads;
}

void MultiThreader::SetGlobalDefaultNumberOfThreads(ThreadIdType val)
{
  m_GlobalDefaultNumberOfThreads = val;

  // clamp between 1 and m_GlobalMaximumNumberOfThreads
  m_GlobalDefaultNumberOfThreads  = std::min( m_GlobalDefaultNumberOfThreads,
                                              m_GlobalMaximumNumberOfThreads );
  m_GlobalDefaultNumberOfThreads  = std::max( m_GlobalDefaultNumberOfThreads,
                                              NumericTraits<ThreadIdType>::OneValue() );

}

void MultiThreader::SetNumberOfThreads(ThreadIdType numberOfThreads)
{
  if( m_NumberOfThreads == numberOfThreads &&
      numberOfThreads <= m_GlobalMaximumNumberOfThreads )
    {
    return;
    }

  m_NumberOfThreads = numberOfThreads;

  // clamp between 1 and m_GlobalMaximumNumberOfThreads
  m_NumberOfThreads  = std::min( m_NumberOfThreads,
                                 m_GlobalMaximumNumberOfThreads );
  m_NumberOfThreads  = std::max( m_NumberOfThreads, NumericTraits<ThreadIdType>::OneValue() );

}

ThreadIdType MultiThreader::GetGlobalDefaultNumberOfThreads()
{
  if( m_GlobalDefaultNumberOfThreads == 0 ) //need to initialize
    {
    m_GlobalDefaultNumberOfThreads = ThreadPool::GetGlobalDefaultNumberOfThreads();
    }
  return m_GlobalDefaultNumberOfThreads;
}

MultiThreader::MultiThreader() :
  m_ThreadPool( ThreadPool::GetInstance() ),
  m_UseThreadPool( MultiThreader::GetGlobalDefaultUseThreadPool() )
{
  for( ThreadIdType i = 0; i < ITK_MAX_THREADS; ++i )
    {
    m_ThreadInfoArray[i].ThreadID           = i;
    m_ThreadInfoArray[i].ActiveFlag         = ITK_NULLPTR;
    m_ThreadInfoArray[i].ActiveFlagLock     = ITK_NULLPTR;

    m_MultipleMethod[i]                     = ITK_NULLPTR;
    m_MultipleData[i]                       = ITK_NULLPTR;

    m_SpawnedThreadActiveFlag[i]            = 0;
    m_SpawnedThreadActiveFlagLock[i]        = ITK_NULLPTR;
    m_SpawnedThreadInfoArray[i].ThreadID    = i;
    }

  m_SingleMethod = ITK_NULLPTR;
  m_SingleData = ITK_NULLPTR;
  if (m_UseThreadPool)
    {
    ThreadIdType idleCount = std::max<ThreadIdType>(1u,
        m_ThreadPool->GetNumberOfCurrentlyIdleThreads());
    ThreadIdType maxCount = std::max(1u, GetGlobalDefaultNumberOfThreads());
    m_NumberOfThreads = std::min(maxCount, idleCount);
    }
  else
    {
    m_NumberOfThreads = MultiThreader::GetGlobalDefaultNumberOfThreads();
    }
}

MultiThreader::~MultiThreader()
{
}

// Set the user defined method that will be run on NumberOfThreads threads
// when SingleMethodExecute is called.
void MultiThreader::SetSingleMethod(ThreadFunctionType f, void *data)
{
  m_SingleMethod = f;
  m_SingleData   = data;
}

// Set one of the user defined methods that will be run on NumberOfThreads
// threads when MultipleMethodExecute is called. This method should be
// called with index = 0, 1, ..,  NumberOfThreads-1 to set up all the
// required user defined methods
void MultiThreader::SetMultipleMethod(ThreadIdType index, ThreadFunctionType f, void *data)
{
  // You can only set the method for 0 through NumberOfThreads-1
  if( index >= m_NumberOfThreads )
    {
    itkExceptionMacro(<< "Can't set method " << index << " with a thread count of " << m_NumberOfThreads);
    }
  else
    {
    m_MultipleMethod[index] = f;
    m_MultipleData[index]   = data;
    }
}

// Execute the method set as the SingleMethod on NumberOfThreads threads.
void MultiThreader::SingleMethodExecute()
{
  ThreadIdType        thread_loop = 0;
  ThreadProcessIdType process_id[ITK_MAX_THREADS];

  if( !m_SingleMethod )
    {
    itkExceptionMacro(<< "No single method set!");
    }

  // obey the global maximum number of threads limit
  m_NumberOfThreads = std::min( m_GlobalMaximumNumberOfThreads, m_NumberOfThreads );

  // Init process_id table because a valid process_id (i.e., non-zero), is
  // checked in the WaitForSingleMethodThread loops
  for( thread_loop = 1; thread_loop < m_NumberOfThreads; ++thread_loop )
    {
    process_id[thread_loop] = 0;
    }

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
    for( thread_loop = 1; thread_loop < m_NumberOfThreads; ++thread_loop )
      {
      m_ThreadInfoArray[thread_loop].UserData = m_SingleData;
      m_ThreadInfoArray[thread_loop].NumberOfThreads = m_NumberOfThreads;
      m_ThreadInfoArray[thread_loop].ThreadFunction = m_SingleMethod;

      if(this->m_UseThreadPool)
        {
        this->ThreadPoolDispatchSingleMethodThread(&m_ThreadInfoArray[thread_loop]);
        }
      else
        {
        process_id[thread_loop] =
          this->SpawnDispatchSingleMethodThread(&m_ThreadInfoArray[thread_loop]);
        }
      }
    }
  catch( std::exception & e )
    {
    // get the details of the exception to rethrow them
    exceptionDetails = e.what();
    // If creation of any thread failed, we must make sure that all
    // threads are correctly cleaned
    exceptionOccurred = true;
    }
  catch( ... )
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
    m_ThreadInfoArray[0].NumberOfThreads = m_NumberOfThreads;
    m_SingleMethod( (void *)( &m_ThreadInfoArray[0] ) );
    }
  catch( ProcessAborted & )
    {
    // Need cleanup and rethrow ProcessAborted
    // close down other threads
    for( thread_loop = 1; thread_loop < m_NumberOfThreads; ++thread_loop )
      {
      try
        {
        if(this->m_UseThreadPool)
          {
          try
            {
            m_ThreadPool->WaitForJob(m_ThreadInfoArray[thread_loop].Semaphore);
            }
          catch (ExceptionObject& exc)
            {
            std::cerr << exc << std::endl;
            throw;
            }
          }
        else
          {
          this->SpawnWaitForSingleMethodThread(process_id[thread_loop]);
          }
        }
      catch( ... )
        {
        }
      }
    // rethrow
    throw;
    }
  catch( std::exception & e )
    {
    // get the details of the exception to rethrow them
    exceptionDetails = e.what();
    // if this method fails, we must make sure all threads are
    // correctly cleaned
    exceptionOccurred = true;
    }
  catch( ... )
    {
    // if this method fails, we must make sure all threads are
    // correctly cleaned
    exceptionOccurred = true;
    }
  // The parent thread has finished this->SingleMethod() - so now it
  // waits for each of the other processes to exit
  for( thread_loop = 1; thread_loop < m_NumberOfThreads; ++thread_loop )
    {
    try
      {
      if(this->m_UseThreadPool)
        {
        try
          {
          m_ThreadPool->WaitForJob(m_ThreadInfoArray[thread_loop].Semaphore);
          }
        catch (ExceptionObject& exc)
          {
          std::cerr << exc << std::endl;
          throw;
          }
        }
      else
        {
        this->SpawnWaitForSingleMethodThread(process_id[thread_loop]);
        }
      if( m_ThreadInfoArray[thread_loop].ThreadExitCode
          != ThreadInfoStruct::SUCCESS )
        {
        exceptionOccurred = true;
        }
      }
    catch( std::exception & e )
      {
      // get the details of the exception to rethrow them
      exceptionDetails = e.what();
      exceptionOccurred = true;
      }
    catch( ... )
      {
      exceptionOccurred = true;
      }
    }

  if( exceptionOccurred )
    {
    if( exceptionDetails.empty() )
      {
      itkExceptionMacro("Exception occurred during SingleMethodExecute");
      }
    else
      {
      itkExceptionMacro(<< "Exception occurred during SingleMethodExecute" << std::endl << exceptionDetails);
      }
    }
}

void
MultiThreader
::ThreadPoolDispatchSingleMethodThread(MultiThreader::ThreadInfoStruct *threadInfo)
{
  ThreadJob threadJob;
  threadJob.m_ThreadFunction = (this->SingleMethodProxy);
  threadJob.m_UserData = (void *) threadInfo;
  threadJob.m_Semaphore = &threadInfo->Semaphore;
  try
    {
    m_ThreadPool->AddWork(threadJob);
    }
  catch (ExceptionObject& exc)
    {
    std::cerr << exc << std::endl;
    throw;
    }
}

ITK_THREAD_RETURN_TYPE
MultiThreader
::SingleMethodProxy(void *arg)
{
  // grab the ThreadInfoStruct originally prescribed
  MultiThreader::ThreadInfoStruct
  * threadInfoStruct =
    reinterpret_cast<MultiThreader::ThreadInfoStruct *>( arg );

  // execute the user specified threader callback, catching any exceptions
  try
    {
    ( *threadInfoStruct->ThreadFunction )(threadInfoStruct);
    threadInfoStruct->ThreadExitCode = MultiThreader::ThreadInfoStruct::SUCCESS;
    }
  catch( ProcessAborted & )
    {
    threadInfoStruct->ThreadExitCode =
      MultiThreader::ThreadInfoStruct::ITK_PROCESS_ABORTED_EXCEPTION;
    }
  catch( ExceptionObject & )
    {
    threadInfoStruct->ThreadExitCode =
      MultiThreader::ThreadInfoStruct::ITK_EXCEPTION;
    }
  catch( std::exception & )
    {
    threadInfoStruct->ThreadExitCode =
      MultiThreader::ThreadInfoStruct::STD_EXCEPTION;
    }
  catch( ... )
    {
    threadInfoStruct->ThreadExitCode = MultiThreader::ThreadInfoStruct::UNKNOWN;
    }

  return ITK_THREAD_RETURN_VALUE;
}

// Print method for the multithreader
void MultiThreader::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Thread Count: " << m_NumberOfThreads << "\n";
  os << indent << "Global Maximum Number Of Threads: "
     << m_GlobalMaximumNumberOfThreads << std::endl;
  os << indent << "Global Default Number Of Threads: "
     << m_GlobalDefaultNumberOfThreads << std::endl;
}

}
