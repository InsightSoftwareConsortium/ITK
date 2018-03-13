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
#include "itkMultiThreaderBase.h"
#include "itkMultiThreader.h"
#include "itkPoolMultiThreader.h"
#include "itkNumericTraits.h"
#include "itkMutexLockHolder.h"
#include "itksys/SystemTools.hxx"
#include "itkThreadPool.h"
#include <iostream>
#include <string>
#include <algorithm>


namespace itk
{

// GlobalDefaultUseThreadPoolIsInitialized is used only in this
// file to ensure that the ITK_USE_THREADPOOL environmenal variable
// is only used as a fall back option.  If the SetGlobalDefaultUseThreadPool
// API is ever used by the developer, the developers choice is
// respected over the environmental variable.
static bool GlobalDefaultUseThreadPoolIsInitialized=false;
static SimpleFastMutexLock globalDefaultInitializerLock;

bool MultiThreaderBase::m_GlobalDefaultUseThreadPool = true;

void MultiThreaderBase::SetGlobalDefaultUseThreadPool( const bool GlobalDefaultUseThreadPool )
  {
  m_GlobalDefaultUseThreadPool = GlobalDefaultUseThreadPool;
  GlobalDefaultUseThreadPoolIsInitialized=false;
  }

bool MultiThreaderBase::GetGlobalDefaultUseThreadPool( )
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
          MultiThreaderBase::SetGlobalDefaultUseThreadPool( true );
          }
        else
          {
          MultiThreaderBase::SetGlobalDefaultUseThreadPool( false );
          }
        }

      // always set that we are initialized
      GlobalDefaultUseThreadPoolIsInitialized=true;
      }
    }
  return m_GlobalDefaultUseThreadPool;
  }

// Initialize static member that controls global maximum number of threads.
ThreadIdType MultiThreaderBase::m_GlobalMaximumNumberOfThreads = ITK_MAX_THREADS;

// Initialize static member that controls global default number of threads : 0
// => Not initialized.
ThreadIdType MultiThreaderBase::m_GlobalDefaultNumberOfThreads = 0;

void MultiThreaderBase::SetGlobalMaximumNumberOfThreads(ThreadIdType val)
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

ThreadIdType MultiThreaderBase::GetGlobalMaximumNumberOfThreads()
{
  return m_GlobalMaximumNumberOfThreads;
}

void MultiThreaderBase::SetGlobalDefaultNumberOfThreads(ThreadIdType val)
{
  m_GlobalDefaultNumberOfThreads = val;

  // clamp between 1 and m_GlobalMaximumNumberOfThreads
  m_GlobalDefaultNumberOfThreads  = std::min( m_GlobalDefaultNumberOfThreads,
                                              m_GlobalMaximumNumberOfThreads );
  m_GlobalDefaultNumberOfThreads  = std::max( m_GlobalDefaultNumberOfThreads,
                                              NumericTraits<ThreadIdType>::OneValue() );

}

void MultiThreaderBase::SetNumberOfThreads(ThreadIdType numberOfThreads)
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

ThreadIdType MultiThreaderBase::GetGlobalDefaultNumberOfThreads()
{
  if( m_GlobalDefaultNumberOfThreads == 0 ) //need to initialize
    {
    m_GlobalDefaultNumberOfThreads = ThreadPool::GetGlobalDefaultNumberOfThreads();
    }
  return m_GlobalDefaultNumberOfThreads;
}


MultiThreaderBase::Pointer MultiThreaderBase::New()
{
  Pointer smartPtr = ::itk::ObjectFactory< MultiThreaderBase >::Create();
  if ( smartPtr == nullptr )
    {
    if ( GetGlobalDefaultUseThreadPool() )
      {
      return PoolMultiThreader::New();
      }
    else
      {
      return MultiThreader::New();
      }
    }
  smartPtr->UnRegister();
  return smartPtr;
}

MultiThreaderBase::MultiThreaderBase()
{
  m_NumberOfThreads = MultiThreaderBase::GetGlobalDefaultNumberOfThreads();
}

MultiThreaderBase::~MultiThreaderBase()
{
}

ITK_THREAD_RETURN_TYPE
MultiThreaderBase
::SingleMethodProxy(void *arg)
{
  // grab the ThreadInfoStruct originally prescribed
  auto * threadInfoStruct = static_cast<MultiThreaderBase::ThreadInfoStruct *>( arg );

  // execute the user specified threader callback, catching any exceptions
  try
    {
    ( *threadInfoStruct->ThreadFunction )(arg);
    threadInfoStruct->ThreadExitCode = ThreadInfoStruct::SUCCESS;
    }
  catch( ProcessAborted & )
    {
    threadInfoStruct->ThreadExitCode = ThreadInfoStruct::ITK_PROCESS_ABORTED_EXCEPTION;
    }
  catch( ExceptionObject & )
    {
    threadInfoStruct->ThreadExitCode = ThreadInfoStruct::ITK_EXCEPTION;
    }
  catch( std::exception & )
    {
    threadInfoStruct->ThreadExitCode = ThreadInfoStruct::STD_EXCEPTION;
    }
  catch( ... )
    {
    threadInfoStruct->ThreadExitCode = ThreadInfoStruct::UNKNOWN;
    }

  return ITK_THREAD_RETURN_VALUE;
}

// Print method for the multithreader
void MultiThreaderBase::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Thread Count: " << m_NumberOfThreads << "\n";
  os << indent << "Global Maximum Number Of Threads: "
     << m_GlobalMaximumNumberOfThreads << std::endl;
  os << indent << "Global Default Number Of Threads: "
     << m_GlobalDefaultNumberOfThreads << std::endl;
  os << indent << "Global Default Use ThreadPool: "
     << m_GlobalDefaultUseThreadPool << std::endl;
}

}
