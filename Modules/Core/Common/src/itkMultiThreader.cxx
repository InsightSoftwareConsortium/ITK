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
#include "itkObjectFactory.h"
#include "itksys/SystemTools.hxx"
#include <stdlib.h>


#if defined(ITK_USE_PTHREADS)
#include "itkMultiThreaderPThreads.cxx"
#elif defined(ITK_USE_WIN32_THREADS)
#include "itkMultiThreaderWinThreads.cxx"
#else
#include "itkMultiThreaderNoThreads.cxx"
#endif


namespace itk
{
// Initialize static member that controls global maximum number of threads.
ThreadIdType MultiThreader:: m_GlobalMaximumNumberOfThreads = ITK_MAX_THREADS;

// Initialize static member that controls global default number of threads : 0
// => Not initialized.
ThreadIdType MultiThreader:: m_GlobalDefaultNumberOfThreads = 0;

void MultiThreader::SetGlobalMaximumNumberOfThreads(ThreadIdType val)
{
  m_GlobalMaximumNumberOfThreads = val;

  if ( m_GlobalMaximumNumberOfThreads > ITK_MAX_THREADS )
    {
    m_GlobalMaximumNumberOfThreads = ITK_MAX_THREADS;
    }

  if ( m_GlobalMaximumNumberOfThreads < 1 )
    {
    m_GlobalMaximumNumberOfThreads = 1;
    }

  // If necessary reset the default to be used from now on.
  if ( m_GlobalDefaultNumberOfThreads > m_GlobalMaximumNumberOfThreads )
    {
    m_GlobalDefaultNumberOfThreads = m_GlobalMaximumNumberOfThreads;
    }
}

ThreadIdType MultiThreader::GetGlobalMaximumNumberOfThreads()
{
  return m_GlobalMaximumNumberOfThreads;
}

void MultiThreader::SetGlobalDefaultNumberOfThreads(ThreadIdType val)
{
  m_GlobalDefaultNumberOfThreads = val;

  if ( m_GlobalDefaultNumberOfThreads > m_GlobalMaximumNumberOfThreads )
    {
    m_GlobalDefaultNumberOfThreads = m_GlobalMaximumNumberOfThreads;
    }

  if ( m_GlobalDefaultNumberOfThreads < 1 )
    {
    m_GlobalDefaultNumberOfThreads = 1;
    }
}

void MultiThreader::SetNumberOfThreads(ThreadIdType numberOfThreads)
{
  if ( m_NumberOfThreads == numberOfThreads &&
       numberOfThreads <= m_GlobalMaximumNumberOfThreads )
    {
    return;
    }

  m_NumberOfThreads = numberOfThreads;

  if ( m_NumberOfThreads > m_GlobalMaximumNumberOfThreads )
    {
    m_NumberOfThreads = m_GlobalMaximumNumberOfThreads;
    }

  if ( m_NumberOfThreads < 1 )
    {
    m_NumberOfThreads = 1;
    }
}


ThreadIdType MultiThreader::GetGlobalDefaultNumberOfThreads()
{
  // if default number has been set then don't try to update it; just
  // return the value
  if ( m_GlobalDefaultNumberOfThreads != 0 )
    {
    return m_GlobalDefaultNumberOfThreads;
    }

  // first, check for enviornment variable
  itksys_stl::string itkGlobalDefaultNumberOfThreadsEnv = "0";
  if ( itksys::SystemTools::GetEnv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS",
                                   itkGlobalDefaultNumberOfThreadsEnv) )
    {
    m_GlobalDefaultNumberOfThreads =
      atoi( itkGlobalDefaultNumberOfThreadsEnv.c_str() );
    }

  // otherwise, set number of threads based on system information
  if ( m_GlobalDefaultNumberOfThreads <= 0 )
    {
    ThreadIdType num;
    num = GetGlobalDefaultNumberOfThreadsByPlatform();
    m_GlobalDefaultNumberOfThreads = num;
    }

  // limit the number of threads to m_GlobalMaximumNumberOfThreads
  if ( m_GlobalDefaultNumberOfThreads > m_GlobalMaximumNumberOfThreads )
    {
    m_GlobalDefaultNumberOfThreads = m_GlobalMaximumNumberOfThreads;
    }

  // verify that the default number of threads is larger than zero
  if ( m_GlobalDefaultNumberOfThreads < 1 )
    {
    m_GlobalDefaultNumberOfThreads = 1;
    }

  return m_GlobalDefaultNumberOfThreads;
}


// Constructor. Default all the methods to NULL. Since the
// ThreadInfoArray is static, the ThreadIDs can be initialized here
// and will not change.
MultiThreader::MultiThreader()
{
  for ( ThreadIdType i = 0; i < ITK_MAX_THREADS; i++ )
    {
    m_ThreadInfoArray[i].ThreadID           = i;
    m_ThreadInfoArray[i].ActiveFlag         = 0;
    m_ThreadInfoArray[i].ActiveFlagLock     = 0;

    m_MultipleMethod[i]                     = 0;
    m_MultipleData[i]                       = 0;

    m_SpawnedThreadActiveFlag[i]            = 0;
    m_SpawnedThreadActiveFlagLock[i]        = 0;
    m_SpawnedThreadInfoArray[i].ThreadID    = i;
    }

  m_SingleMethod = 0;
  m_SingleData = 0;
  m_NumberOfThreads = this->GetGlobalDefaultNumberOfThreads();
}

MultiThreader::~MultiThreader()
{}

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
  if ( index >= m_NumberOfThreads )
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
  ThreadIdType                 thread_loop = 0;
  ThreadProcessIDType process_id[ITK_MAX_THREADS];

  if ( !m_SingleMethod )
    {
    itkExceptionMacro(<< "No single method set!");
    return;
    }

  // obey the global maximum number of threads limit
  if ( m_NumberOfThreads > m_GlobalMaximumNumberOfThreads )
    {
    m_NumberOfThreads = m_GlobalMaximumNumberOfThreads;
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
    for ( thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
      {
      m_ThreadInfoArray[thread_loop].UserData    = m_SingleData;
      m_ThreadInfoArray[thread_loop].NumberOfThreads = m_NumberOfThreads;
      m_ThreadInfoArray[thread_loop].ThreadFunction = m_SingleMethod;

      process_id[thread_loop] =
        this->DispatchSingleMethodThread(&m_ThreadInfoArray[thread_loop]);
      }
    }
  catch ( std::exception & e )
    {
    // get the details of the exception to rethrow them
    exceptionDetails = e.what();
    // If creation of any thread failed, we must make sure that all
    // threads are correctly cleaned
    exceptionOccurred = true;
    }
  catch ( ... )
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
  catch ( ProcessAborted & excp )
    {
    // Need cleanup and rethrow ProcessAborted
    // close down other threads
    for ( thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
      {
      try
        {
        this->WaitForSingleMethodThread(process_id[thread_loop]);
        }
      catch ( ... )
              {}
      }
    // rethrow
    throw excp;
    }
  catch ( std::exception & e )
    {
    // get the details of the exception to rethrow them
    exceptionDetails = e.what();
    // if this method fails, we must make sure all threads are
    // correctly cleaned
    exceptionOccurred = true;
    }
  catch ( ... )
    {
    // if this method fails, we must make sure all threads are
    // correctly cleaned
    exceptionOccurred = true;
    }

  // The parent thread has finished this->SingleMethod() - so now it
  // waits for each of the other processes to exit
  for ( thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
    {
    try
      {
      this->WaitForSingleMethodThread(process_id[thread_loop]);
      if ( m_ThreadInfoArray[thread_loop].ThreadExitCode
           != ThreadInfoStruct::SUCCESS )
        {
        exceptionOccurred = true;
        }
      }
    catch ( std::exception & e )
      {
      // get the details of the exception to rethrow them
      exceptionDetails = e.what();
      exceptionOccurred = true;
      }
    catch ( ... )
      {
      exceptionOccurred = true;
      }
    }

  if ( exceptionOccurred )
    {
    if ( exceptionDetails.empty() )
      {
      itkExceptionMacro("Exception occurred during SingleMethodExecute");
      }
    else
      {
      itkExceptionMacro(<< "Exception occurred during SingleMethodExecute" << std::endl << exceptionDetails);
      }
    }
}
ITK_THREAD_RETURN_TYPE
MultiThreader
::SingleMethodProxy(void *arg)
{
  // grab the ThreadInfoStruct originally prescribed
  MultiThreader::ThreadInfoStruct
  * threadInfoStruct =
    reinterpret_cast< MultiThreader::ThreadInfoStruct * >( arg );

  // execute the user specified threader callback, catching any exceptions
  try
    {
    ( *threadInfoStruct->ThreadFunction )(threadInfoStruct);
    threadInfoStruct->ThreadExitCode = MultiThreader::ThreadInfoStruct::SUCCESS;
    }
  catch ( ProcessAborted & )
    {
    threadInfoStruct->ThreadExitCode =
      MultiThreader::ThreadInfoStruct::ITK_PROCESS_ABORTED_EXCEPTION;
    }
  catch ( ExceptionObject & )
    {
    threadInfoStruct->ThreadExitCode =
      MultiThreader::ThreadInfoStruct::ITK_EXCEPTION;
    }
  catch ( std::exception & )
    {
    threadInfoStruct->ThreadExitCode =
      MultiThreader::ThreadInfoStruct::STD_EXCEPTION;
    }
  catch ( ... )
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
