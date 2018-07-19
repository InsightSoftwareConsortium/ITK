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
#include "itkPoolMultiThreader.h"
#include "itkNumericTraits.h"
#include <iostream>
#include <string>
#if !defined( ITK_LEGACY_FUTURE_REMOVE )
# include "vcl_algorithm.h"
#endif
#include <algorithm>

namespace itk
{

PoolMultiThreader::PoolMultiThreader() :
  m_ThreadPool( ThreadPool::GetInstance() )
{
  for( ThreadIdType i = 0; i < ITK_MAX_THREADS; ++i )
    {
    m_ThreadInfoArray[i].WorkUnitID = i;
    }

  ThreadIdType idleCount = std::max<ThreadIdType>(1u, m_ThreadPool->GetNumberOfCurrentlyIdleThreads());
  ThreadIdType defaultThreads = std::max(1u, GetGlobalDefaultNumberOfThreads());
#if defined( ITKV4_COMPATIBILITY )
  m_NumberOfWorkUnits = std::min( defaultThreads, idleCount );
#else
  m_NumberOfWorkUnits = std::min( 3 * defaultThreads, idleCount );
#endif
  m_MaximumNumberOfThreads = m_ThreadPool->GetMaximumNumberOfThreads();
}

PoolMultiThreader::~PoolMultiThreader()
{
}

void PoolMultiThreader::SetSingleMethod(ThreadFunctionType f, void *data)
{
  m_SingleMethod = f;
  m_SingleData   = data;
}

void PoolMultiThreader::SetMaximumNumberOfThreads(ThreadIdType numberOfThreads)
{
  Superclass::SetMaximumNumberOfThreads( numberOfThreads );
  ThreadIdType threadCount = m_ThreadPool->GetMaximumNumberOfThreads();
  if ( threadCount < m_MaximumNumberOfThreads )
    {
    m_ThreadPool->AddThreads( m_MaximumNumberOfThreads - threadCount );
    }
  m_MaximumNumberOfThreads = m_ThreadPool->GetMaximumNumberOfThreads();
}

void PoolMultiThreader::SingleMethodExecute()
{
  ThreadIdType thread_loop = 0;

  if( !m_SingleMethod )
    {
    itkExceptionMacro(<< "No single method set!");
    }

  // obey the global maximum number of threads limit
  m_NumberOfWorkUnits = std::min( this->GetGlobalMaximumNumberOfThreads(), m_NumberOfWorkUnits );


  // Spawn a set of threads through the SingleMethodProxy. Exceptions
  // thrown from a thread will be caught by the SingleMethodProxy. A
  // naive mechanism is in place for determining whether a thread
  // threw an exception.
  //
  // Thanks to Hannu Helminen for suggestions on how to catch
  // exceptions thrown by threads.
  bool exceptionOccurred = false;
  std::string exceptionDetails;
  try
    {
    ThreadJob threadJob;
    threadJob.m_ThreadFunction = (this->SingleMethodProxy);

    for( thread_loop = 1; thread_loop < m_NumberOfWorkUnits; ++thread_loop )
      {
      m_ThreadInfoArray[thread_loop].UserData = m_SingleData;
      m_ThreadInfoArray[thread_loop].NumberOfWorkUnits = m_NumberOfWorkUnits;
      m_ThreadInfoArray[thread_loop].ThreadFunction = m_SingleMethod;

      threadJob.m_UserData = &m_ThreadInfoArray[thread_loop];
      threadJob.m_Semaphore = &m_ThreadInfoArray[thread_loop].Semaphore;
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
  try
    {
    m_ThreadInfoArray[0].UserData = m_SingleData;
    m_ThreadInfoArray[0].NumberOfWorkUnits = m_NumberOfWorkUnits;
    m_SingleMethod( (void *)( &m_ThreadInfoArray[0] ) );
    }
  catch( ProcessAborted & )
    {
    // Need cleanup and rethrow ProcessAborted
    // close down other threads
    for( thread_loop = 1; thread_loop < m_NumberOfWorkUnits; ++thread_loop )
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
      catch( ... )
        {
        }
      }
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
  for( thread_loop = 1; thread_loop < m_NumberOfWorkUnits; ++thread_loop )
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

void PoolMultiThreader::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}
