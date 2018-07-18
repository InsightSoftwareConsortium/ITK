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
  ThreadIdType threadLoop = 0;

  if( !m_SingleMethod )
    {
    itkExceptionMacro(<< "No single method set!");
    }

  // obey the global maximum number of threads limit
  m_NumberOfWorkUnits = std::min( this->GetGlobalMaximumNumberOfThreads(), m_NumberOfWorkUnits );

  bool exceptionOccurred = false;
  std::string exceptionDetails;
  for ( threadLoop = 1; threadLoop < m_NumberOfWorkUnits; ++threadLoop )
    {
    m_ThreadInfoArray[threadLoop].UserData = m_SingleData;
    m_ThreadInfoArray[threadLoop].NumberOfWorkUnits = m_NumberOfWorkUnits;
    m_ThreadInfoArray[threadLoop].Future = m_ThreadPool->AddWork( m_SingleMethod, &m_ThreadInfoArray[threadLoop] );
    }

  try
    {
    // Now, the parent thread calls this->SingleMethod() itself
    m_ThreadInfoArray[0].UserData = m_SingleData;
    m_ThreadInfoArray[0].NumberOfWorkUnits = m_NumberOfWorkUnits;
    m_SingleMethod( (void *)( &m_ThreadInfoArray[0] ) );

    // The parent thread has finished SingleMethod()
    // so now it waits for each of the other work units to finish
    for ( threadLoop = 1; threadLoop < m_NumberOfWorkUnits; ++threadLoop )
      {
      m_ThreadInfoArray[threadLoop].Future.get();
      }
    }
  catch( ProcessAborted & )
    {
    // Need cleanup and rethrow ProcessAborted
    // close down other threads
    for( threadLoop = 1; threadLoop < m_NumberOfWorkUnits; ++threadLoop )
      {
      try
        {
        m_ThreadInfoArray[threadLoop].Future.get();
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
