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
#ifndef itkDomainThreader_hxx
#define itkDomainThreader_hxx

#include "itkDomainThreader.h"

namespace itk
{

template< typename TDomainPartitioner, typename TAssociate >
DomainThreader< TDomainPartitioner, TAssociate >
::DomainThreader()
{
  this->m_DomainPartitioner   = DomainPartitionerType::New();
  this->m_MultiThreader       = MultiThreader::New();
  this->m_NumberOfThreadsUsed = 0;
  this->m_Associate           = ITK_NULLPTR;
}

template< typename TDomainPartitioner, typename TAssociate >
DomainThreader< TDomainPartitioner, TAssociate >
::~DomainThreader()
{
}

template< typename TDomainPartitioner, typename TAssociate >
MultiThreader *
DomainThreader< TDomainPartitioner, TAssociate >
::GetMultiThreader() const
{
  return this->m_MultiThreader;
}

template< typename TDomainPartitioner, typename TAssociate >
ThreadIdType
DomainThreader< TDomainPartitioner, TAssociate >
::GetMaximumNumberOfThreads() const
{
  return this->m_MultiThreader->GetNumberOfThreads();
}

template< typename TDomainPartitioner, typename TAssociate >
void
DomainThreader< TDomainPartitioner, TAssociate >
::SetMaximumNumberOfThreads( const ThreadIdType threads )
{
  if( threads != this->GetMaximumNumberOfThreads() )
    {
    this->m_MultiThreader->SetNumberOfThreads( threads );
    this->Modified();
    }
}

template< typename TDomainPartitioner, typename TAssociate >
void
DomainThreader< TDomainPartitioner, TAssociate >
::Execute( TAssociate * enclosingClass, const DomainType & completeDomain )
{
  this->m_Associate = enclosingClass;
  this->m_CompleteDomain = completeDomain;

  this->DetermineNumberOfThreadsUsed();

  this->BeforeThreadedExecution();

  // This calls ThreadedExecution in each thread.
  this->StartThreadingSequence();

  this->AfterThreadedExecution();
}

template< typename TDomainPartitioner, typename TAssociate >
void
DomainThreader< TDomainPartitioner, TAssociate >
::DetermineNumberOfThreadsUsed()
{
  const ThreadIdType threaderNumberOfThreads = this->GetMultiThreader()->GetNumberOfThreads();

  // Attempt a single dummy partition, just to get the number of subdomains actually created
  DomainType subdomain;
  this->m_NumberOfThreadsUsed = this->m_DomainPartitioner->PartitionDomain(0,
                                            threaderNumberOfThreads,
                                            this->m_CompleteDomain,
                                            subdomain);

  if( this->m_NumberOfThreadsUsed < threaderNumberOfThreads )
    {
    // If PartitionDomain is only able to create a lesser number of subdomains,
    // ensure that superfluous threads aren't created
    // DomainThreader::SetMaximumNumberOfThreads *should* already have been called by this point,
    // but it's not fatal if it somehow gets called later
    this->GetMultiThreader()->SetNumberOfThreads(this->m_NumberOfThreadsUsed);
    }
  else if( this->m_NumberOfThreadsUsed > threaderNumberOfThreads )
    {
    itkExceptionMacro( "A subclass of ThreadedDomainPartitioner::PartitionDomain"
                      << "returned more subdomains than were requested" );
    }
}

template< typename TDomainPartitioner, typename TAssociate >
void
DomainThreader< TDomainPartitioner, TAssociate >
::StartThreadingSequence()
{
  // Set up the multithreaded processing
  ThreadStruct str;
  str.domainThreader = this;

  MultiThreader* multiThreader = this->GetMultiThreader();
  multiThreader->SetSingleMethod(this->ThreaderCallback, &str);

  // multithread the execution
  multiThreader->SingleMethodExecute();
}

template< typename TDomainPartitioner, typename TAssociate >
ITK_THREAD_RETURN_TYPE
DomainThreader< TDomainPartitioner, TAssociate >
::ThreaderCallback( void* arg )
{
  MultiThreader::ThreadInfoStruct* info = static_cast<MultiThreader::ThreadInfoStruct *>(arg);
  ThreadStruct *str = static_cast<ThreadStruct *>(info->UserData);
  DomainThreader *thisDomainThreader = str->domainThreader;
  const ThreadIdType threadId    = info->ThreadID;
  const ThreadIdType threadCount = info->NumberOfThreads;

  // Get the sub-domain to process for this thread.
  DomainType subdomain;
  const ThreadIdType total = thisDomainThreader->GetDomainPartitioner()->PartitionDomain(threadId,
                                            threadCount,
                                            thisDomainThreader->m_CompleteDomain,
                                            subdomain);

  // Execute the actual method with appropriate sub-domain.
  // If the threadId is greater than the total number of regions
  // that PartitionDomain will create, don't use this thread.
  // Sometimes the threads dont break up very well and it is just
  // as efficient to leave a few threads idle.
  if ( threadId < total )
    {
    thisDomainThreader->ThreadedExecution( subdomain, threadId );
    }

  return ITK_THREAD_RETURN_VALUE;
}
}

#endif
