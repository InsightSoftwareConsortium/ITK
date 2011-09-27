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
#ifndef __itkThreadedDomainPartitioner_hxx
#define __itkThreadedDomainPartitioner_hxx

#include "itkThreadedDomainPartitioner.h"
#include "vnl/vnl_math.h"

namespace itk
{

/**
 *
 */
template <class TDomain, class TDataHolder>
ThreadedDomainPartitioner<TDomain, TDataHolder>
::ThreadedDomainPartitioner()
{
  this->m_Threader = MultiThreader::New();
  this->m_NumberOfThreads = m_Threader->GetNumberOfThreads();
  this->m_Holder = NULL;
  this->m_ThreadedGenerateData = NULL;
  this->m_DomainHasBeenSet = false;
  this->m_NumberOfThreadsUsed = 0;
}

/*
 * Destructor
 */
template <class TDomain, class TDataHolder>
ThreadedDomainPartitioner<TDomain, TDataHolder>
::~ThreadedDomainPartitioner()
{
}

//----------------------------------------------------------------------------
template <class TDomain, class TDataHolder>
void
ThreadedDomainPartitioner<TDomain, TDataHolder>
::VerifyInputConsistency()
{
  // Make sure hold and threader worker have been defined
  if( this->m_Holder == NULL )
    {
    itkExceptionMacro("m_Holder must be defined.");
    }
  if( this->m_ThreadedGenerateData == NULL )
    {
    itkExceptionMacro("m_ThreadedGenereateData must be defined.");
    }
  if( ! this->m_DomainHasBeenSet )
    {
    itkExceptionMacro("m_Domain must be set.");
    }
}

//----------------------------------------------------------------------------
template <class TDomain, class TDataHolder>
void
ThreadedDomainPartitioner<TDomain, TDataHolder>
::StartThreadedExecution()
{
  this->VerifyInputConsistency();

  // Set up the multithreaded processing
  ThreadStruct str;
  str.Filter = this;

  MultiThreader* multiThreader = this->GetMultiThreader();
  multiThreader->SetNumberOfThreads(this->GetNumberOfThreads());
  multiThreader->SetSingleMethod(this->ThreaderCallback, &str);

  // multithread the execution
  multiThreader->SingleMethodExecute();
}

//----------------------------------------------------------------------------
// Callback routine used by the threading library. This routine just calls
// the ThreadedGenerateData method after setting the correct region for this
// thread.
template <class TDomain, class TDataHolder>
ITK_THREAD_RETURN_TYPE
ThreadedDomainPartitioner<TDomain, TDataHolder>
::ThreaderCallback( void *arg )
{
  MultiThreader::ThreadInfoStruct* info = static_cast<MultiThreader::ThreadInfoStruct *>(arg);
  ThreadStruct *str = static_cast<ThreadStruct *>(info->UserData);
  ThreadIdType threadId = info->ThreadID;
  ThreadIdType threadCount = info->NumberOfThreads;

  // first find out how many pieces extent can be split into.
  DomainType subdomain;
  const ThreadIdType total = str->Filter->PartitionDomain(threadId,
                                            threadCount,
                                            str->Filter->m_Domain,
                                            subdomain);

  // store the actual number of threads used
  if( threadId == 0 )
    {
    str->Filter->m_NumberOfThreadsUsed = total;
    }

  // execute the actual method with appropriate output region.
  // If the threadID is greater than the total number of regions
  // that PartitionDomain will create, don't use this thread.
  // Sometimes the threads dont break up very well and it is just
  // as efficient to leave a few threads idle.
  if (threadId < total)
    {
    str->Filter->m_ThreadedGenerateData(subdomain,
                                        threadId,
                                        str->Filter->m_Holder);
    }

  return ITK_THREAD_RETURN_VALUE;
}

template <class TDomain, class TDataHolder>
ThreadIdType
ThreadedDomainPartitioner<TDomain, TDataHolder>
::DetermineNumberOfThreadsToUse() const
{
  if( ! this->m_DomainHasBeenSet )
    {
    itkExceptionMacro("m_Domain must be set.");
    }

  DomainType subdomain;

  return this->PartitionDomain( 0,
                                this->GetNumberOfThreads(),
                                this->m_Domain,
                                subdomain);
}

/*
 *
 */
template <class TDomain, class TDataHolder>
void
ThreadedDomainPartitioner<TDomain, TDataHolder>
::SetCompleteDomain( const DomainType & object )
{
  if( object != this->m_Domain )
    {
    this->m_Domain = object;
    this->m_DomainHasBeenSet = true;
    this->Modified();
    }
}

/*
 *
 */
template <class TDomain, class TDataHolder>
void
ThreadedDomainPartitioner<TDomain, TDataHolder>
::SetThreadedGenerateData( ThreadedGenerateDataFuncType func )
{
  m_ThreadedGenerateData = func;
}

/*
 *
 */
template <class TDomain, class TDataHolder>
void
ThreadedDomainPartitioner<TDomain, TDataHolder>
::SetHolder( DataHolderType* holder )
{
  if( this->m_Holder != holder )
    {
    this->m_Holder = holder;
    this->Modified();
    }
}

} // end namespace itk

#endif
