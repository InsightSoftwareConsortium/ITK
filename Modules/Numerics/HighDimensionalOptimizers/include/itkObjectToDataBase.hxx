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
#ifndef __itkObjectToDataBase_hxx
#define __itkObjectToDataBase_hxx

#include "itkObjectToDataBase.h"
#include "vnl/vnl_math.h"

namespace itk
{

/**
 *
 */
template <class TInputObject, class TDataHolder>
ObjectToDataBase<TInputObject, TDataHolder>
::ObjectToDataBase()
{
  this->ProcessObject::SetNumberOfRequiredOutputs(0);
  this->m_Holder = NULL;
  this->m_ThreadedGenerateData = NULL;
  this->m_OverallObjectHasBeenSet = false;
  this->m_NumberOfThreadsUsed = 0;
}

/*
 * Destructor
 */
template <class TInputObject, class TDataHolder>
ObjectToDataBase<TInputObject, TDataHolder>
::~ObjectToDataBase()
{
}

//----------------------------------------------------------------------------
template <class TInputObject, class TDataHolder>
void
ObjectToDataBase<TInputObject, TDataHolder>
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
  if( ! this->m_OverallObjectHasBeenSet )
    {
    itkExceptionMacro("m_OverallObject must be set.");
    }
}

//----------------------------------------------------------------------------
template <class TInputObject, class TDataHolder>
void
ObjectToDataBase<TInputObject, TDataHolder>
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
template <class TInputObject, class TDataHolder>
ITK_THREAD_RETURN_TYPE
ObjectToDataBase<TInputObject, TDataHolder>
::ThreaderCallback( void *arg )
{
  MultiThreader::ThreadInfoStruct* info = static_cast<MultiThreader::ThreadInfoStruct *>(arg);
  ThreadStruct *str = static_cast<ThreadStruct *>(info->UserData);
  ThreadIdType threadId = info->ThreadID;
  ThreadIdType threadCount = info->NumberOfThreads;

  // first find out how many pieces extent can be split into.
  InputObjectType splitObject;
  const ThreadIdType total = str->Filter->SplitRequestedObject(threadId,
                                            threadCount,
                                            str->Filter->m_OverallObject,
                                            splitObject);

  // store the actual number of threads used
  if( threadId == 0 )
    {
    str->Filter->m_NumberOfThreadsUsed = total;
    }

  // execute the actual method with appropriate output region.
  // If the threadID is greater than the total number of regions
  // that SplitRequestedObject will create, don't use this thread.
  // Sometimes the threads dont break up very well and it is just
  // as efficient to leave a few threads idle.
  if (threadId < total)
    {
    str->Filter->m_ThreadedGenerateData(splitObject,
                                        threadId,
                                        str->Filter->m_Holder);
    }

  return ITK_THREAD_RETURN_VALUE;
}

template <class TInputObject, class TDataHolder>
ThreadIdType
ObjectToDataBase<TInputObject, TDataHolder>
::DetermineNumberOfThreadsToUse() const
{
  if( ! this->m_OverallObjectHasBeenSet )
    {
    itkExceptionMacro("m_OverallObject must be set.");
    }

  InputObjectType splitObject;

  return this->SplitRequestedObject( 0,
                                    this->GetNumberOfThreads(),
                                    this->m_OverallObject,
                                    splitObject);
}

/*
 *
 */
template <class TInputObject, class TDataHolder>
void
ObjectToDataBase<TInputObject, TDataHolder>
::SetOverallObject( const InputObjectType & object )
{
  if( object != this->m_OverallObject )
    {
    this->m_OverallObject = object;
    this->m_OverallObjectHasBeenSet = true;
    this->Modified();
    }
}

/*
 *
 */
template <class TInputObject, class TDataHolder>
void
ObjectToDataBase<TInputObject, TDataHolder>
::SetThreadedGenerateData( ThreadedGenerateDataFuncType func )
{
  m_ThreadedGenerateData = func;
}

/*
 *
 */
template <class TInputObject, class TDataHolder>
void
ObjectToDataBase<TInputObject, TDataHolder>
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
