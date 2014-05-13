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

#include <iostream>
#include "itkConditionVariable.h"
#include "itkMultiThreader.h"

class ConditionVariableTestUserData
{
public:
  itk::SimpleMutexLock            m_Mutex;
  itk::ConditionVariable::Pointer m_ConditionVariable;
  unsigned int                    m_Counter;
  unsigned int                    m_CountLimit;

  ConditionVariableTestUserData()
  {
    m_ConditionVariable = itk::ConditionVariable::New();
    m_Counter = 0;
    m_CountLimit = 12;
  }
  ~ConditionVariableTestUserData() {}
};


ITK_THREAD_RETURN_TYPE ConditionVariableTestIncCount( void *ptr )
{
  ConditionVariableTestUserData *data = static_cast<ConditionVariableTestUserData *>(
                       ( (itk::MultiThreader::ThreadInfoStruct *)(ptr) )->UserData );

  double v = 400.0;

  for (unsigned i = 0; i < 10; i++)
    {
    data->m_Mutex.Lock();
    data->m_Counter++;
    // Check the value of count.  If the condition has been reached, signal
    // waiting threads.
    if (data->m_Counter == data->m_CountLimit)
      {
      //      data->m_ConditionVariable->Signal();
      // Tests both Broadcast and Signal
      data->m_ConditionVariable->Broadcast();
      }

    data->m_Mutex.Unlock();

    // Do some work here.
    for (unsigned j = 0; j < 1000; j++)
      {
      v = v / 2.4;
      v = v * 2.4;
      }
    }

  return ITK_THREAD_RETURN_VALUE;
}

ITK_THREAD_RETURN_TYPE ConditionVariableTestWatchCount( void *ptr )
{
  ConditionVariableTestUserData *data = static_cast<ConditionVariableTestUserData *>(
                           ( (itk::MultiThreader::ThreadInfoStruct *)(ptr) )->UserData );

  // Lock the mutex and wait for the signal.
  data->m_Mutex.Lock();
  while ( data->m_Counter < data->m_CountLimit )
    {
    data->m_ConditionVariable->Wait( &data->m_Mutex );
    }
  data->m_Mutex.Unlock();

  return ITK_THREAD_RETURN_VALUE;
}

ITK_THREAD_RETURN_TYPE ConditionVariableTestCallback( void *ptr )
{
  itk::ThreadIdType threadId = ( (itk::MultiThreader::ThreadInfoStruct *)(ptr) )->ThreadID;

  if ( threadId == 0 )
    {
    ConditionVariableTestWatchCount( ptr );
    }
  else
    {
    ConditionVariableTestIncCount( ptr );
    }
  return ITK_THREAD_RETURN_VALUE;
}

int itkConditionVariableTest(int , char*[])
{
  ConditionVariableTestUserData cond;

  try
    {
    itk::MultiThreader::Pointer multithreader = itk::MultiThreader::New();
    multithreader->SetNumberOfThreads(3);
    multithreader->SetSingleMethod( ConditionVariableTestCallback, &cond);

    for (unsigned int i = 0; i < 1000; i++)
      {
      cond.m_Counter = 0;
      multithreader->SingleMethodExecute();
      }
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return 2;
    }

  std::cout << "[TEST PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
