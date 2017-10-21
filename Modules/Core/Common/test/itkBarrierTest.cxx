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

#include "itkBarrier.h"
#include "itkMultiThreader.h"
#include "itkTestingMacros.h"


class BarrierTestUserData
{
public:

  itk::Barrier::Pointer m_FirstBarrier;
  itk::Barrier::Pointer m_SecondBarrier;
  unsigned int          m_Counter[ITK_MAX_THREADS];
  unsigned int          m_NumberOfIterations;
  unsigned int          m_NumberOfThreads;
  bool                  m_TestFailure;

  BarrierTestUserData( unsigned int number_of_threads)
  {
    m_TestFailure = false;
    m_NumberOfThreads = number_of_threads;
    for (unsigned int i = 0; i < number_of_threads - 1; i++)
      { m_Counter[i] = 0; }
    m_NumberOfIterations = 50;
    m_FirstBarrier = itk::Barrier::New();
    m_SecondBarrier = itk::Barrier::New();
    m_FirstBarrier->Initialize(number_of_threads);
    m_SecondBarrier->Initialize(number_of_threads);
  }
  ~BarrierTestUserData() {}
};

ITK_THREAD_RETURN_TYPE BarrierTestIncrement( void *ptr )
{
  itk::ThreadIdType threadId = ( (itk::MultiThreader::ThreadInfoStruct *)(ptr) )->ThreadID;
  BarrierTestUserData *data = static_cast<BarrierTestUserData *>(
                  ( (itk::MultiThreader::ThreadInfoStruct *)(ptr) )->UserData );

  for (unsigned int i = 0;  i < data->m_NumberOfIterations; i++)
    {
    // set the value for this iteration
    data->m_Counter[threadId] = i;

    // wait for all the other threads
    data->m_FirstBarrier->Wait();
    data->m_SecondBarrier->Wait();
    }

  return ITK_THREAD_RETURN_VALUE;
}

ITK_THREAD_RETURN_TYPE BarrierCheckIncrement( void *ptr )
{
  BarrierTestUserData *data = static_cast<BarrierTestUserData *>(
                  ( (itk::MultiThreader::ThreadInfoStruct *)(ptr) )->UserData );

  for (unsigned int i = 0; i < data->m_NumberOfIterations; i++)
    {
    // Wait for other threads to populate the m_Counter array
    data->m_FirstBarrier->Wait();

    // Check the values in the m_Counter array
    for (unsigned int j = 0; j < data->m_NumberOfThreads - 1; j++)
      {
      if (data->m_Counter[j] != i)
        {
        data->m_TestFailure = true;
        }
      }
    data->m_SecondBarrier->Wait();
    }

  return ITK_THREAD_RETURN_VALUE;
}

ITK_THREAD_RETURN_TYPE BarrierTestCallback( void *ptr )
{
  itk::ThreadIdType threadId = ( (itk::MultiThreader::ThreadInfoStruct *)(ptr) )->ThreadID;
  BarrierTestUserData *data = static_cast<BarrierTestUserData *>(
                  ( (itk::MultiThreader::ThreadInfoStruct *)(ptr) )->UserData );

  if (threadId == data->m_NumberOfThreads - 1)
    {
    BarrierCheckIncrement( ptr );
    }
  else
    {
    BarrierTestIncrement( ptr );
    }

  return ITK_THREAD_RETURN_VALUE;
}

ITK_THREAD_RETURN_TYPE BarrierSpecialTest( void *ptr )
{
  BarrierTestUserData *data = static_cast<BarrierTestUserData *>(
                  ( (itk::MultiThreader::ThreadInfoStruct *)(ptr) )->UserData );

  for (unsigned int j = 0; j < 1000; j++ )
    {
    data->m_FirstBarrier->Wait();
    }

  return ITK_THREAD_RETURN_VALUE;
}

int itkBarrierTest(int argc, char *argv[])
{
  itk::Barrier::Pointer barrier = itk::Barrier::New();
  EXERCISE_BASIC_OBJECT_METHODS( barrier, Barrier, LightObject );

  itk::ThreadIdType number_of_threads = 4;
  if (argc > 1)
    {
    number_of_threads = ::atoi(argv[1]);
    }

  BarrierTestUserData data(number_of_threads);

  try
    {
    itk::MultiThreader::Pointer multithreader = itk::MultiThreader::New();
    itk::ThreadIdType maxThreads = multithreader->GetGlobalDefaultNumberOfThreads();
    if (multithreader->GetUseThreadPool() && maxThreads < number_of_threads)
      {
      multithreader->GetModifiableThreadPool()->AddThreads(number_of_threads - maxThreads);
      }
    multithreader->SetNumberOfThreads(number_of_threads);
    multithreader->SetSingleMethod( BarrierTestCallback, &data);

    for (unsigned int i = 0; i < 5; i++) //repeat test 5 times
      {
      multithreader->SingleMethodExecute();
      }

    // perform another test
    multithreader->SetSingleMethod( BarrierSpecialTest, &data);
    multithreader->SingleMethodExecute();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  if (data.m_TestFailure == false)
    {
    std::cout << "[TEST PASSED]" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "[TEST FAILED]" << std::endl;
    return 2;
    }
}
