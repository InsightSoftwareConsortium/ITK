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

#include "itkMultiThreader.h"
#include "itkTimeProbe.h"
#include "itkConfigure.h"
#include "itksys/SystemTools.hxx"


typedef struct {
  int   numberOfLoop;
  itk::MutexLock::Pointer sharedMutex;
} SharedThreadData;

void* ThreadFunction(void *ptr)
{

  // Retrieve shared thread data and user data
  itk::MultiThreader::ThreadInfoStruct* threadInfo =
    static_cast<itk::MultiThreader::ThreadInfoStruct*>(ptr);

  itk::ThreadIdType localthreadId    = threadInfo->ThreadID;

  SharedThreadData* localThreadData  =
    static_cast<SharedThreadData*>(threadInfo->UserData);

  int localnumberOfLoop              = localThreadData->numberOfLoop;
  itk::MutexLock::Pointer localMutex = localThreadData->sharedMutex;

  // Loop
  for (int i = 0; i < localnumberOfLoop; ++i)
    {
    localMutex->Lock();
    std::cerr << "Thread #" << localthreadId << ":: counter = " << i << std::endl;
    localMutex->Unlock();
    }

  localMutex->Lock();
  std::cerr << "Thread #" << localthreadId << " is done." << std::endl;
  localMutex->Unlock();

  return ITK_NULLPTR;
}

#if !defined(ITK_USE_PTHREADS)
int itkSpawnThreadTest(int, char* [])
{
#else
int itkSpawnThreadTest(int argc, char* argv[])
{
  int loop = 100;
  if( argc > 1 )
    {
    const int nt = atoi( argv[1] );
    if(nt > 1)
      {
      loop = nt;
      }
    }

  itk::MultiThreader::Pointer    threader  = itk::MultiThreader::New();
  itk::MutexLock::Pointer        mutexlock = itk::MutexLock::New();
  if(threader.IsNull() || mutexlock.IsNull() )
    {
    return EXIT_FAILURE;
    }

  SharedThreadData threadData;
  threadData.numberOfLoop = loop;
  threadData.sharedMutex  = mutexlock;

  int id1 = threader->SpawnThread((itk::ThreadFunctionType) &ThreadFunction, &threadData);
  int id2 = threader->SpawnThread((itk::ThreadFunctionType) &ThreadFunction, &threadData);
  int id3 = threader->SpawnThread((itk::ThreadFunctionType) &ThreadFunction, &threadData);
  int id4 = threader->SpawnThread((itk::ThreadFunctionType) &ThreadFunction, &threadData);
  int id5 = threader->SpawnThread((itk::ThreadFunctionType) &ThreadFunction, &threadData);
  int id6 = threader->SpawnThread((itk::ThreadFunctionType) &ThreadFunction, &threadData);
  int id7 = threader->SpawnThread((itk::ThreadFunctionType) &ThreadFunction, &threadData);

  for(int i=0; i<threadData.numberOfLoop; ++i)
    {
    mutexlock->Lock();
    std::cerr << "Main Thread:: counter = " << i << std::endl;
    mutexlock->Unlock();
    }

  mutexlock->Lock();
  std::cerr << "Main Thread is done." << std::endl;
  mutexlock->Unlock();

  // wait for the threads
  threader->TerminateThread(id1);
  threader->TerminateThread(id2);
  threader->TerminateThread(id3);
  threader->TerminateThread(id4);
  threader->TerminateThread(id5);
  threader->TerminateThread(id6);
  threader->TerminateThread(id7);

  std::cerr << "All theads are stopped ...." << std::endl;
#endif
  return EXIT_SUCCESS;
}
