/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkMultiThreaderBase.h"
#include "itkTimeProbe.h"
#include "itkConfigure.h"
#include <mutex>

std::shared_ptr<std::mutex> sharedMutex;
bool                        debugPrint = false;

ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
execute(void * ptr)
{
  // Here - get any args from ptr.
  auto * threadInfo = static_cast<itk::MultiThreaderBase::WorkUnitInfo *>(ptr);

  auto * data = static_cast<int *>(threadInfo->UserData);

  sharedMutex->lock();
  if (debugPrint)
  {
    std::cout << "Pointer:" << ptr << "  Value: " << *data << "  WUID: " << threadInfo->WorkUnitID << std::endl;
  }
  sharedMutex->unlock();

  int    n = 10;
  int    m = *data;
  double sum = 1.0;

  for (int j = 0; j < m; j++)
  {
    sum = 1.0;
    for (int i = 1; i <= n; i++)
    {
      sum = sum + i + j;
    }
  }

  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

int
itkMultithreadingTest(int argc, char * argv[])
{
  sharedMutex = std::make_shared<std::mutex>();

  int count = 10000;
  if (argc > 1)
  {
    const int nt = std::stoi(argv[1]);
    if (nt > 1)
    {
      count = nt;
    }
  }
  if (argc > 2)
  {
    debugPrint = std::stoi(argv[2]);
  }

  itk::MultiThreaderBase::Pointer threader = itk::MultiThreaderBase::New();
  if (threader.IsNull())
  {
    return EXIT_FAILURE;
  }
  itk::TimeProbe                timeProbe;
  itk::TimeProbe::TimeStampType startTime = timeProbe.GetInstantValue();
  int                           data = 123;
  for (int i = 0; i < count; i++)
  {
    threader->SetSingleMethod(&execute, &data);
    threader->SingleMethodExecute();
  }
  itk::TimeProbe::TimeStampType elapsed = timeProbe.GetInstantValue() - startTime;
  std::cout << std::endl << " Thread pool test : Time elapsed : " << elapsed << std::endl;

  return EXIT_SUCCESS;
}
