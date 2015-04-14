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

void* execute(void *ptr)
{
  // Here - get any args from ptr.
  //std::cout << std::endl << "Thread fn starting.." << std::endl;
  int *data = reinterpret_cast<int *>(ptr);
  int m = 100;
  std::cout << "Ptr received  :" << ptr
            << "Value " << *data << std::endl;
  int    n = 10;
  double sum = 1.0;

  for( int j = 0; j < m; j++ )
    {
    sum = 1.0;
    for( int i = 1; i <= n; i++ )
      {
      sum = sum + i + j;
      }
    }
  //std::cout << std::endl << "Thread fn DONE" << std::endl;
  return ITK_NULLPTR;
}

#if defined(_WIN32) || defined(_WIN64)
int itkThreadPoolTest(int, char* [])
{
#else
int itkThreadPoolTest(int argc, char* argv[])
{
  int count = 1000000;
  if( argc > 1 )
    {
    const int nt = atoi( argv[1] );
    if(nt > 1)
      {
      count = nt;
      }
    }

  itk::MultiThreader::Pointer    threader = itk::MultiThreader::New();
  if(threader.IsNull())
    {
    return EXIT_FAILURE;
    }
  itk::TimeProbe timeProbe;
  itk::TimeProbe::TimeStampType startTime = timeProbe.GetInstantValue();
  int data = 100;
  for(int i=0; i<count;i++)
    {

    threader->SetSingleMethod(&execute,(void *)&data);
    threader->SingleMethodExecute();
    }
  itk::TimeProbe::TimeStampType elapsed = timeProbe.GetInstantValue() - startTime;
  std::cout<<std::endl <<" Thread pool test : Time elapsed : " << elapsed << std::endl;
#endif
  return EXIT_SUCCESS;
}
