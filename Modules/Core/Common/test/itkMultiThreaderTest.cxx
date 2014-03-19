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

bool VerifyRange(int value, int min, int max, const char * msg)
{
  if( value < min )
    {
    std::cerr << msg << std::endl;
    return false;
    }

  if( value > max )
    {
    std::cerr << msg << std::endl;
    return false;
    }
  return true;
}


bool SetAndVerifyGlobalMaximumNumberOfThreads( int value )
{
  itk::MultiThreader::SetGlobalMaximumNumberOfThreads( value );
  return VerifyRange( itk::MultiThreader::GetGlobalMaximumNumberOfThreads(),
        1, ITK_MAX_THREADS, "Range error in MaximumNumberOfThreads");
}

bool SetAndVerifyGlobalDefaultNumberOfThreads( int value )
{
  itk::MultiThreader::SetGlobalDefaultNumberOfThreads( value );
  return VerifyRange( itk::MultiThreader::GetGlobalDefaultNumberOfThreads(),
        1, itk::MultiThreader::GetGlobalMaximumNumberOfThreads(),
        "Range error in DefaultNumberOfThreads");
}

bool SetAndVerifyNumberOfThreads( int value, itk::MultiThreader * threader )
{
  threader->SetNumberOfThreads( value );
  return VerifyRange( threader->GetNumberOfThreads(),
        1, itk::MultiThreader::GetGlobalMaximumNumberOfThreads(),
        "Range error in NumberOfThreads");
}

int itkMultiThreaderTest(int argc, char* argv[])
{
  // Choose a number of threads.
  int numberOfThreads = 10;
  if( argc > 1 )
    {
    const int nt = atoi( argv[1] );
    if(nt > 1)
      {
      numberOfThreads = nt;
      }
    }

  itk::MultiThreader::Pointer    threader = itk::MultiThreader::New();
  if(threader.IsNull())
    {
    return EXIT_FAILURE;
    }

  itk::MultiThreader::SetGlobalDefaultNumberOfThreads( numberOfThreads );

  {
  // Test settings for GlobalMaximumNumberOfThreads

  bool result = true;

  result &= SetAndVerifyGlobalMaximumNumberOfThreads( -1 );
  result &= SetAndVerifyGlobalMaximumNumberOfThreads(  0 );
  result &= SetAndVerifyGlobalMaximumNumberOfThreads(  1 );
  result &= SetAndVerifyGlobalMaximumNumberOfThreads(  2 );
  result &= SetAndVerifyGlobalMaximumNumberOfThreads(  ITK_MAX_THREADS  );
  result &= SetAndVerifyGlobalMaximumNumberOfThreads(  ITK_MAX_THREADS - 1 );
  result &= SetAndVerifyGlobalMaximumNumberOfThreads(  ITK_MAX_THREADS + 1 );

  if( !result )
    {
    return EXIT_FAILURE;
    }


  result &= SetAndVerifyGlobalDefaultNumberOfThreads( -1 );
  result &= SetAndVerifyGlobalDefaultNumberOfThreads(  0 );
  result &= SetAndVerifyGlobalDefaultNumberOfThreads(  1 );
  result &= SetAndVerifyGlobalDefaultNumberOfThreads( itk::MultiThreader::GetGlobalMaximumNumberOfThreads() );
  result &= SetAndVerifyGlobalDefaultNumberOfThreads( itk::MultiThreader::GetGlobalMaximumNumberOfThreads() - 1 );
  result &= SetAndVerifyGlobalDefaultNumberOfThreads( itk::MultiThreader::GetGlobalMaximumNumberOfThreads() + 1 );

  if( !result )
    {
    return EXIT_FAILURE;
    }

  itk::MultiThreader::Pointer threader2 = itk::MultiThreader::New();

  result &= SetAndVerifyNumberOfThreads( -1, threader2 );
  result &= SetAndVerifyNumberOfThreads(  0, threader2 );
  result &= SetAndVerifyNumberOfThreads(  1, threader2 );
  result &= SetAndVerifyNumberOfThreads( itk::MultiThreader::GetGlobalMaximumNumberOfThreads(), threader2 );
  result &= SetAndVerifyNumberOfThreads( itk::MultiThreader::GetGlobalMaximumNumberOfThreads() - 1, threader2);
  result &= SetAndVerifyNumberOfThreads( itk::MultiThreader::GetGlobalMaximumNumberOfThreads() + 1, threader2);

  if( !result )
    {
    return EXIT_FAILURE;
    }

  }

  return EXIT_SUCCESS;
}

namespace itkMultiThreaderTestHelpers
{

void ThreadedMethod()
{

#ifdef ITK_USE_PTHREADS
// ThreadProcessIdType threadId = pthread_self();
#endif

#ifdef ITK_USE_WIN32_THREADS
// ThreadProcessIdType threadId = GetCurrentThread();
#endif

}


} // end of itkMultiThreaderTestHelpers
