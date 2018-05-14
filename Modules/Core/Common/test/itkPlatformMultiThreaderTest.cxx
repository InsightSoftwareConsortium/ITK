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

#include "itkPlatformMultiThreader.h"
#include "itkTestingMacros.h"

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
  itk::MultiThreaderBase::SetGlobalMaximumNumberOfThreads( value );
  return VerifyRange( itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads(),
        1, ITK_MAX_THREADS, "Range error in MaximumNumberOfThreads");
}

bool SetAndVerifyGlobalDefaultNumberOfThreads( int value )
{
  itk::MultiThreaderBase::SetGlobalDefaultNumberOfThreads( value );
  return VerifyRange( itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads(),
        1, itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads(),
        "Range error in DefaultNumberOfThreads");
}

bool SetAndVerifyNumberOfThreads( int value, itk::PlatformMultiThreader * threader )
{
  threader->SetNumberOfThreads( value );
  return VerifyRange( threader->GetNumberOfThreads(),
        1, itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads(),
        "Range error in NumberOfThreads");
}

int itkPlatformMultiThreaderTest(int argc, char* argv[])
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

  itk::PlatformMultiThreader::Pointer threader = itk::PlatformMultiThreader::New();
  if(threader.IsNull())
    {
    return EXIT_FAILURE;
    }

  EXERCISE_BASIC_OBJECT_METHODS(threader, PlatformMultiThreader, MultiThreaderBase);

  itk::MultiThreaderBase::SetGlobalDefaultNumberOfThreads( numberOfThreads );

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
  result &= SetAndVerifyGlobalDefaultNumberOfThreads( itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads() );
  result &= SetAndVerifyGlobalDefaultNumberOfThreads( itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads() - 1 );
  result &= SetAndVerifyGlobalDefaultNumberOfThreads( itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads() + 1 );

  if( !result )
    {
    return EXIT_FAILURE;
    }

  itk::PlatformMultiThreader::Pointer threader2 = itk::PlatformMultiThreader::New();

  result &= SetAndVerifyNumberOfThreads( -1, threader2 );
  result &= SetAndVerifyNumberOfThreads(  0, threader2 );
  result &= SetAndVerifyNumberOfThreads(  1, threader2 );
  result &= SetAndVerifyNumberOfThreads( itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads(), threader2 );
  result &= SetAndVerifyNumberOfThreads( itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads() - 1, threader2);
  result &= SetAndVerifyNumberOfThreads( itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads() + 1, threader2);

  if( !result )
    {
    return EXIT_FAILURE;
    }

  }

  return EXIT_SUCCESS;
}
