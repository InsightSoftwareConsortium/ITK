/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiThreaderTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkConfigure.h"
#include "itkMultiThreader.h"
#include <stdlib.h>

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
// ThreadProcessIDType threadId = pthread_self();
#endif

#ifdef ITK_USE_WIN32_THREADS
// ThreadProcessIDType threadId = GetCurrentThread();
#endif

}


} // end of itkMultiThreaderTestHelpers
