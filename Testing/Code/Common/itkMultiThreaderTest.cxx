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

#ifdef ITK_USE_SPROC
// const ThreadProcessIDType threadId = GetCurrentThread()  ??;
#endif

}


} // end of itkMultiThreaderTestHelpers
