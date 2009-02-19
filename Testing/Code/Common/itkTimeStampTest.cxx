/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeStampTest.cxx
  Language:  C++

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTimeStamp.h"
#include "itkMultiThreader.h"

typedef struct {
  itk::TimeStamp * m_TimeStamp;
  signed long counters[ITK_MAX_THREADS];
} TimeStampTestHelper;
 
ITK_THREAD_RETURN_TYPE modified_function( void *ptr )
{
  typedef itk::MultiThreader::ThreadInfoStruct  ThreadInfoType;

  ThreadInfoType * infoStruct = static_cast< ThreadInfoType * >( ptr );

  const unsigned int threadId = infoStruct->ThreadID;

  TimeStampTestHelper * helper = 
    static_cast< TimeStampTestHelper * >( infoStruct->UserData );

  itk::TimeStamp *tsp = helper->m_TimeStamp;
  helper->counters[threadId]++;

  tsp->Modified();

  return ITK_THREAD_RETURN_VALUE;
}

// Fake the non-optimized timestamp by surrounding
// the call to TimeStamp::Modified with a mutex
static itk::SimpleFastMutexLock TimeStampTestMutex;

ITK_THREAD_RETURN_TYPE modified_function_base( void *ptr )
{
  typedef itk::MultiThreader::ThreadInfoStruct  ThreadInfoType;

  ThreadInfoType * infoStruct = static_cast< ThreadInfoType * >( ptr );

  const unsigned int threadId = infoStruct->ThreadID;

  TimeStampTestHelper * helper = 
    static_cast< TimeStampTestHelper * >( infoStruct->UserData );

  itk::TimeStamp *tsp = helper->m_TimeStamp;
  helper->counters[threadId]++;

  TimeStampTestMutex.Lock();
  tsp->Modified();
  TimeStampTestMutex.Unlock();

  return ITK_THREAD_RETURN_VALUE;
}

bool TimeStampTest( itk::ThreadFunctionType modfunc, bool addmodifiedcall=false )
{
  bool success = true;
  
  TimeStampTestHelper helper;

  for(unsigned int k=0; k < ITK_MAX_THREADS; k++)
    {
    helper.counters[k] = 0;
    }

  try
    {
    itk::TimeStamp ts;
       
    itk::MultiThreader::Pointer multithreader = itk::MultiThreader::New();
    multithreader->SetNumberOfThreads(ITK_MAX_THREADS+10);// this will be clamped

    const signed long numberOfThreads = 
      static_cast<signed long>( multithreader->GetNumberOfThreads() );

    if( numberOfThreads > ITK_MAX_THREADS )
      {
      std::cerr << "[PARTIAL TEST FAILED]" << std::endl;
      std::cerr << "numberOfThreads > ITK_MAX_THREADS" << std::endl;
      return false;
      }

    std::cout << "Global Maximum Number of Threads = " << 
      multithreader->GetGlobalMaximumNumberOfThreads() << std::endl;
    std::cout << "Global Default Number of Threads = " << 
      multithreader->GetGlobalDefaultNumberOfThreads() << std::endl;
    
    std::cout << "Number of Threads = " << numberOfThreads << std::endl;

    helper.m_TimeStamp = &ts;

    multithreader->SetSingleMethod( modfunc, &helper);

    // call modified once to make it up-to-date;
    ts.Modified();

    const signed long init_mtime = ts.GetMTime();
    std::cout << "init_mtime: " << init_mtime << std::endl;

    signed long prev_mtime = init_mtime;

    const signed int num_exp = 2000;

    for( signed int i = 0; i < num_exp; i++ )
      {
      multithreader->SingleMethodExecute();

      // This is to check whether if we update the modified
      // time in the main thread, we do get a good result or not
      if ( addmodifiedcall ) ts.Modified();

      signed long current_mtime = ts.GetMTime();

      // This is to use the same tests as the standard test
      if ( addmodifiedcall ) current_mtime-=1;

      for( signed int j = 0; j < numberOfThreads; j++ )
        {
        if( helper.counters[j] != i+1 )
          {
          std::cerr << "counter[" << j << "] = " << helper.counters[j];
          std::cerr << " at iteration " << i << std::endl;
          }
        }

      if( ( current_mtime - prev_mtime ) > numberOfThreads )
        {
        // This might be a normal case since the modified time of a time stamp
        // is global If a new itk object is created this will also increment
        // the time
        std::cout << "[Iteration " << i << "]" << std::endl;
        std::cout << "current_mtime   : " << current_mtime << std::endl;
        std::cout << "prev_mtime      : " << prev_mtime << std::endl;
        std::cout << "num_threads     : " << numberOfThreads << std::endl;
        std::cout << "cur - prev mtime: " << current_mtime - prev_mtime << std::endl;
        std::cout << std::endl;
        }
      else
        {
        if( ( current_mtime - prev_mtime ) < numberOfThreads )
          {
          // This is a failure
          std::cerr << "[Iteration " << i << " FAILED]" << std::endl;
          std::cerr << "current_mtime   : " << current_mtime << std::endl;
          std::cerr << "prev_mtime      : " << prev_mtime << std::endl;
          std::cerr << "num_threads     : " << numberOfThreads << std::endl;
          std::cerr << "cur - prev mtime: " << current_mtime - prev_mtime << std::endl;
          std::cerr << std::endl;
          success = false;
          }
        }

      prev_mtime = current_mtime;

      // This is to use the same tests as the standard test
      if ( addmodifiedcall ) prev_mtime+=1;
      }
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << "[PARTIAL TEST FAILED]" << std::endl;
    std::cerr << "Exception caught: "<< e << std::endl;
    return false;
    }

  if (!success)
    {
    std::cerr << "[PARTIAL TEST FAILED]" << std::endl;
    return false;
    }

  std::cout << "[PARTIAL TEST PASSED]" << std::endl;
  return true;
}

int itkTimeStampTest(int, char*[])
{
   const bool baseres = TimeStampTest( modified_function_base );
  if ( !baseres )
    {
    std::cerr << "[BASE TEST FAILED]" << std::endl;
    }
  
  const bool optres  = TimeStampTest( modified_function );
  if ( !optres )
    {
    std::cerr << "[OPTIMIZED TEST FAILED]" << std::endl;
    }
  
  const bool baseresadd = TimeStampTest( modified_function_base, true );
  if ( !baseresadd )
    {
    std::cerr << "[BASE TEST 2 FAILED]" << std::endl;
    }
  
  const bool optresadd  = TimeStampTest( modified_function, true );
  if ( !optresadd )
    {
    std::cerr << "[OPTIMIZED TEST 2 FAILED]" << std::endl;
    }
  
  if ( !(baseres && optres && baseresadd && optresadd) )
    {
    std::cerr << "[TEST FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[TEST PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
