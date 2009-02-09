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

ITK_THREAD_RETURN_TYPE modified_function( void *ptr )
{
  itk::TimeStamp *tsp = static_cast<itk::TimeStamp *>(
     ( (itk::MultiThreader::ThreadInfoStruct *)(ptr) )->UserData );

  tsp->Modified();
 
  return ITK_THREAD_RETURN_VALUE;
}

int itkTimeStampTest(int, char*[])
{
  bool success = true;
  
  try
    {
    itk::TimeStamp ts;
       
    itk::MultiThreader::Pointer multithreader = itk::MultiThreader::New();
    multithreader->SetNumberOfThreads(ITK_MAX_THREADS+10);// this will be clamped

    const unsigned long numberOfThreads = 
      static_cast<unsigned long>( multithreader->GetNumberOfThreads() );

    if( numberOfThreads > ITK_MAX_THREADS )
      {
      std::cerr << "numberOfThreads > ITK_MAX_THREADS" << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << "Number of Threads = " << numberOfThreads << std::endl;
    
    multithreader->SetSingleMethod( modified_function, &ts);

    // call modified once to make it up-to-date;
    ts.Modified();

    const unsigned long init_mtime = ts.GetMTime();
    std::cout << "init_mtime: " << init_mtime << std::endl;

    unsigned long prev_mtime = init_mtime;

    const unsigned int num_exp = 2000;

    for( unsigned int i = 0; i < num_exp; i++ )
      {
      multithreader->SingleMethodExecute();

      const unsigned long current_mtime = ts.GetMTime();

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
          std::cout << "[Iteration " << i << " FAILED]" << std::endl;
          std::cout << "current_mtime   : " << current_mtime << std::endl;
          std::cout << "prev_mtime      : " << prev_mtime << std::endl;
          std::cout << "num_threads     : " << numberOfThreads << std::endl;
          std::cout << "cur - prev mtime: " << current_mtime - prev_mtime << std::endl;
          std::cout << std::endl;
          success = false;
          }
        }

      prev_mtime = current_mtime;
      }
    }
  catch (itk::ExceptionObject &e)
    {
    std::cout << "[TEST FAILED]" << std::endl;
    std::cerr << "Exception caught: "<< e << std::endl;
    return EXIT_FAILURE;
    }

  if (!success)
    {
    std::cout << "[TEST FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[TEST PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
