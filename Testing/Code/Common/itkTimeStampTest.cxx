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

  tsp-> Modified();
 
  return ITK_THREAD_RETURN_VALUE;
}

int itkTimeStampTest(int, char*[])
{
  try
    {  
    itk::TimeStamp ts;

    const unsigned int num_threads = ITK_MAX_THREADS;
    const unsigned int num_exp = 2000;
    
    itk::MultiThreader::Pointer multithreader = itk::MultiThreader::New();
    multithreader->SetNumberOfThreads(num_threads);
    multithreader->SetSingleMethod( modified_function, &ts);
    multithreader->SingleMethodExecute();

    const unsigned long init_mtime = ts.GetMTime();
    
    for (unsigned int i = 0; i < num_exp; i++)
      {
      multithreader->SingleMethodExecute();

      const unsigned long current_mtime = ts.GetMTime();

      if ( (current_mtime-init_mtime)!=num_threads*(i+1) )
        {
        std::cout << "[TEST FAILED]" << std::endl;
        std::cout << "init_mtime: "<<init_mtime<< std::endl;
        std::cout << "current_mtime: "<<current_mtime<< std::endl;
        std::cout << "num_threads: "<<num_threads<< std::endl;
        std::cout << "current_exp: "<<i+1<< std::endl;
        std::cout << "num_threads*current_exp: "<<num_threads*(i+1)<< std::endl;
        return EXIT_FAILURE;
        }
      }

    const unsigned long final_mtime = ts.GetMTime();

    if ( (final_mtime-init_mtime)!=num_threads*num_exp )
      {
      std::cout << "[TEST FAILED]" << std::endl;
      std::cout << "init_mtime: "<<init_mtime<< std::endl;
      std::cout << "final_mtime: "<<final_mtime<< std::endl;
      std::cout << "num_threads: "<<num_threads<< std::endl;
      std::cout << "num_exp: "<<num_exp<< std::endl;
      std::cout << "num_threads*num_exp: "<<num_threads*num_exp<< std::endl;
      return EXIT_FAILURE;
      }
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return 2;
    }

  std::cout << "[TEST PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
