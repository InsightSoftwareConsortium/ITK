/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleFilterWatcherTest.cxx
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

#include <iostream>
#include "itkSimpleFilterWatcher.h"
#include "itkInvertIntensityImageFilter.h"
#include "itkImage.h"


int itkSimpleFilterWatcherTest (int, char*[])
{
  // Test out the code
  typedef itk::SimpleFilterWatcher        WatcherType;
  typedef itk::Image<char,3>              ImageType;
  typedef itk::InvertIntensityImageFilter<
    ImageType, ImageType>                 FilterType;
  FilterType::Pointer filter = FilterType::New();
  const char * comment = "comment";

  // Test constructor that takes a ProcessObject.
  WatcherType watcher1( filter, comment );

  // Test copy constructor.
  WatcherType watcher2( watcher1 );
  if ( watcher1.GetNameOfClass() != watcher2.GetNameOfClass()
       || watcher1.GetProcess() != watcher2.GetProcess()
       || watcher1.GetSteps() != watcher2.GetSteps()
       || watcher1.GetIterations() != watcher2.GetIterations()
       || watcher1.GetQuiet() != watcher2.GetQuiet()
       || watcher1.GetComment() != watcher2.GetComment() )
    //|| watcher1.GetTimeProbe() != watcher2.GetTimeProbe() )
    {
    std::cout << "Copy constructor failed." << std::endl;    
    return EXIT_FAILURE;
    }

  // Test default constructor.
  WatcherType watcher3;

  // Test assignment operator.
  watcher3 = watcher2;
  if ( watcher3.GetNameOfClass() != watcher2.GetNameOfClass()
       || watcher3.GetProcess() != watcher2.GetProcess()
       || watcher3.GetSteps() != watcher2.GetSteps()
       || watcher3.GetIterations() != watcher2.GetIterations()
       || watcher3.GetQuiet() != watcher2.GetQuiet()
       || watcher3.GetComment() != watcher2.GetComment() )
    //|| watcher3.GetTimeProbe() != watcher2.GetTimeProbe() )
    {
    std::cout << "Operator= failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Test GetNameOfClass().
  std::string name = watcher3.GetNameOfClass();
  if ( name != filter->GetNameOfClass() )
    {
    std::cout << "GetNameOfClass failed. Expected: " 
              << filter->GetNameOfClass() 
              << " but got: " 
              << watcher3.GetNameOfClass() << std::endl;
    return EXIT_FAILURE;
    }

  // Test Quiet functions.
  watcher3.QuietOff();
  watcher3.QuietOn();
  watcher3.SetQuiet( false );
  if ( watcher3.GetQuiet() != false )
    {
    std::cout << "GetQuiet() failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Test comment.
  if ( watcher3.GetComment() != comment )
    {
    std::cout << "GetComment() failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Check Pointer
  if ( watcher3.GetProcess() != filter )
    {
    std::cout << "GetProcess() failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Return success.
  std::cout << "SimpleFilterWatcher test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}
