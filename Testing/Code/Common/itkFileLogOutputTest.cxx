/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileLogOutputTest.cxx
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
   //Warning about: identifier was truncated to '255' characters in the debug information (MVC6.0 Debug)
#pragma warning( disable : 4786 )
#endif

#include <iostream>
#include <fstream>

#include "itkFileLogOutput.h"
#include "itkLogger.h"
#include "itkConsoleLogOutput.h"


int itkFileLogOutputTest( int, char * [] )
{
  try
    {

    // Create an ITK FileLogOutputs
    itk::FileLogOutput::Pointer fileLog1 = itk::FileLogOutput::New();
    itk::FileLogOutput::Pointer fileLog2 = itk::FileLogOutput::New();

    std::ofstream fout1("test_ConsoleLogOutput_1.txt");
    fileLog1->SetFile( fout1 );

    std::ofstream fout2("test_ConsoleLogOutput_2.txt");
    fileLog2->SetFile( fout2 );

    // Create an ITK Logger
    itk::Logger::Pointer logger = itk::Logger::New();

    std::cout << "Testing itk::ConsoleLogOutput" << std::endl;

    // Setting the logger
    logger->SetName("org.itk.rootLogger");
    logger->SetPriorityLevel(itk::Logger::INFO);
    logger->SetLevelForFlushing(itk::Logger::CRITICAL);

    std::cout << "  Adding console and file stream LogOutputs" << std::endl;
    logger->AddLogOutput( fileLog1 );
    logger->AddLogOutput( fileLog2 );

    // Printing the logger's member variables
    std::cout << "  Name: " << logger->GetName() << std::endl;
    std::cout << "  Priority Level: " << logger->GetPriorityLevel() << std::endl;
    std::cout << "  Level For Flushing: " << logger->GetLevelForFlushing() << std::endl;

    logger->Print(std::cout);
    fileLog1->Print(std::cout);
    }
  catch(...)
    {
    std::cerr << "Exception caught !!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}


