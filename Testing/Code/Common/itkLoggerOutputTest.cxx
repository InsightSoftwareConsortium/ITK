/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLoggerOutputTest.cxx
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
#include "itkStdStreamLogOutput.h"
#include "itkLogger.h"
#include "itkLoggerOutput.h"


int itkLoggerOutputTest( int argc, char *argv [] )
{
  try
    {
    if (argc < 2)
      {
      std::cout << "Usage: " << argv[0] << " logFilename" << std::endl;
      return EXIT_FAILURE;
      }
    

    // Create an ITK StdStreamLogOutputs
    itk::StdStreamLogOutput::Pointer coutput = itk::StdStreamLogOutput::New();
    itk::StdStreamLogOutput::Pointer foutput = itk::StdStreamLogOutput::New();
    coutput->SetStream(std::cout);
    std::ofstream fout(argv[1]);
    foutput->SetStream(fout);

    // Create an ITK Logger
    itk::Logger::Pointer logger = itk::Logger::New();

    std::cout << "Testing itk::LoggerOutput" << std::endl;

    // Setting the logger
    logger->SetName("org.itk.rootLogger");
    logger->SetPriorityLevel(itk::LoggerBase::INFO);
    logger->SetLevelForFlushing(itk::LoggerBase::CRITICAL);

    std::cout << "  Adding console and file stream LogOutputs" << std::endl;
    logger->AddLogOutput(coutput);
    logger->AddLogOutput(foutput);

    // Printing the logger's member variables
    std::cout << "  Name: " << logger->GetName() << std::endl;
    std::cout << "  Priority Level: " << logger->GetPriorityLevel() << std::endl;
    std::cout << "  Level For Flushing: " << logger->GetLevelForFlushing() << std::endl;

    // Create an ITK LoggerOutput and then test it.
    itk::LoggerOutput::Pointer pOver = itk::LoggerOutput::New();
    pOver->OverrideITKWindow();
    pOver->SetLogger(logger);  // redirect messages from ITK OutputWindow -> logger2

    // test message for ITK OutputWindow
    itk::OutputWindow::GetInstance()->DisplayText("** This is from ITK OutputWindow **\n");
    itk::OutputWindow::GetInstance()->DisplayDebugText("** This is from ITK OutputWindow **\n");
    itk::OutputWindow::GetInstance()->DisplayWarningText("** This is from ITK OutputWindow **\n");
    itk::OutputWindow::GetInstance()->DisplayErrorText("** This is from ITK OutputWindow **\n");
    itk::OutputWindow::GetInstance()->DisplayGenericOutputText("** This is from ITK OutputWindow **\n");

    }
  catch(...)
    {
    std::cerr << "Exception catched !!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}


