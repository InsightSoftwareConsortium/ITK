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

#include <iostream>
#include <fstream>
#include "itkStdStreamLogOutput.h"
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
