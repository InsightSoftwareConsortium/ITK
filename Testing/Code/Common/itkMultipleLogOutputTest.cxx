/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleLogOutputTest.cxx
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
#include "itkMultipleLogOutput.h"


int itkMultipleLogOutputTest( int argc, char *argv [] )
{
  try
    {
    if (argc < 2)
      {
      std::cout << "Usage: " << argv[0] << " logFilename" << std::endl;
      return EXIT_FAILURE;
      }
    

    // Create an ITK StdStreamLogOutput
    itk::StdStreamLogOutput::Pointer coutput = itk::StdStreamLogOutput::New();
    itk::StdStreamLogOutput::Pointer foutput = itk::StdStreamLogOutput::New();
    itk::MultipleLogOutput::Pointer m_output = itk::MultipleLogOutput::New();

    std::cout << "Testing itk::MultipleLogOutput" << std::endl;
    coutput->SetStream(std::cout);
    std::ofstream fout(argv[1]);
    foutput->SetStream(fout);

    std::cout << "  Adding console and file stream LogOutputs" << std::endl;
    m_output->AddLogOutput(coutput);
    m_output->AddLogOutput(foutput);

    std::cout << "  Writing by itk::MultipleLogOutput" << std::endl;
    m_output->Write(1.2345);
    m_output->Write("This is the test message.\n");
    m_output->Write("This is the second test message.\n", 1.2345);
    m_output->Flush();
    }
  catch(...)
    {
    std::cerr << "Exception catched !!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}


