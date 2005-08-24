/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStdStreamLogOutputTest.cxx
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

int itkStdStreamLogOutputTest( int argc, char *argv [] )
{
  try
    {

    if (argc < 2)
      {
      std::cout << "Usage: " << argv[0] << " logFilename" << std::endl;
      exit(EXIT_FAILURE);
      }
    
    // Create an ITK StdStreamLogOutput
    itk::StdStreamLogOutput::Pointer output = itk::StdStreamLogOutput::New();

    std::cout << "Testing itk::StdStreamLogOutput" << std::endl;
    std::cout.precision(15);

    std::cout << "  Testing with standard console stream" << std::endl;
    output->SetStream(std::cout);
    output->Write(1.2345);
    output->Write("This is the test message.\n");
    output->Write("This is the second test message.\n", 1.2345);
    output->Flush();

    std::cout << "  Testing with a file stream" << std::endl;
    std::ofstream fout(argv[1]);
    output->SetStream(fout);
    output->Write(1.2345);
    output->Write("This is the test message.\n");
    output->Write("This is the second test message.\n", 1.2345);
    output->Flush();
    output->Print(std::cout);
    }
  catch(...)
    {
    std::cerr << "Exception caught !!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}


