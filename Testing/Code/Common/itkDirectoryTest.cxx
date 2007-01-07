/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDirectoryTest.cxx
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
#include "itkDirectory.h"

int itkDirectoryTest(int argc, char *argv[])
{
  itk::Directory::Pointer directory = itk::Directory::New();
  
  if (argc < 2)
    {
    std::cerr << "Usage: " << argv[0] << " directory" << std::endl;
    return 1;
    }

  if (directory->Load("qwerty"))
    {
    std::cerr << "directory->Load(\"qwerty\")"
              << " should have failed." << std::endl;
    return EXIT_FAILURE;
    }
  directory->Load(argv[1]);
  directory->Print(std::cout);

  // Test GetFile with a success and failure
  if (directory->GetNumberOfFiles() > 0)
    {
    std::cout << "File 0 is " << directory->GetFile(0) << std::endl;
    }

  // This should fail
  unsigned int fileOutOfRange = static_cast<unsigned int>( directory->GetNumberOfFiles());
  if (directory->GetFile( fileOutOfRange) )
    {
    std::cerr << "directory->GetFile(directory->GetNumberOfFiles())"
              << " should have failed." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

