/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArchetypeSeriesFileNamesTest.cxx
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
#include "itkArchetypeSeriesFileNames.h"

int itkArchetypeSeriesFileNamesTest(int argc, char* argv[])
{

  if(argc < 2)
    {
    std::cerr << "Usage: " << argv[0]
              << "One or more filenames (with directory)";
    return EXIT_FAILURE;
    }

  
  std::cout << "Number of arguments: " << argc << std::endl;

  for (int i=1; i < argc; i++)
    {
    std::cout << "Testing argument " << i << std::endl;
    std::cout << "Archetype name: " << argv[i] << std::endl;
    
    itk::ArchetypeSeriesFileNames::Pointer fit = itk::ArchetypeSeriesFileNames::New();
    fit->SetArchetype ( argv[i] );

    std::vector<std::string> names = fit->GetFileNames();
    std::vector<std::string>::iterator nit;

    std::cout << "List of returned filenames: " << std::endl;
    for (nit = names.begin();
         nit != names.end();
         nit++)
      {
      std::cout << "File: " << (*nit).c_str() << std::endl;
      }

    std::cout << fit;

    }

  return EXIT_SUCCESS;

}
