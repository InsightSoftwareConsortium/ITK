/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularExpressionSeriesFileNamesTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRegularExpressionSeriesFileNames.h"

int itkRegularExpressionSeriesFileNamesTest(int ac, char* av[])
{

  if(ac < 3)
  {
    std::cerr << "Usage: " << av[0] << " DicomDirectory\n";
    return EXIT_FAILURE;
  }


  itk::RegularExpressionSeriesFileNames::Pointer fit = itk::RegularExpressionSeriesFileNames::New();
  fit->SetDirectory(av[1]);
  fit->SetRegularExpression(av[2]);
  fit->SetSubMatch(atoi(av[3]));

  std::vector<std::string> names = fit->GetFileNames();
  std::vector<std::string>::iterator nit;

  for (nit = names.begin();
       nit != names.end();
       nit++)
    {
    std::cout << "File: " << (*nit).c_str() << std::endl;
    }

  return EXIT_SUCCESS;

}
