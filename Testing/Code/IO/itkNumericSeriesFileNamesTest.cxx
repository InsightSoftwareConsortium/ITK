/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericSeriesFileNamesTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericSeriesFileNames.h"

int itkNumericSeriesFileNamesTest(int, char* [])
{

  itk::NumericSeriesFileNames::Pointer fit = itk::NumericSeriesFileNames::New();
  fit->SetStartIndex(10);
  fit->SetEndIndex(20);
  fit->SetIncrementIndex(2);
  fit->SetSeriesFormat ("foo.%03d.png");
  
  std::vector<std::string> names = fit->GetFileNames();
  std::vector<std::string>::iterator nit;

  for (nit = names.begin();
       nit != names.end();
       nit++)
    {
    std::cout << "File: " << (*nit).c_str() << std::endl;
    }

  std::cout << fit;

  return EXIT_SUCCESS;

}
