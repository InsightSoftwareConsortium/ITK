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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkRegularExpressionSeriesFileNames.h"

int itkRegularExpressionSeriesFileNamesTest(int ac, char* av[])
{

  if(ac < 2)
  {
    std::cerr << "Usage: " << av[0] << " Directory\n";
    return EXIT_FAILURE;
  }


  itk::RegularExpressionSeriesFileNames::Pointer fit = itk::RegularExpressionSeriesFileNames::New();
  fit->SetDirectory(av[1]);
  fit->SetRegularExpression("[^.]*.(.*)");
  fit->SetSubMatch(1);

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
