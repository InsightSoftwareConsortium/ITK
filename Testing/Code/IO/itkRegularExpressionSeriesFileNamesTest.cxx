/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularExpressionSeriesFileNamesTest.cxx
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

// normal sort
  std::cout << "Normal Sort--------" << std::endl;
  for (nit = names.begin();
       nit != names.end();
       nit++)
    {
    std::cout << "File: " << (*nit).c_str() << std::endl;
    }

// numeric sort
  fit->SetRegularExpression("[^0-9]*([0-9]*)");
  fit->NumericSortOn();
  fit->SetSubMatch(1);
  names = fit->GetFileNames();
  std::cout << "Numeric Sort--------" << std::endl;
  for (nit = names.begin();
       nit != names.end();
       nit++)
    {
    std::cout << "File: " << (*nit).c_str() << std::endl;
    }

  std::cout << fit;

  // Show only those files with numbers in the names
  fit->SetRegularExpression("([0-9]+)");
  fit->NumericSortOn();
  fit->SetSubMatch(1);
  names = fit->GetFileNames();
  std::cout << "Numeric sort on only files with numbers in the names--------" << std::endl;
  for (nit = names.begin();
       nit != names.end();
       nit++)
    {
    std::cout << "File: " << (*nit).c_str() << std::endl;
    }


  std::cout << "Vector size: " << names.size() << std::endl;

  // Show only those files with numbers in the names followed by other
  // numbers.  Sort them by the first set of numbers.
  fit->SetRegularExpression("([0-9]+)[^0-9]+([0-9]+)");
  fit->NumericSortOn();
  fit->SetSubMatch(1);
  names = fit->GetFileNames();
  std::cout << "Numeric sort on only files with numbers in the names.  Sort on the first set of numbers.--------" << std::endl;
  for (nit = names.begin();
       nit != names.end();
       nit++)
    {
    std::cout << "File: " << (*nit).c_str() << std::endl;
    }

  // Show only those files with numbers in the names followed by other
  // numbers.  Sort them by the second set of numbers.
  fit->SetRegularExpression("([0-9]+)[^0-9]+([0-9]+)");
  fit->NumericSortOn();
  fit->SetSubMatch(2);
  names = fit->GetFileNames();
  std::cout << "Numeric sort on only files with numbers in the names.  Sort on the second set of numbers.--------" << std::endl;
  for (nit = names.begin();
       nit != names.end();
       nit++)
    {
    std::cout << "File: " << (*nit).c_str() << std::endl;
    }


  std::cout << "Vector size: " << names.size() << std::endl;

  std::cout << "Directory: " << fit->GetDirectory() << std::endl;
  std::cout << "RegularExpression: " << fit->GetRegularExpression() << std::endl;
  std::cout << "SubMatch: " << fit->GetSubMatch() << std::endl;
  

  return EXIT_SUCCESS;

}
