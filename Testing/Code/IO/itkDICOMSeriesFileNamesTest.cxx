/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDICOMSeriesFileNamesTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkDICOMImageIO2Factory.h"
#include "itkDICOMSeriesFileNames.h"

int itkDICOMSeriesFileNamesTest(int ac, char* av[])
{

  if(ac < 1)
  {
    std::cerr << "Usage: " << av[0] << " DicomDirectory\n";
    return EXIT_FAILURE;
  }

  std::vector<std::string> names;
  std::vector<std::string>::iterator nit;

  itk::DICOMSeriesFileNames::Pointer fit = itk::DICOMSeriesFileNames::New();
  fit->SetDirectory(av[1]);
  fit->SetFileNameSortingOrderToSortByImageNumber();

  std::cout << "Files sorted by image number" << std::endl;
  names = fit->GetFileNames();
  for (nit = names.begin();
       nit != names.end();
       nit++)
    {
    std::cout << "File: " << (*nit).c_str() << std::endl;
    }


  fit->SetFileNameSortingOrderToSortBySliceLocation();

  std::cout << "Files sorted by slice location" << std::endl;
  names = fit->GetFileNames();
  for (nit = names.begin();
       nit != names.end();
       nit++)
    {
    std::cout << "File: " << (*nit).c_str() << std::endl;
    }

  
  fit->SetFileNameSortingOrderToSortByImagePositionPatient();

  std::cout << "Files sorted by ImagePositionPatient" << std::endl;
  names = fit->GetFileNames();
  for (nit = names.begin();
       nit != names.end();
       nit++)
    {
    std::cout << "File: " << (*nit).c_str() << std::endl;
    }
  
  return EXIT_SUCCESS;

}
