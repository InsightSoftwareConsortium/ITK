/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDICOMSeriesFileNamesTest.cxx
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
#include "itkDICOMImageIO2Factory.h"
#include "itkDICOMSeriesFileNames.h"

int itkDICOMSeriesFileNamesTest(int ac, char* av[])
{

  if(ac < 2)
  {
    std::cerr << "Usage: " << av[0] << " DicomDirectory\n";
    return EXIT_FAILURE;
  }

  std::vector<std::string> names;
  std::vector<std::string>::iterator nit;

  itk::DICOMSeriesFileNames::Pointer fit = itk::DICOMSeriesFileNames::New();
  fit->SetDirectory(av[1]);
  fit->SetFileNameSortingOrderToSortByImageNumber();

  // Now look at each series in turn
  std::vector<std::string> seriesUIDs;
  seriesUIDs = fit->GetSeriesUIDs();
  
  std::vector<std::string>::iterator sit;
  for (sit = seriesUIDs.begin(); sit != seriesUIDs.end(); ++sit)
    {
    std::cout << "Series: " << *sit << std::endl;
    std::cout << "\tFiles sorted by image number" << std::endl;
    fit->SetFileNameSortingOrderToSortByImageNumber();
    names = fit->GetFileNames( *sit );
    for (nit = names.begin();
         nit != names.end();
         nit++)
      {
      std::cout << "\t\tFile: " << (*nit).c_str() << std::endl;
      }
    
    
    std::cout << "\tFiles sorted by slice location(Ascending)" << std::endl;
    fit->SetFileNameSortingOrderToSortBySliceLocation();
    fit->AscendingOn();
    names = fit->GetFileNames( *sit );
    for (nit = names.begin();
         nit != names.end();
         nit++)
      {
      std::cout << "\t\tFile: " << (*nit).c_str() << std::endl;
      }
    
    std::cout << "\tFiles sorted by slice location(Descending)" << std::endl;
    fit->SetFileNameSortingOrderToSortBySliceLocation();
    fit->AscendingOff();
    names = fit->GetFileNames( *sit );
    for (nit = names.begin();
         nit != names.end();
         nit++)
      {
      std::cout << "\t\tFile: " << (*nit).c_str() << std::endl;
      }
    
    
    std::cout << "\tFiles sorted by ImagePositionPatient(Ascending)" << std::endl;
    fit->SetFileNameSortingOrderToSortByImagePositionPatient();
    fit->AscendingOn();
    names = fit->GetFileNames( *sit );
    for (nit = names.begin();
         nit != names.end();
         nit++)
      {
      std::cout << "\t\tFile: " << (*nit).c_str() << std::endl;
      }
    std::cout << "\tFiles sorted by ImagePositionPatient(Descending)" << std::endl;
    fit->SetFileNameSortingOrderToSortByImagePositionPatient();
    fit->AscendingOff();
    names = fit->GetFileNames( *sit );
    for (nit = names.begin();
         nit != names.end();
         nit++)
      {
      std::cout << "\t\tFile: " << (*nit).c_str() << std::endl;
      }

    }

  return EXIT_SUCCESS;

}
