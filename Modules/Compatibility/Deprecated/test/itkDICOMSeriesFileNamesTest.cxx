/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

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
         ++nit)
      {
      std::cout << "\t\tFile: " << (*nit).c_str() << std::endl;
      }


    std::cout << "\tFiles sorted by slice location(Ascending)" << std::endl;
    fit->SetFileNameSortingOrderToSortBySliceLocation();
    fit->AscendingOn();
    names = fit->GetFileNames( *sit );
    for (nit = names.begin();
         nit != names.end();
         ++nit)
      {
      std::cout << "\t\tFile: " << (*nit).c_str() << std::endl;
      }

    std::cout << "\tFiles sorted by slice location(Descending)" << std::endl;
    fit->SetFileNameSortingOrderToSortBySliceLocation();
    fit->AscendingOff();
    names = fit->GetFileNames( *sit );
    for (nit = names.begin();
         nit != names.end();
         ++nit)
      {
      std::cout << "\t\tFile: " << (*nit).c_str() << std::endl;
      }


    std::cout << "\tFiles sorted by ImagePositionPatient(Ascending)" << std::endl;
    fit->SetFileNameSortingOrderToSortByImagePositionPatient();
    fit->AscendingOn();
    names = fit->GetFileNames( *sit );
    for (nit = names.begin();
         nit != names.end();
         ++nit)
      {
      std::cout << "\t\tFile: " << (*nit).c_str() << std::endl;
      }
    std::cout << "\tFiles sorted by ImagePositionPatient(Descending)" << std::endl;
    fit->SetFileNameSortingOrderToSortByImagePositionPatient();
    fit->AscendingOff();
    names = fit->GetFileNames( *sit );
    for (nit = names.begin();
         nit != names.end();
         ++nit)
      {
      std::cout << "\t\tFile: " << (*nit).c_str() << std::endl;
      }

    }

  return EXIT_SUCCESS;

}
