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
       ++nit)
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
       ++nit)
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
       ++nit)
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
       ++nit)
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
       ++nit)
    {
    std::cout << "File: " << (*nit).c_str() << std::endl;
    }


  std::cout << "Vector size: " << names.size() << std::endl;

  std::cout << "Directory: " << fit->GetDirectory() << std::endl;
  std::cout << "RegularExpression: " << fit->GetRegularExpression() << std::endl;
  std::cout << "SubMatch: " << fit->GetSubMatch() << std::endl;


  return EXIT_SUCCESS;

}
