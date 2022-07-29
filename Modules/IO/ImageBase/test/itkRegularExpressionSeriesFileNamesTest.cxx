/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkRegularExpressionSeriesFileNames.h"
#include "itkTestingMacros.h"

int
itkRegularExpressionSeriesFileNamesTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " Directory\n";
    return EXIT_FAILURE;
  }


  itk::RegularExpressionSeriesFileNames::Pointer fit = itk::RegularExpressionSeriesFileNames::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(fit, RegularExpressionSeriesFileNames, Object);


  const char * directory = argv[1];
  fit->SetDirectory(directory);
  ITK_TEST_SET_GET_VALUE(*directory, *(fit->GetDirectory()));

  std::string regularExpression = "[^.]*.(.*)";
  fit->SetRegularExpression(regularExpression);
  ITK_TEST_SET_GET_VALUE(regularExpression, fit->GetRegularExpression());

  unsigned int subMatch = 1;
  fit->SetSubMatch(subMatch);
  ITK_TEST_SET_GET_VALUE(subMatch, fit->GetSubMatch());

  bool numericSort = false;
  ITK_TEST_SET_GET_BOOLEAN(fit, NumericSort, numericSort);

  std::vector<std::string>           names = fit->GetFileNames();
  std::vector<std::string>::iterator nit;

  // normal sort
  std::cout << "Normal Sort--------" << std::endl;
  for (nit = names.begin(); nit != names.end(); ++nit)
  {
    std::cout << "File: " << nit->c_str() << std::endl;
  }

  // Show only those files with numbers in the names
  regularExpression = "([0-9]+)";
  fit->SetRegularExpression(regularExpression);
  ITK_TEST_SET_GET_VALUE(regularExpression, fit->GetRegularExpression());

  numericSort = true;
  ITK_TEST_SET_GET_BOOLEAN(fit, NumericSort, numericSort);

  fit->SetSubMatch(subMatch);
  names = fit->GetFileNames();
  std::cout << "Numeric sort on only files with numbers in the names--------" << std::endl;
  for (nit = names.begin(); nit != names.end(); ++nit)
  {
    std::cout << "File: " << nit->c_str() << std::endl;
  }


  std::cout << "Vector size: " << names.size() << std::endl;

  // Show only those files with numbers in the names followed by other
  // numbers.  Sort them by the first set of numbers.
  regularExpression = "([0-9]+)[^0-9]+([0-9]+)";
  fit->SetRegularExpression(regularExpression);
  ITK_TEST_SET_GET_VALUE(regularExpression, fit->GetRegularExpression());

  fit->NumericSortOn();
  fit->SetSubMatch(subMatch);
  names = fit->GetFileNames();
  std::cout << "Numeric sort on only files with numbers in the names.  Sort on the first set of numbers.--------"
            << std::endl;
  for (nit = names.begin(); nit != names.end(); ++nit)
  {
    std::cout << "File: " << nit->c_str() << std::endl;
  }

  // Show only those files with numbers in the names followed by other
  // numbers.  Sort them by the second set of numbers.
  fit->SetRegularExpression("([0-9]+)[^0-9]+([0-9]+)");
  fit->NumericSortOn();

  subMatch = 2;
  fit->SetSubMatch(subMatch);
  ITK_TEST_SET_GET_VALUE(subMatch, fit->GetSubMatch());

  names = fit->GetFileNames();
  std::cout << "Numeric sort on only files with numbers in the names.  Sort on the second set of numbers.--------"
            << std::endl;
  for (nit = names.begin(); nit != names.end(); ++nit)
  {
    std::cout << "File: " << nit->c_str() << std::endl;
  }


  std::cout << "Vector size: " << names.size() << std::endl;

  std::cout << "Directory: " << fit->GetDirectory() << std::endl;
  std::cout << "RegularExpression: " << fit->GetRegularExpression() << std::endl;
  std::cout << "SubMatch: " << fit->GetSubMatch() << std::endl;


  return EXIT_SUCCESS;
}
