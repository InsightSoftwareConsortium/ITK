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

#include "itkNumericSeriesFileNames.h"
#include "itksys/SystemTools.hxx"
#include "itkTestingMacros.h"

int
itkNumericSeriesFileNamesTest(int, char *[])
{

  itk::NumericSeriesFileNames::Pointer fit = itk::NumericSeriesFileNames::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(fit, NumericSeriesFileNames, Object);


  // Test exceptions
  itk::SizeValueType startIndex = 6;
  itk::SizeValueType endIndex = 5;

  fit->SetStartIndex(startIndex);
  fit->SetEndIndex(endIndex);

  ITK_TRY_EXPECT_EXCEPTION(fit->GetFileNames());

  endIndex = 7;
  fit->SetEndIndex(endIndex);

  itk::SizeValueType incrementIndex = 0;
  fit->SetIncrementIndex(incrementIndex);

  ITK_TRY_EXPECT_EXCEPTION(fit->GetFileNames());


  startIndex = 10;
  fit->SetStartIndex(startIndex);
  ITK_TEST_SET_GET_VALUE(startIndex, fit->GetStartIndex());

  endIndex = 20;
  fit->SetEndIndex(endIndex);
  ITK_TEST_SET_GET_VALUE(endIndex, fit->GetEndIndex());

  incrementIndex = 2;
  fit->SetIncrementIndex(incrementIndex);
  ITK_TEST_SET_GET_VALUE(incrementIndex, fit->GetIncrementIndex());

  std::string format = "foo.%0200d.png";
  fit->SetSeriesFormat(format);
  ITK_TEST_SET_GET_VALUE(format, fit->GetSeriesFormat());

  std::vector<std::string>           names = fit->GetFileNames();
  std::vector<std::string>::iterator nit;

  for (nit = names.begin(); nit != names.end(); ++nit)
  {
    // Check for filename truncation
    if (itksys::SystemTools::GetFilenameLastExtension(*nit) != ".png")
    {
      std::cerr << "Generated file name: " << *nit << " does not have the proper extension"
                << " .png"
                << " and may have been truncated." << std::endl;
      return EXIT_FAILURE;
    }
    std::cout << "File: " << nit->c_str() << std::endl;
  }

  // Exercise the PrintSelf method to print the filenames for coverage purposes
  std::cout << fit;

  return EXIT_SUCCESS;
}
