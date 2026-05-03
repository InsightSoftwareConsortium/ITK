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
#include "itkGTest.h"

TEST(NumericSeriesFileNames, ConvertedLegacyTest)
{

  const itk::NumericSeriesFileNames::Pointer fit = itk::NumericSeriesFileNames::New();

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(fit, NumericSeriesFileNames, Object);


  // Test exceptions
  itk::SizeValueType startIndex = 6;
  itk::SizeValueType endIndex = 5;

  fit->SetStartIndex(startIndex);
  fit->SetEndIndex(endIndex);

  EXPECT_THROW(fit->GetFileNames(), itk::ExceptionObject);

  endIndex = 7;
  fit->SetEndIndex(endIndex);

  itk::SizeValueType incrementIndex = 0;
  fit->SetIncrementIndex(incrementIndex);

  EXPECT_THROW(fit->GetFileNames(), itk::ExceptionObject);


  startIndex = 10;
  fit->SetStartIndex(startIndex);
  EXPECT_EQ(startIndex, fit->GetStartIndex());

  endIndex = 20;
  fit->SetEndIndex(endIndex);
  EXPECT_EQ(endIndex, fit->GetEndIndex());

  incrementIndex = 2;
  fit->SetIncrementIndex(incrementIndex);
  EXPECT_EQ(incrementIndex, fit->GetIncrementIndex());

  const std::string format = "foo.%0200d.png";
  fit->SetSeriesFormat(format);
  EXPECT_EQ(format, fit->GetSeriesFormat());

  const std::vector<std::string> names = fit->GetFileNames();

  for (auto & name : names)
  {
    // Check for filename truncation
    EXPECT_EQ(itksys::SystemTools::GetFilenameLastExtension(name), ".png");
  }

  // Exercise the PrintSelf method to print the filenames for coverage purposes
  std::cout << fit;
}
