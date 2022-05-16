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

#include <iostream>
#include "itkTestingComparisonImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"
#include <cstdlib>

int
itkTestingComparisonImageFilterTest(int argc, char * argv[])
{
  if (argc < 12)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv);
    std::cerr << "  inputImageFile1 inputImageFile2 outputImage ignoreBoundaryPixels threshold radius "
                 "numberOfPixelsWithDifferences minimumDifference maximumDifference meanDifference totalDifference"
              << std::endl;
    return EXIT_FAILURE;
  }


  // Test using an unsigned integral pixel type and generate a signed
  // integral pixel type
  using InputPixelType = short;
  using OutputPixelType = unsigned short;

  constexpr unsigned int Dimension = 2;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;


  using ReaderType = itk::ImageFileReader<InputImageType>;

  auto reader1 = ReaderType::New();
  auto reader2 = ReaderType::New();

  reader1->SetFileName(argv[1]);
  reader2->SetFileName(argv[2]);

  // Define the filter
  using FilterType = itk::Testing::ComparisonImageFilter<InputImageType, OutputImageType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, ComparisonImageFilter, ImageToImageFilter);


  // setup the filter
  auto ignoreBoundaryPixels = static_cast<bool>(std::stoi(argv[4]));
  ITK_TEST_SET_GET_BOOLEAN(filter, IgnoreBoundaryPixels, ignoreBoundaryPixels);

  auto differenceThreshold = static_cast<typename FilterType::OutputPixelType>(std::stoi(argv[5]));
  filter->SetDifferenceThreshold(differenceThreshold);
  ITK_TEST_SET_GET_VALUE(differenceThreshold, filter->GetDifferenceThreshold());

  int toleranceRadius = std::stoi(argv[6]);
  filter->SetToleranceRadius(toleranceRadius);
  ITK_TEST_SET_GET_VALUE(toleranceRadius, filter->GetToleranceRadius());

  itk::SimpleFilterWatcher watcher(filter, "Difference");

  // wire the pipeline
  filter->SetValidInput(reader1->GetOutput());
  filter->SetTestInput(reader2->GetOutput());

  // Write the output
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto writer = WriterType::New();

  writer->SetInput(filter->GetOutput());

  writer->SetFileName(argv[3]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  unsigned long numberOfPixelsWithDifferences = filter->GetNumberOfPixelsWithDifferences();

  char * end;
  ITK_TEST_EXPECT_EQUAL(numberOfPixelsWithDifferences, std::strtoul(argv[7], &end, 10));

  auto minimumDifference = static_cast<typename FilterType::OutputPixelType>(std::stod(argv[8]));
  ITK_TEST_EXPECT_EQUAL(minimumDifference, filter->GetMinimumDifference());

  auto maximumDifference = static_cast<typename FilterType::OutputPixelType>(std::stod(argv[9]));
  ITK_TEST_EXPECT_EQUAL(maximumDifference, filter->GetMaximumDifference());

  auto meanDifference = static_cast<typename FilterType::RealType>(std::stod(argv[10]));

  const double epsilon = 1e-4;
  std::cout.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
  if (!itk::Math::FloatAlmostEqual(meanDifference, filter->GetMeanDifference(), 10, epsilon))
  {
    std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetMeanDifference" << std::endl;
    std::cerr << "Expected value " << meanDifference << std::endl;
    std::cerr << " differs from " << filter->GetMeanDifference();
    std::cerr << " by more than " << epsilon << std::endl;
    return EXIT_FAILURE;
  }

  auto totalDifference = static_cast<typename FilterType::AccumulateType>(std::stod(argv[11]));
  ITK_TEST_EXPECT_EQUAL(totalDifference, filter->GetTotalDifference());

  // Change test input spacing to test that comparison filter fails if spacings are different
  InputImageType::SpacingType spacing;
  spacing[0] = 5;
  spacing[1] = 1;

  // Expect failure
  reader2->GetOutput()->SetSpacing(spacing);
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());

  // Expect success
  auto coordinateTolerance = static_cast<double>(spacing[0]);
  filter->SetCoordinateTolerance(coordinateTolerance);
  ITK_TEST_SET_GET_VALUE(coordinateTolerance, filter->GetCoordinateTolerance());
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Reset
  filter->SetCoordinateTolerance(1.0e-6);
  reader2->GetOutput()->SetSpacing(reader1->GetOutput()->GetSpacing());
  // Change test input origin to test that comparison filter fails if origins are different
  InputImageType::PointType origin;
  origin[0] = 5;
  origin[1] = 1;

  // Expect failure
  reader2->GetOutput()->SetOrigin(origin);
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());

  filter->SetCoordinateTolerance(10);
  // Expect success
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Reset
  filter->SetCoordinateTolerance(1.0e-6);
  reader2->GetOutput()->SetOrigin(reader1->GetOutput()->GetOrigin());

  // Change test input direction to test that comparison filter fails if directions are different
  InputImageType::DirectionType direction;
  direction[0][0] = 2;
  direction[0][1] = 0;
  direction[1][0] = 0;
  direction[1][1] = 1;

  // Expect failure
  reader2->GetOutput()->SetDirection(direction);
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());

  // Expect success
  filter->SetDirectionTolerance(2);
  ITK_TEST_SET_GET_VALUE(2, filter->GetDirectionTolerance());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Reset
  filter->SetDirectionTolerance(1.0e-6);

  // Test disabling VerifyInputInformation()
  filter->SetVerifyInputInformation(false);
  ITK_TEST_SET_GET_VALUE(false, filter->GetVerifyInputInformation());
  filter->VerifyInputInformationOn();
  ITK_TEST_SET_GET_VALUE(true, filter->GetVerifyInputInformation());
  filter->VerifyInputInformationOff();
  ITK_TEST_SET_GET_VALUE(false, filter->GetVerifyInputInformation());
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  return EXIT_SUCCESS;
}
