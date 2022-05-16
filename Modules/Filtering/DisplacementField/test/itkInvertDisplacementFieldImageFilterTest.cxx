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

#include "itkInvertDisplacementFieldImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"

int
itkInvertDisplacementFieldImageFilterTest(int argc, char * argv[])
{
  if (argc != 5)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " numberOfIterations meanTolerance maxTolerance enforceBoundaryCondition" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;

  using VectorType = itk::Vector<float, ImageDimension>;
  using DisplacementFieldType = itk::Image<VectorType, ImageDimension>;

  // Create a displacement field
  DisplacementFieldType::PointType     origin;
  DisplacementFieldType::SpacingType   spacing;
  DisplacementFieldType::SizeType      size;
  DisplacementFieldType::DirectionType direction;

  direction.SetIdentity();
  origin.Fill(0.0);
  spacing.Fill(0.5);
  size.Fill(100);

  auto ones = itk::MakeFilled<VectorType>(1);

  auto field = DisplacementFieldType::New();
  field->SetOrigin(origin);
  field->SetSpacing(spacing);
  field->SetRegions(size);
  field->SetDirection(direction);
  field->Allocate();
  field->FillBuffer(ones);

  constexpr VectorType zeroVector{};

  // make sure boundary does not move
  float weight1 = 1.0;

  const DisplacementFieldType::RegionType region = field->GetLargestPossibleRegion();
  const DisplacementFieldType::IndexType  startIndex = region.GetIndex();

  itk::ImageRegionIteratorWithIndex<DisplacementFieldType> ItF(field, field->GetLargestPossibleRegion());
  for (ItF.GoToBegin(); !ItF.IsAtEnd(); ++ItF)
  {
    DisplacementFieldType::IndexType index = ItF.GetIndex();
    bool                             isOnBoundary = false;
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      if (index[d] == startIndex[d] || index[d] == static_cast<int>(size[d]) - startIndex[d] - 1)
      {
        isOnBoundary = true;
        break;
      }
    }
    if (isOnBoundary)
    {
      ItF.Set(zeroVector);
    }
    else
    {
      ItF.Set(ItF.Get() * weight1);
    }
  }

  using InverterType = itk::InvertDisplacementFieldImageFilter<DisplacementFieldType>;
  auto inverter = InverterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(inverter, InvertDisplacementFieldImageFilter, ImageToImageFilter);


  auto numberOfIterations = static_cast<unsigned int>(std::stoi(argv[1]));
  inverter->SetMaximumNumberOfIterations(numberOfIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, inverter->GetMaximumNumberOfIterations());

  auto meanTolerance = static_cast<typename InverterType::RealType>(std::stod(argv[2]));
  inverter->SetMeanErrorToleranceThreshold(meanTolerance);
  ITK_TEST_SET_GET_VALUE(meanTolerance, inverter->GetMeanErrorToleranceThreshold());

  auto maxTolerance = static_cast<typename InverterType::RealType>(std::stod(argv[3]));
  inverter->SetMaxErrorToleranceThreshold(maxTolerance);
  ITK_TEST_SET_GET_VALUE(maxTolerance, inverter->GetMaxErrorToleranceThreshold());

  auto enforceBoundaryCondition = static_cast<bool>(std::stoi(argv[4]));
  ITK_TEST_SET_GET_BOOLEAN(inverter, EnforceBoundaryCondition, enforceBoundaryCondition);

  inverter->SetInput(field);
  ITK_TEST_SET_GET_VALUE(field, inverter->GetDisplacementField());

  inverter->SetDisplacementField(field);
  ITK_TEST_SET_GET_VALUE(field, inverter->GetDisplacementField());

  typename InverterType::RealType maxErrorNorm = 0.0;
  ITK_TEST_EXPECT_EQUAL(maxErrorNorm, inverter->GetMaxErrorNorm());

  try
  {
    inverter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
  }

  DisplacementFieldType::IndexType index;
  index[0] = 30;
  index[1] = 30;

  VectorType v = inverter->GetOutput()->GetPixel(index);
  VectorType delta = v + ones;
  if (delta.GetNorm() > 0.05)
  {
    std::cerr << "Failed to find proper inverse." << std::endl;
    return EXIT_FAILURE;
  }

  if (inverter->GetMeanErrorNorm() > inverter->GetMeanErrorToleranceThreshold() &&
      inverter->GetMaxErrorNorm() > inverter->GetMaxErrorToleranceThreshold())
  {
    std::cerr << "Failed to converge properly." << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
