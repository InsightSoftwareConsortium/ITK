/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkWarpHarmonicEnergyCalculator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


int
itkWarpHarmonicEnergyCalculatorTest(int argc, char * argv[])
{
  if (argc != 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0] << " useImageSpacing"
              << " derivativeWeights"
              << " expectedEnergy" << std::endl;
    return EXIT_FAILURE;
  }

  // Define the dimension of the images
  constexpr unsigned int ImageDimension = 3;

  using DeformationPixelType = itk::Vector<double, ImageDimension>;

  // Declare the types of the images
  using DisplacementFieldType = itk::Image<DeformationPixelType, ImageDimension>;

  // Declare the type of the index to access images
  using IndexType = itk::Index<ImageDimension>;

  // Declare the type of the size
  using SizeType = itk::Size<ImageDimension>;

  // Declare the type of the region
  using RegionType = itk::ImageRegion<ImageDimension>;

  // Create the input image
  DisplacementFieldType::Pointer inputDisplacementField = DisplacementFieldType::New();

  // Define its size, and start index
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  RegionType region;
  region.SetIndex(start);
  region.SetSize(size);

  // Initialize the input image
  inputDisplacementField->SetLargestPossibleRegion(region);
  inputDisplacementField->SetBufferedRegion(region);
  inputDisplacementField->SetRequestedRegion(region);
  inputDisplacementField->Allocate();

  // Initialize the content of the input image
  DeformationPixelType vectorValue;
  vectorValue.Fill(5.0); // FIXME: replace with something more interesting...
  inputDisplacementField->FillBuffer(vectorValue);

  // Declare the type for the itk::WarpHarmonicEnergyCalculator
  using CalculatorType = itk::WarpHarmonicEnergyCalculator<DisplacementFieldType>;


  // Create the calculator
  CalculatorType::Pointer calculator = CalculatorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(calculator, WarpHarmonicEnergyCalculator, Object);


  auto useImageSpacing = static_cast<bool>(std::stoi(argv[1]));
  ITK_TEST_SET_GET_BOOLEAN(calculator, UseImageSpacing, useImageSpacing);

  CalculatorType::WeightsType derivativeWeights;
  derivativeWeights.Fill(std::stod(argv[2]));
  calculator->SetDerivativeWeights(derivativeWeights);
  ITK_TEST_SET_GET_VALUE(derivativeWeights, calculator->GetDerivativeWeights());

  // Set the input image
  calculator->SetImage(inputDisplacementField);

  // Execute the calculator
  ITK_TRY_EXPECT_NO_EXCEPTION(calculator->Compute());

  // Regression test: check the computed harmonic energy
  double       expectedEnergy = std::stod(argv[3]);
  const double computedEnergy = calculator->GetHarmonicEnergy();
  if (itk::Math::NotAlmostEquals(expectedEnergy, computedEnergy))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetHarmonicEnergy()" << std::endl;
    std::cerr << "Expected: " << expectedEnergy << ", but got: " << computedEnergy << std::endl;
    return EXIT_FAILURE;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
