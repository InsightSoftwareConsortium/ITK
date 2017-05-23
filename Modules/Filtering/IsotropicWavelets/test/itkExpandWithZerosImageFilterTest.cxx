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

#include "itkExpandWithZerosImageFilter.h"
#include "itkShrinkDecimateImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"
#include "itkTestingComparisonImageFilter.h"

#include <iostream>

#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

template <unsigned int VDimension>
int
runExpandWithZerosImageFilterTest()
{
  typedef float                             PixelType;
  typedef itk::Image<PixelType, VDimension> ImageType;
  bool                                      testPassed = true;

  // Create the input image
  typename ImageType::RegionType region;
  typename ImageType::SizeType   size;
  size.Fill(32);
  typename ImageType::IndexType inputIndex;
  inputIndex.Fill(9);
  region.SetSize(size);
  region.SetIndex(inputIndex);
  region.SetSize(size);

  typename ImageType::Pointer input = ImageType::New();
  input->SetLargestPossibleRegion(region);
  input->SetBufferedRegion(region);
  input->Allocate();
  input->FillBuffer(1);

  typedef itk::ExpandWithZerosImageFilter<ImageType, ImageType> ExpanderType;
  typename ExpanderType::Pointer                                expander = ExpanderType::New();

  unsigned int                             expandFactor = 3;
  typename ExpanderType::ExpandFactorsType expandFactors;
  expandFactors.Fill(expandFactor);

  expander->SetExpandFactors(expandFactors);
  TEST_SET_GET_VALUE(expandFactors, expander->GetExpandFactors());
  expander->SetInput(input);
  expander->Update();

  // Check the output against expected value
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator                      outIter(expander->GetOutput(), expander->GetOutput()->GetBufferedRegion());
  typename ImageType::IndexType outStartIndex = expander->GetOutput()->GetLargestPossibleRegion().GetIndex();

  while (!outIter.IsAtEnd())
  {
    typename ImageType::IndexType index = outIter.GetIndex();
    double                        value = outIter.Get();
    bool                          indexIsMultipleOfFactor = true;
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      if ((index[i] - outStartIndex[i]) % expander->GetExpandFactors()[i] != 0)
      {
        indexIsMultipleOfFactor = false;
        break;
      }
    }
    double trueValue = -1;
    if (indexIsMultipleOfFactor)
    {
      trueValue = 1;
    }
    else
    {
      trueValue = 0;
    }

    double tolerance = 1e-4;
    if (!itk::Math::FloatAlmostEqual(trueValue, value, 10, tolerance))
    {
      testPassed = false;
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error at Index: " << index << " ";
      std::cerr << "Expected value: " << trueValue << " ";
      std::cerr << ", but got: " << value << std::endl;
    }
    ++outIter;
  }

#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(expander->GetOutput(), "ExpandWithZeros Output");
#endif

  // Test than expand with zeros + shrinkage (decimate) results on input image.
  typedef itk::ShrinkDecimateImageFilter<ImageType, ImageType> DecimatorType;
  typename DecimatorType::Pointer                              decimator = DecimatorType::New();
  decimator->SetShrinkFactors(expandFactors);
  decimator->SetInput(expander->GetOutput());
  decimator->Update();

  typedef itk::Testing::ComparisonImageFilter<ImageType, ImageType> DifferenceFilterType;
  typename DifferenceFilterType::Pointer                            differenceFilter = DifferenceFilterType::New();
  differenceFilter->SetToleranceRadius(0);
  differenceFilter->SetDifferenceThreshold(0.000001);
  differenceFilter->SetValidInput(input);
  differenceFilter->SetTestInput(decimator->GetOutput());
  differenceFilter->Update();

  unsigned int numberOfDiffPixels = differenceFilter->GetNumberOfPixelsWithDifferences();
  if (numberOfDiffPixels > 0)
  {
    std::cerr << "Test failed! " << std::endl;
    std::cerr << "ExpandWithZeros + ShrinkDecimate should be equal to input image, but got " << numberOfDiffPixels
              << " unequal pixels" << std::endl;
    testPassed = false;
  }

  if (!testPassed)
  {
    std::cerr << "Test failed!" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "Test passed." << std::endl;
    return EXIT_SUCCESS;
  }
}

int
itkExpandWithZerosImageFilterTest(int argc, char * argv[])
{
  if (argc > 2)
  {
    std::cerr << "Usage: " << argv[0] << "[dimension]" << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                            ImageDimension = 2;
  typedef double                                PixelType;
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  typedef itk::ExpandWithZerosImageFilter<ImageType, ImageType> ExpanderType;
  ExpanderType::Pointer                                         expander = ExpanderType::New();
  EXERCISE_BASIC_OBJECT_METHODS(expander, ExpandWithZerosImageFilter, ImageToImageFilter);

  unsigned int dimension = 3;
  if (argc == 2)
  {
    dimension = atoi(argv[1]);
  }

  if (dimension == 2)
  {
    return runExpandWithZerosImageFilterTest<2>();
  }
  else if (dimension == 3)
  {
    return runExpandWithZerosImageFilterTest<3>();
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
