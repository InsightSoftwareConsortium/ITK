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

#include "itkShrinkDecimateImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkMath.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"

#include <iostream>

#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

template <unsigned int VDimension>
int
runShrinkDecimateImageFilterTest()
{
  typedef float                             PixelType;
  typedef itk::Image<PixelType, VDimension> ImageType;
  bool                                      testPassed = true;

  // Create the input image
  typename ImageType::RegionType region;
  typename ImageType::SizeType   size;
  size.Fill(32);
  typename ImageType::IndexType index;
  index.Fill(9);
  region.SetSize(size);
  region.SetIndex(index);

  typename ImageType::Pointer input = ImageType::New();
  input->SetLargestPossibleRegion(region);
  input->SetBufferedRegion(region);
  input->Allocate();

  {
    itk::ImageRegionIteratorWithIndex<ImageType> inIt(input, region);
    for (inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt)
    {
      // Multiply by ten for more precision
      inIt.Set(inIt.GetIndex()[0] * 10);
    }
  }

  typedef itk::ShrinkDecimateImageFilter<ImageType, ImageType> DecimatorType;
  typename DecimatorType::Pointer                              decimator = DecimatorType::New();

  try
  {
    unsigned int                              shrinkFactor = 1;
    typename DecimatorType::ShrinkFactorsType shrinkFactors;
    shrinkFactors.Fill(shrinkFactor);
    for (unsigned int i = 0; i < shrinkFactors.Size(); ++i)
    {
      decimator->SetShrinkFactor(i, shrinkFactors[i]);
    }
    TEST_SET_GET_VALUE(shrinkFactors, decimator->GetShrinkFactors());

    // Update with 2,2 shrink factor
    shrinkFactor = 2;
    shrinkFactors.Fill(shrinkFactor);
    decimator->SetShrinkFactors(shrinkFactors);
    TEST_SET_GET_VALUE(shrinkFactors, decimator->GetShrinkFactors());

    decimator->SetInput(input);

    TRY_EXPECT_NO_EXCEPTION(decimator->Update());

    // Check values
    itk::ImageRegionConstIteratorWithIndex<ImageType> outIt(decimator->GetOutput(),
                                                            decimator->GetOutput()->GetLargestPossibleRegion());
    for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
    {
      int trueValue = outIt.GetIndex()[0] * shrinkFactors[0] * 10;
      if (outIt.Get() != static_cast<int>(trueValue))
      {
        std::cerr << "Wrong pixel value at " << outIt.GetIndex() << " of " << outIt.Get() << " . Should be "
                  << trueValue << std::endl;
        testPassed = false;
      }
    }
  }
  catch (itk::ExceptionObject & e)
  {
    std::cerr << "Exception: " << e << std::endl;
    testPassed = false;
  }

  if (!testPassed)
  {
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
  }

#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(decimator->GetOutput(), "ShrinkDecimate Output");
#endif

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}

int
itkShrinkDecimateImageFilterTest(int argc, char * argv[])
{
  if (argc > 2)
  {
    std::cerr << "Usage: " << argv[0] << "[dimension]" << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                            ImageDimension = 3;
  typedef double                                PixelType;
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  typedef itk::ShrinkDecimateImageFilter<ImageType, ImageType> ShrinkDecimateImageFilterType;
  ShrinkDecimateImageFilterType::Pointer                       decimator = ShrinkDecimateImageFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS(decimator, ShrinkDecimateImageFilter, ImageToImageFilter);

  unsigned int dimension = 3;
  if (argc == 2)
  {
    dimension = atoi(argv[1]);
  }

  if (dimension == 2)
  {
    return runShrinkDecimateImageFilterTest<2>();
  }
  else if (dimension == 3)
  {
    return runShrinkDecimateImageFilterTest<3>();
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
