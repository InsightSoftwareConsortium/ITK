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

#include <iostream>
#include "itkShrinkDecimateImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkMath.h"
#include "itkImageRegionIteratorWithIndex.h"
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

template <unsigned int N>
int
runShrinkDecimateImageFilterTest()
{
  typedef float                    PixelType;
  typedef itk::Image<PixelType, N> ImageType;
  bool                             testPassed = true;

  //=============================================================

  std::cout << "Create the input image." << std::endl;
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
      // we multiply by ten so for more precision
      inIt.Set(inIt.GetIndex()[0] * 10);
    }
  }
  //=============================================================

  std::cout << std::endl;
  typedef itk::ShrinkDecimateImageFilter<ImageType, ImageType> DecimatorType;
  typename DecimatorType::Pointer                              decimator = DecimatorType::New();

  try
  {
    // update with 2,2 shrink factor
    unsigned int factors[N];
    for (unsigned int i = 0; i < N; i++)
      factors[i] = 2;
    std::cout << "== Testing with shrink factors " << factors[0] << " " << factors[1] << " == " << std::endl;
    decimator->SetShrinkFactors(factors);
    decimator->SetInput(input);
    decimator->Print(std::cout);
    decimator->Update();

    // check values
    itk::ImageRegionConstIteratorWithIndex<ImageType> outIt(decimator->GetOutput(),
                                                            decimator->GetOutput()->GetLargestPossibleRegion());
    for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
    {
      int trueValue = outIt.GetIndex()[0] * factors[0] * 10;
      if (outIt.Get() != static_cast<int>(trueValue))
      {
        std::cout << "Wrong pixel value at " << outIt.GetIndex() << " of " << outIt.Get() << " . Should be "
                  << trueValue << std::endl;
        testPassed = false;
      }
    }
  }
  catch (itk::ExceptionObject & e)
  {
    std::cout << "Excpetion: " << e << std::endl;
    testPassed = false;
  }

  if (!testPassed)
  {
    std::cout << "Test failed." << std::endl;
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
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
