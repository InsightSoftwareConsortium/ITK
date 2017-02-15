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
#include "itkExpandWithZerosImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkMath.h"
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

template <unsigned int N>
int
runExpandWithZerosImageFilterTest()
{
  typedef float                    PixelType;
  typedef itk::Image<PixelType, N> ImageType;
  bool                             testPassed = true;

  // =============================================================

  std::cout << "Create the input image fill with ones." << std::endl;
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

  // =============================================================

  typedef itk::ExpandWithZerosImageFilter<ImageType, ImageType> ExpanderType;
  typename ExpanderType::Pointer                                expander = ExpanderType::New();

  unsigned int factors[N];
  for (unsigned int i = 0; i < N; i++)
  {
    factors[i] = 3;
  }

  expander->SetInput(input);
  expander->SetExpandFactors(factors);
  expander->Print(std::cout);
  expander->Update();

  // =============================================================

  std::cout << "Checking the output against expected." << std::endl;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator                      outIter(expander->GetOutput(), expander->GetOutput()->GetBufferedRegion());
  typename ImageType::IndexType outStartIndex = expander->GetOutput()->GetLargestPossibleRegion().GetIndex();

  while (!outIter.IsAtEnd())
  {
    typename ImageType::IndexType index = outIter.GetIndex();
    double                        value = outIter.Get();

    bool indexIsMultipleOfFactor(true);
    for (unsigned int i = 0; i < N; ++i)
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

    if (itk::Math::abs(trueValue - value) > 1e-4)
    {
      testPassed = false;
      std::cout << "Error at Index: " << index << " ";
      std::cout << "Expected: " << trueValue << " ";
      std::cout << "Actual: " << value << std::endl;
    }
    ++outIter;
  }

  if (!testPassed)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(expander->GetOutput(), "ExpandWithZeros Output");
#endif
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}

int
itkExpandWithZerosImageFilterTest(int argc, char * argv[])
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
    return runExpandWithZerosImageFilterTest<2>();
  }
  else if (dimension == 3)
  {
    return runExpandWithZerosImageFilterTest<3>();
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
