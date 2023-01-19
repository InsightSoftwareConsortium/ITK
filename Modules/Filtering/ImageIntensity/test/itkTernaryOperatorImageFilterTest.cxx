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
#include "itkImage.h"
#include "itkTernaryOperatorImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"

int
itkTernaryOperatorImageFilterTest(int, char *[])
{

  //
  // Test the functor
  //

  itk::Functor::TernaryOperator<bool, short, short, short> func;

  if ((true ? 12 : 37) != func(true, 12, 37))
  {
    std::cerr << "Incorrect value returned: true ? 12 : 37." << std::endl;
    return EXIT_FAILURE;
  }

  if ((false ? 12 : 37) != func(false, 12, 37))
  {
    std::cerr << "Incorrect value returned: false ? 12 : 37." << std::endl;
    return EXIT_FAILURE;
  }

  //
  // Check the image filter
  //

  //
  // Overview of the test:
  // Mask is a checkerboard, with "even" indices set to true and
  // "odd" indices set to false. Input2 and Input3 are set to constant
  // values that differ from one another. After the ternary operator
  // is applied, the output image should have the value of Input2 at
  // even indices and the value of Input3 at odd indices.
  //

  // Define the dimension of the images
  constexpr unsigned int ImageDimension = 2;

  // Declare the pixel types of the images
  using MaskPixelType = bool;
  using GrayPixelType = short;

  using MaskImageType = itk::Image<MaskPixelType, ImageDimension>;
  using GrayImageType = itk::Image<GrayPixelType, ImageDimension>;

  MaskImageType::IndexType origin;
  origin.Fill(0);
  MaskImageType::SizeType size;
  size.Fill(20);
  MaskImageType::RegionType region(origin, size);

  auto mask = MaskImageType::New();
  mask->SetRegions(region);
  mask->Allocate();
  mask->FillBuffer(false);

  // Checkerboard the mask
  using MaskItType = itk::ImageRegionIteratorWithIndex<MaskImageType>;
  MaskItType maskIt(mask, mask->GetLargestPossibleRegion());
  for (maskIt.GoToBegin(); !maskIt.IsAtEnd(); ++maskIt)
  {
    if (maskIt.GetIndex()[0] + maskIt.GetIndex()[1] % 2 == 0)
    {
      maskIt.Set(true);
    }
  }

  constexpr short val1 = 25;
  auto            image1 = GrayImageType::New();
  image1->SetRegions(region);
  image1->Allocate();
  image1->FillBuffer(val1);

  constexpr short val2 = 123;
  auto            image2 = GrayImageType::New();
  image2->SetRegions(region);
  image2->Allocate();
  image2->FillBuffer(val2);

  using TernaryType = itk::TernaryOperatorImageFilter<MaskImageType, GrayImageType>;
  auto tern = TernaryType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(tern, TernaryOperatorImageFilter, TernaryGeneratorImageFilter);

  tern->SetInput1(mask);
  tern->SetInput2(image1);
  tern->SetInput3(image2);
  tern->Update();

  auto output = GrayImageType::New();
  output->Graft(tern->GetOutput());

  // Even indices should be equal to val1 (the value of the second input)
  // Odd indices should be equal to val2 (the value of the third input)
  using GrayItType = itk::ImageRegionIteratorWithIndex<GrayImageType>;
  GrayItType outIt(output, output->GetLargestPossibleRegion());
  for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
  {
    if (outIt.GetIndex()[0] + outIt.GetIndex()[1] % 2 == 0 && outIt.Get() != val1)
    {
      std::cerr << "Error: Value should be " << val1 << " but was " << outIt.Get() << std::endl;
      return EXIT_FAILURE;
    }
    if (outIt.GetIndex()[0] + outIt.GetIndex()[1] % 2 == 1 && outIt.Get() != val2)
    {
      std::cerr << "Error: Value should be " << val2 << " but was " << outIt.Get() << std::endl;
      return EXIT_FAILURE;
    }
  }

  tern->SetInput3(GrayImageType::Pointer(nullptr));
  tern->SetConstant3(99);
  tern->Update();

  output = tern->GetOutput();

  // Even indices should be equal to val1 (the value of the second input)
  // Odd indices should be equal to 99
  outIt = GrayItType(output, output->GetLargestPossibleRegion());
  for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
  {
    if (outIt.GetIndex()[0] + outIt.GetIndex()[1] % 2 == 0 && outIt.Get() != val1)
    {
      std::cerr << "Error: Value should be " << val1 << " but was " << outIt.Get() << std::endl;
      return EXIT_FAILURE;
    }
    if (outIt.GetIndex()[0] + outIt.GetIndex()[1] % 2 == 1 && outIt.Get() != 99)
    {
      std::cerr << "Error: Value should be " << 99 << " but was " << outIt.Get() << std::endl;
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
