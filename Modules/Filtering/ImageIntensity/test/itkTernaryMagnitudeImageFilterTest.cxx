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

#include "itkImageFileWriter.h"
#include "itkTernaryMagnitudeImageFilter.h"
#include "itkTestingMacros.h"

int
itkTernaryMagnitudeImageFilterTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << "outputImage " << std::endl;
    return EXIT_FAILURE;
  }

  // Define the dimension of the images
  constexpr unsigned int Dimension = 3;

  // Define the pixel types
  using InputPixelType = float;
  using OutputPixelType = unsigned short;

  // Declare the types of the images
  using InputImage1Type = itk::Image<InputPixelType, Dimension>;
  using InputImage2Type = itk::Image<InputPixelType, Dimension>;
  using InputImage3Type = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  // Declare the type of the index to access images
  using IndexType = itk::Index<Dimension>;

  // Declare the type of the size
  using SizeType = itk::Size<Dimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<Dimension>;

  // Create the input images
  InputImage1Type::Pointer inputImageA = InputImage1Type::New();
  InputImage2Type::Pointer inputImageB = InputImage2Type::New();
  InputImage3Type::Pointer inputImageC = InputImage3Type::New();

  // Define their size and start index
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

  // Initialize Image A
  inputImageA->SetLargestPossibleRegion(region);
  inputImageA->SetBufferedRegion(region);
  inputImageA->SetRequestedRegion(region);
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetLargestPossibleRegion(region);
  inputImageB->SetBufferedRegion(region);
  inputImageB->SetRequestedRegion(region);
  inputImageB->Allocate();

  // Initialize Image C
  inputImageC->SetLargestPossibleRegion(region);
  inputImageC->SetBufferedRegion(region);
  inputImageC->SetRequestedRegion(region);
  inputImageC->Allocate();

  // Declare Iterator types for each image
  using InputImage1IteratorType = itk::ImageRegionIteratorWithIndex<InputImage1Type>;
  using InputImage2IteratorType = itk::ImageRegionIteratorWithIndex<InputImage2Type>;
  using InputImage3IteratorType = itk::ImageRegionIteratorWithIndex<InputImage3Type>;

  // Create one iterator for Image A (this is a light object)
  InputImage1IteratorType it1(inputImageA, inputImageA->GetBufferedRegion());

  // Initialize the content of Image A
  InputImage1Type::PixelType valueA = 2.0;
  while (!it1.IsAtEnd())
  {
    it1.Set(valueA);
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  InputImage2IteratorType it2(inputImageB, inputImageB->GetBufferedRegion());

  // Initialize the content of Image B
  InputImage2Type::PixelType valueB = 3.0;
  while (!it2.IsAtEnd())
  {
    it2.Set(valueB);
    ++it2;
  }

  // Create one iterator for Image C (this is a light object)
  InputImage3IteratorType it3(inputImageC, inputImageC->GetBufferedRegion());

  // Initialize the content of Image C
  InputImage3Type::PixelType valueC = 4.0;
  while (!it3.IsAtEnd())
  {
    it3.Set(valueC);
    ++it3;
  }

  using TernaryMagnitudeImageFilterType =
    itk::TernaryMagnitudeImageFilter<InputImage1Type, InputImage2Type, InputImage3Type, OutputImageType>;

  // Create the filter
  TernaryMagnitudeImageFilterType::Pointer filter = TernaryMagnitudeImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, TernaryMagnitudeImageFilter, TernaryFunctorImageFilter);

  // Set the input images
  filter->SetInput1(inputImageA);
  filter->SetInput2(inputImageB);
  filter->SetInput3(inputImageC);

  filter->SetFunctor(filter->GetFunctor());

  // Execute the filter
  filter->Update();

  // Get the filter output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Write the result image
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName(argv[1]);

  writer->SetInput(outputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
