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

#include "itkCheckerBoardImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"


int
itkCheckerBoardImageFilterTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputImage " << std::endl;
    return EXIT_FAILURE;
  }

  // Define the dimension of the images
  constexpr unsigned int Dimension = 3;

  // Declare the pixel types of the images
  using PixelType = unsigned char;

  // Declare the types of the images
  using ImageType = itk::Image<PixelType, Dimension>;

  // Declare the type of the index to access images
  using IndexType = itk::Index<Dimension>;

  // Declare the type of the size
  using SizeType = itk::Size<Dimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<Dimension>;


  // Declare the type for the filter
  using CheckerBoardImageFilterType = itk::CheckerBoardImageFilter<ImageType>;

  // Declare the type of the arrays that define how many
  // checkers to have along every dimension.
  using CheckerBoardPatternArrayType = CheckerBoardImageFilterType::PatternArrayType;

  // Create the input images
  ImageType::Pointer inputImageA = ImageType::New();
  ImageType::Pointer inputImageB = ImageType::New();

  // Define their size, and start index
  SizeType size;
  size[0] = 40;
  size[1] = 40;
  size[2] = 40;

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


  // Declare appropriate Iterator types for each image
  using IteratorType = itk::ImageRegionIteratorWithIndex<ImageType>;

  // Create one iterator for Image A (this is a light object)
  IteratorType it1(inputImageA, inputImageA->GetBufferedRegion());

  // Initialize the content of Image A
  constexpr ImageType::PixelType input1Value = 2;
  while (!it1.IsAtEnd())
  {
    it1.Set(input1Value);
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  IteratorType it2(inputImageB, inputImageB->GetBufferedRegion());

  // Initialize the content of Image B
  constexpr ImageType::PixelType input2Value = 3;
  while (!it2.IsAtEnd())
  {
    it2.Set(input2Value);
    ++it2;
  }

  // Create the filter
  CheckerBoardImageFilterType::Pointer checkerBoard = CheckerBoardImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(checkerBoard, CheckerBoardImageFilter, ImageToImageFilter);

  // Set the input images
  checkerBoard->SetInput1(inputImageA);
  checkerBoard->SetInput2(inputImageB);

  CheckerBoardPatternArrayType pattern;
  pattern[0] = 4;  // number of checkers along X
  pattern[1] = 8;  // number of checkers along Y
  pattern[2] = 10; // number of checkers along Z

  checkerBoard->SetCheckerPattern(pattern);
  ITK_TEST_SET_GET_VALUE(pattern, checkerBoard->GetCheckerPattern());

  // Execute the filter
  checkerBoard->Update();

  // Get the filter output
  ImageType::Pointer outputImage = checkerBoard->GetOutput();

  // Write the result image
  using WriterType = itk::ImageFileWriter<ImageType>;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName(argv[1]);

  writer->SetInput(outputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
