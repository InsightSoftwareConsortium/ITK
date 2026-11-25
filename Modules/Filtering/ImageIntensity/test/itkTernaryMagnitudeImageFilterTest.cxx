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
  constexpr unsigned int Dimension{ 3 };

  // Define the pixel types
  using InputPixelType = float;
  using OutputPixelType = unsigned short;

  // Declare the types of the images
  using InputImage1Type = itk::Image<InputPixelType, Dimension>;
  using InputImage2Type = itk::Image<InputPixelType, Dimension>;
  using InputImage3Type = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  // Declare the type of the size
  using SizeType = itk::Size<Dimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<Dimension>;

  // Create the input images
  auto inputImageA = InputImage1Type::New();
  auto inputImageB = InputImage2Type::New();
  auto inputImageC = InputImage3Type::New();

  // Define their size and region
  constexpr SizeType size{ 2, 2, 2 };
  RegionType         region{ size };

  // Initialize Image A
  inputImageA->SetRegions(region);
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetRegions(region);
  inputImageB->Allocate();

  // Initialize Image C
  inputImageC->SetRegions(region);
  inputImageC->Allocate();

  // Initialize the content of Image A
  constexpr InputImage1Type::PixelType valueA{ 2.0 };
  inputImageA->FillBuffer(valueA);

  // Initialize the content of Image B
  constexpr InputImage2Type::PixelType valueB{ 3.0 };
  inputImageB->FillBuffer(valueB);

  // Initialize the content of Image C
  constexpr InputImage3Type::PixelType valueC{ 4.0 };
  inputImageC->FillBuffer(valueC);

  using TernaryMagnitudeImageFilterType =
    itk::TernaryMagnitudeImageFilter<InputImage1Type, InputImage2Type, InputImage3Type, OutputImageType>;

  // Create the filter
  auto filter = TernaryMagnitudeImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, TernaryMagnitudeImageFilter, TernaryGeneratorImageFilter);

  // Set the input images
  filter->SetInput1(inputImageA);
  filter->SetInput2(inputImageB);
  filter->SetInput3(inputImageC);

  // Execute the filter
  filter->Update();

  // Get the filter output
  const OutputImageType::Pointer outputImage = filter->GetOutput();

  // Write the result image
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto writer = WriterType::New();

  writer->SetFileName(argv[1]);

  writer->SetInput(outputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
