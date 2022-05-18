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

#include "itkOpeningByReconstructionImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkOpeningByReconstructionImageFilterTest2(int argc, char * argv[])
{
  if (argc < 9)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr
      << "Usage: " << itkNameOfTestExecutableMacro(argv)
      << " OutputImage Radius PreserveIntensities(0,1) fullyConnected OriginX OriginY SpacingX SpacingY [DiffImage]"
      << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int Dimension = 2;
  using PixelType = unsigned char;
  using InputImageType = itk::Image<PixelType, Dimension>;
  using OutputImageType = itk::Image<PixelType, Dimension>;
  using RegionType = InputImageType::RegionType;
  using SizeType = InputImageType::SizeType;
  using IndexType = InputImageType::IndexType;
  using SpacingType = InputImageType::SpacingType;
  using OriginType = InputImageType::PointType;

  // Declare the type of the Structuring element to be used
  using StructuringElementType = itk::BinaryBallStructuringElement<PixelType, Dimension>;

  // Declare the type for the Morphology Filters to be Tested
  using MorphologicalFilterType =
    itk::OpeningByReconstructionImageFilter<InputImageType, OutputImageType, StructuringElementType>;

  // Create image
  auto inputImage = InputImageType::New();

  // Define regions of input image
  RegionType region;
  SizeType   size;
  size.Fill(std::stoi(argv[2]));
  IndexType index;
  index.Fill(0);
  region.SetSize(size);
  region.SetIndex(index);

  // fill spacing and origin
  OriginType origin;
  origin[0] = std::stod(argv[5]);
  origin[1] = std::stod(argv[6]);
  inputImage->SetOrigin(origin);

  SpacingType spacing;
  spacing[0] = std::stod(argv[7]);
  spacing[1] = std::stod(argv[8]);
  inputImage->SetSpacing(spacing);


  inputImage->SetRegions(region);
  inputImage->Allocate();
  // Fill with zero values
  inputImage->FillBuffer(static_cast<PixelType>(0));

  // Create the filter
  auto filter = MorphologicalFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, OpeningByReconstructionImageFilter, ImageToImageFilter);


  itk::SimpleFilterWatcher watcher(filter, "Opening");
  watcher.QuietOn();

  StructuringElementType structuringElement;

  structuringElement.SetRadius(std::stoi(argv[2]));
  structuringElement.CreateStructuringElement();

  filter->SetKernel(structuringElement);
  ITK_TEST_SET_GET_VALUE(structuringElement, filter->GetKernel());

  bool preserveIntensities = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(filter, PreserveIntensities, preserveIntensities);

  bool fullyConnected = static_cast<bool>(std::stoi(argv[4]));
  ITK_TEST_SET_GET_BOOLEAN(filter, FullyConnected, fullyConnected);

  filter->SetInput(inputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Write the output
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();

  writer->SetFileName(argv[1]);
  writer->SetInput(filter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // Create a difference image if one is requested
  if (argc == 10)
  {
    itk::SubtractImageFilter<InputImageType, OutputImageType, OutputImageType>::Pointer subtract =
      itk::SubtractImageFilter<InputImageType, OutputImageType, OutputImageType>::New();
    subtract->SetInput(0, inputImage);
    subtract->SetInput(1, filter->GetOutput());

    writer->SetFileName(argv[9]);
    writer->SetInput(subtract->GetOutput());

    ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
