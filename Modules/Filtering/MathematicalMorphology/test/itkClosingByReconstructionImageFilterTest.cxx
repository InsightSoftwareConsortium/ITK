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

#include "itkClosingByReconstructionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkSubtractImageFilter.h"
#include "itkTestingMacros.h"


int
itkClosingByReconstructionImageFilterTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " Inputimage OutputImage Radius PreserveIntensities(0,1) fullyConnected [DiffImage]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int Dimension = 2;
  using PixelType = unsigned char;
  using InputImageType = itk::Image<PixelType, Dimension>;
  using OutputImageType = itk::Image<PixelType, Dimension>;

  // Declare the type of the Structuring element to be used
  using StructuringElementType = itk::BinaryBallStructuringElement<PixelType, Dimension>;

  // Declare the type for the Morphology Filters to be Tested
  using MorphologicalFilterType =
    itk::ClosingByReconstructionImageFilter<InputImageType, OutputImageType, StructuringElementType>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  auto reader = ReaderType::New();

  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  // Create the filter
  auto filter = MorphologicalFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, ClosingByReconstructionImageFilter, ImageToImageFilter);


  StructuringElementType structuringElement;
  structuringElement.SetRadius(std::stoi(argv[3]));
  structuringElement.CreateStructuringElement();

  filter->SetKernel(structuringElement);
  ITK_TEST_SET_GET_VALUE(structuringElement, filter->GetKernel());

  bool preserveIntensities = static_cast<bool>(std::stoi(argv[4]));
  ITK_TEST_SET_GET_BOOLEAN(filter, PreserveIntensities, preserveIntensities);

  bool fullyConnected = static_cast<bool>(std::stoi(argv[5]));
  ITK_TEST_SET_GET_BOOLEAN(filter, FullyConnected, fullyConnected);

  filter->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Write the output
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();

  writer->SetFileName(argv[2]);
  writer->SetInput(filter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // Create a difference image if one is requested
  if (argc == 7)
  {
    itk::SubtractImageFilter<InputImageType, OutputImageType, OutputImageType>::Pointer subtract =
      itk::SubtractImageFilter<InputImageType, OutputImageType, OutputImageType>::New();
    subtract->SetInput(1, reader->GetOutput());
    subtract->SetInput(0, filter->GetOutput());

    writer->SetFileName(argv[6]);
    writer->SetInput(subtract->GetOutput());

    ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
