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

#include "itkGrayscaleMorphologicalOpeningImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkSimpleFilterWatcher.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int
itkGrayscaleMorphologicalOpeningImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing arguments." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << "  inputImage outputImage " << std::endl;
    return EXIT_FAILURE;
  }

  // Define the dimension of the images
  constexpr unsigned int Dimension = 2;

  // Define the pixel type
  using PixelType = unsigned char;

  // Declare the types of the images
  using ImageType = itk::Image<PixelType, Dimension>;

  // Declare the reader and writer
  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;


  // Declare the type for the structuring element
  using KernelType = itk::BinaryBallStructuringElement<PixelType, Dimension>;

  // Declare the type for the morphology Filter
  using FilterType = itk::GrayscaleMorphologicalOpeningImageFilter<ImageType, ImageType, KernelType>;

  // Create the reader and writer
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  // Create the filter
  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, GrayscaleMorphologicalOpeningImageFilter, KernelImageFilter);

  itk::SimpleFilterWatcher watcher(filter, "filter");

  // Connect the pipeline
  filter->SetInput(reader->GetOutput());
  writer->SetInput(filter->GetOutput());

  // Create the structuring element
  KernelType           ball;
  KernelType::SizeType ballSize;
  ballSize[0] = 2;
  ballSize[1] = 2;
  ball.SetRadius(ballSize);
  ball.CreateStructuringElement();

  // Connect the structuring element
  filter->SetKernel(ball);

  // Execute the filter
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
