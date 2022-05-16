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

#include "itkSimpleContourExtractorImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkBoxImageFilter.h"
#include "itkTestingMacros.h"

int
itkSimpleContourExtractorImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing arguments." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImage outputImage " << std::endl;
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


  // Declare the type for the morphology Filter
  using FilterType = itk::SimpleContourExtractorImageFilter<ImageType, ImageType>;

  // Create the reader and writer
  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  // Create the filter
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, SimpleContourExtractorImageFilter, BoxImageFilter);

  itk::SimpleFilterWatcher watcher(filter, "filter");

  // Connect the pipeline
  filter->SetInput(reader->GetOutput());
  writer->SetInput(filter->GetOutput());

  FilterType::InputPixelType  inputForegroundValue = 255;
  FilterType::InputPixelType  inputBackgroundValue = 0;
  FilterType::OutputPixelType outputForegroundValue = itk::NumericTraits<FilterType::OutputPixelType>::max();
  FilterType::OutputPixelType outputBackgroundValue = itk::NumericTraits<FilterType::OutputPixelType>::ZeroValue();

  filter->SetInputForegroundValue(inputForegroundValue);

  ITK_TEST_SET_GET_VALUE(inputForegroundValue, filter->GetInputForegroundValue());

  filter->SetInputBackgroundValue(inputBackgroundValue);

  ITK_TEST_SET_GET_VALUE(inputBackgroundValue, filter->GetInputBackgroundValue());

  filter->SetOutputForegroundValue(outputForegroundValue);

  ITK_TEST_SET_GET_VALUE(outputForegroundValue, filter->GetOutputForegroundValue());

  filter->SetOutputBackgroundValue(outputBackgroundValue);

  ITK_TEST_SET_GET_VALUE(outputBackgroundValue, filter->GetOutputBackgroundValue());


  FilterType::InputSizeType radius;

  radius.Fill(1);

  filter->SetRadius(radius);

  // Exercise Print()
  filter->Print(std::cout);

  // Execute the filter
  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception caught during pipeline Update\n" << e;
    return EXIT_FAILURE;
  }

  // All objects should be automatically destroyed at this point

  return EXIT_SUCCESS;
}
