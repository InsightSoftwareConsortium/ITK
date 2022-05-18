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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkLabelToRGBImageFilter.h"
#include "itkTestingMacros.h"


int
itkLabelToRGBImageFilterTest(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " LabelImage OutputImage" << std::endl;
    return EXIT_FAILURE;
  }


  constexpr int Dimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ColorPixelType = itk::RGBPixel<unsigned char>;
  using ColorImageType = itk::Image<ColorPixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  // Read in the input image
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Instantiate the filter
  using FilterType = itk::LabelToRGBImageFilter<ImageType, ColorImageType>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, LabelToRGBImageFilter, UnaryFunctorImageFilter);


  // Exercising Background Value methods
  typename FilterType::LabelPixelType backgroundValue = 10;
  filter->SetBackgroundValue(backgroundValue);
  ITK_TEST_SET_GET_VALUE(backgroundValue, filter->GetBackgroundValue());

  typename FilterType::OutputPixelType backgroundColor;
  backgroundColor.Fill(itk::NumericTraits<typename FilterType::OutputPixelValueType>::ZeroValue());
  filter->SetBackgroundColor(backgroundColor);
  ITK_TEST_SET_GET_VALUE(backgroundColor, filter->GetBackgroundColor());


  // Set the filter input and label images
  filter->SetInput(reader->GetOutput());

  backgroundValue = 0;
  filter->SetBackgroundValue(backgroundValue);


  itk::SimpleFilterWatcher watcher(filter, "filter");

  // Instantiate output image
  using WriterType = itk::ImageFileWriter<ColorImageType>;
  auto writer = WriterType::New();

  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // exercise the methods to change the colors
  unsigned int numberOfColors1 = filter->GetNumberOfColors();
  filter->AddColor(1, 255, 255);

  unsigned int numberOfColors2 = filter->GetNumberOfColors();

  if (numberOfColors2 != numberOfColors1 + 1)
  {
    std::cerr << "Error in GetNumberOfColors() or AddColor() " << std::endl;
    return EXIT_FAILURE;
  }

  filter->ResetColors();
  filter->AddColor(255, 255, 255);

  unsigned int numberOfColors3 = filter->GetNumberOfColors();

  if (numberOfColors3 != 1)
  {
    std::cerr << "Error in GetNumberOfColors() or ResetColors() or AddColor() " << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
