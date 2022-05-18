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

#include "itkRegionalMaximaImageFilter.h"
#include "itkHConvexImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


template <typename TInputImage, typename TOutputImage>
int
RegionalMaximaImageFilterTestHelper(std::string inputImageFile,
                                    std::string outputImageFile,
                                    std::string outputImageFile2,
                                    bool        fullyConnected,
                                    bool        flatIsMaxima)
{
  using InputImageType = TInputImage;
  using OutputImageType = TInputImage;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(inputImageFile);

  using FilterType = itk::RegionalMaximaImageFilter<InputImageType, OutputImageType>;
  auto filter = FilterType::New();

  filter->SetInput(reader->GetOutput());

  ITK_TEST_SET_GET_BOOLEAN(filter, FullyConnected, fullyConnected);

  ITK_TEST_SET_GET_BOOLEAN(filter, FlatIsMaxima, flatIsMaxima);

  typename FilterType::OutputImagePixelType foregroundValue =
    itk::NumericTraits<typename FilterType::OutputImagePixelType>::max();
  filter->SetForegroundValue(foregroundValue);
  ITK_TEST_SET_GET_VALUE(foregroundValue, filter->GetForegroundValue());

  typename FilterType::OutputImagePixelType backgroundValue =
    itk::NumericTraits<typename FilterType::OutputImagePixelType>::NonpositiveMin();
  filter->SetBackgroundValue(backgroundValue);
  ITK_TEST_SET_GET_VALUE(backgroundValue, filter->GetBackgroundValue());

  itk::SimpleFilterWatcher watcher(filter, "RegionalMaximaImageFilter");

  // Write the output images
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(outputImageFile);
  writer->Update();


  // Produce the same output with other filters
  using ConvexFilterType = itk::HConvexImageFilter<InputImageType, InputImageType>;
  auto convexFilter = ConvexFilterType::New();
  convexFilter->SetInput(reader->GetOutput());
  convexFilter->SetFullyConnected(fullyConnected);
  convexFilter->SetHeight(1);

  // Convex gives maxima with value = 1 and others with value = 0
  // Rescale the image so we have maxima = 255 other = 0
  using RescaleFilterType = itk::RescaleIntensityImageFilter<InputImageType, OutputImageType>;
  auto rescaler = RescaleFilterType::New();
  rescaler->SetInput(convexFilter->GetOutput());
  rescaler->SetOutputMaximum(255);
  rescaler->SetOutputMinimum(0);

  auto writer2 = WriterType::New();
  writer2->SetInput(rescaler->GetOutput());
  writer2->SetFileName(outputImageFile2);
  writer2->Update();


  std::cerr << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}

// A test routine for regional extrema using flooding
int
itkRegionalMaximaImageFilterTest(int argc, char * argv[])
{
  if (argc < 7)
  {
    std::cerr << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImageFile"
              << " outputImageFile"
              << " outputImageFile2"
              << " dimension"
              << " fullyConnected"
              << " flatIsMaxima";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  std::string inputImageFile = argv[1];
  std::string outputImageFile = argv[2];
  std::string outputImageFile2 = argv[3];

  unsigned int dimension = std::stoi(argv[4]);

  bool fullyConnected = std::stoi(argv[5]);
  bool flatIsMaxima = std::stoi(argv[6]);

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, 2>;

  using FilterType = itk::RegionalMaximaImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, RegionalMaximaImageFilter, ImageToImageFilter);

  if (dimension == 2)
  {
    using Image2DType = itk::Image<PixelType, 2>;
    return RegionalMaximaImageFilterTestHelper<Image2DType, Image2DType>(
      inputImageFile, outputImageFile, outputImageFile2, fullyConnected, flatIsMaxima);
  }
  else if (dimension == 3)
  {
    using Image3DType = itk::Image<PixelType, 3>;
    return RegionalMaximaImageFilterTestHelper<Image3DType, Image3DType>(
      inputImageFile, outputImageFile, outputImageFile2, fullyConnected, flatIsMaxima);
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Unsupported dimension: " << dimension << std::endl;
    std::cerr << "Only dimensions 2 and 3 are supported." << std::endl;
    return EXIT_FAILURE;
  }
}
