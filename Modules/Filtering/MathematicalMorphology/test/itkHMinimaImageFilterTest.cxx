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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkHMinimaImageFilter.h"
#include "itkTestingMacros.h"

int
itkHMinimaImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImageFile"
              << " outputImageFile"
              << " height"
              << " fullyConnected" << std::endl;
    return EXIT_FAILURE;
  }

  //
  // The following code defines the input and output pixel types and their
  // associated image types.
  //
  constexpr unsigned int Dimension = 2;

  using InputPixelType = short;
  using OutputPixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  // Read the input image
  using ReaderType = itk::ImageFileReader<InputImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  // Define the itk::HMinimaImageFilter filter type
  using HMinimaFilterType = itk::HMinimaImageFilter<InputImageType, OutputImageType>;

  // Create the filter
  HMinimaFilterType::Pointer hMinimaFilter = HMinimaFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(hMinimaFilter, HMinimaImageFilter, ImageToImageFilter);

  itk::SimpleFilterWatcher watchHMinima(hMinimaFilter, "HMinimaImageFilter");

  // Set up the filter
  auto height = static_cast<HMinimaFilterType::InputImagePixelType>(std::stod(argv[3]));

  hMinimaFilter->SetHeight(height);
  ITK_TEST_SET_GET_VALUE(height, hMinimaFilter->GetHeight());

  auto fullyConnected = static_cast<bool>(std::stod(argv[4]));
  ITK_TEST_SET_GET_BOOLEAN(hMinimaFilter, FullyConnected, fullyConnected);


  hMinimaFilter->SetInput(reader->GetOutput());

  // Run the filter
  ITK_TRY_EXPECT_NO_EXCEPTION(hMinimaFilter->Update());


  // Write the output
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(hMinimaFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
