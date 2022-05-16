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

//

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkGrayscaleConnectedOpeningImageFilter.h"
#include "itkTestingMacros.h"


int
itkGrayscaleConnectedOpeningImageFilterTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputImageFile outputImageFile seedX seedY fullyConnected" << std::endl;
    return EXIT_FAILURE;
  }


  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned char;
  using OutputPixelType = unsigned char;
  using WritePixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using WriteImageType = itk::Image<WritePixelType, Dimension>;

  // Define the fillhole filter
  using ConnectedOpeningFilterType = itk::GrayscaleConnectedOpeningImageFilter<InputImageType, OutputImageType>;

  // Create the filter
  auto connectedOpening = ConnectedOpeningFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(connectedOpening, GrayscaleConnectedOpeningImageFilter, ImageToImageFilter);


  itk::SimpleFilterWatcher watcher(connectedOpening, "Opening");
  watcher.QuietOn();

  using ReaderType = itk::ImageFileReader<InputImageType>;
  auto reader = ReaderType::New();

  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  // Set up the ConnectedOpening filter
  auto fullyConnected = static_cast<bool>(std::stoi(argv[5]));
  ITK_TEST_SET_GET_BOOLEAN(connectedOpening, FullyConnected, fullyConnected);

  connectedOpening->SetInput(reader->GetOutput());

  InputImageType::IndexType seed;
  seed[0] = std::stoi(argv[3]);
  seed[1] = std::stoi(argv[4]);
  connectedOpening->SetSeed(seed);
  ITK_TEST_SET_GET_VALUE(seed, connectedOpening->GetSeed());

  ITK_TRY_EXPECT_NO_EXCEPTION(connectedOpening->Update());


  using WriterType = itk::ImageFileWriter<WriteImageType>;
  auto writer = WriterType::New();

  writer->SetFileName(argv[2]);
  writer->SetInput(connectedOpening->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
