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

#include "itkGrayscaleGrindPeakImageFilter.h"
#include "itkTestingMacros.h"

int
itkGrayscaleGrindPeakImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing arguments." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImageFile"
              << " outputImageFile"
              << " fullyConnected" << std::endl;
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

  using ReaderType = itk::ImageFileReader<InputImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);


  // Create the filter
  using GrindPeakFilterType = itk::GrayscaleGrindPeakImageFilter<InputImageType, OutputImageType>;
  auto grindpeak = GrindPeakFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(grindpeak, GrayscaleGrindPeakImageFilter, ImageToImageFilter);

  grindpeak->SetInput(reader->GetOutput());

  auto fullyConnected = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(grindpeak, FullyConnected, fullyConnected);

  ITK_TRY_EXPECT_NO_EXCEPTION(grindpeak->Update());

  // Write the output
  using WriterType = itk::ImageFileWriter<WriteImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(grindpeak->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
