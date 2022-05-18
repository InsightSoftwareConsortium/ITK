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

#include "itkSimpleFilterWatcher.h"
#include "itkGrayscaleGeodesicDilateImageFilter.h"
#include "itkGrayscaleGeodesicErodeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkTestingMacros.h"

// This test should produce the same results as the
// itkHMaximaMinimaImageFilterTest.

int
itkGrayscaleGeodesicErodeDilateImageFilterTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " Inputimage OutputImage Height fullyConnected runOneIteration" << std::endl;
    return EXIT_FAILURE;
  }
  constexpr int Dimension = 2;
  using PixelType = unsigned char;
  using InputImageType = itk::Image<PixelType, Dimension>;
  using OutputImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  using ShiftFilterType = itk::ShiftScaleImageFilter<InputImageType, InputImageType>;
  using DilateFilterType = itk::GrayscaleGeodesicDilateImageFilter<InputImageType, OutputImageType>;
  using ErodeFilterType = itk::GrayscaleGeodesicErodeImageFilter<InputImageType, OutputImageType>;

  auto reader = ReaderType::New();
  auto writer = WriterType::New();
  auto shiftErode = ShiftFilterType::New();
  auto shiftDilate = ShiftFilterType::New();

  auto erode = ErodeFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(erode, GrayscaleGeodesicErodeImageFilter, ImageToImageFilter);


  auto dilate = DilateFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(dilate, GrayscaleGeodesicDilateImageFilter, ImageToImageFilter);

  // Create the reader and writer
  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  // Create the marker image for erosion
  shiftDilate->SetInput(reader->GetOutput());
  shiftDilate->SetShift(-1 * std::stoi(argv[3]));

  // Dilate
  dilate->SetMarkerImage(shiftDilate->GetOutput());
  dilate->SetMaskImage(reader->GetOutput());
  auto fullyConnected = static_cast<bool>(std::stoi(argv[4]));

  ITK_TEST_SET_GET_BOOLEAN(dilate, FullyConnected, fullyConnected);

  auto runOneIteration = static_cast<bool>(std::stoi(argv[5]));

  ITK_TEST_SET_GET_BOOLEAN(dilate, RunOneIteration, runOneIteration);

  ITK_TRY_EXPECT_NO_EXCEPTION(dilate->Update());

  std::cout << "Dilate filter NumberOfIterationsUsed: " << dilate->GetNumberOfIterationsUsed() << std::endl;

  // Create the marker image for erode
  shiftErode->SetInput(dilate->GetOutput());
  shiftErode->SetShift(std::stoi(argv[3]));

  // Erode
  erode->SetMarkerImage(shiftErode->GetOutput());
  erode->SetMaskImage(dilate->GetOutput());

  ITK_TEST_SET_GET_BOOLEAN(erode, FullyConnected, fullyConnected);

  ITK_TEST_SET_GET_BOOLEAN(erode, RunOneIteration, runOneIteration);

  ITK_TRY_EXPECT_NO_EXCEPTION(erode->Update());

  std::cout << "Erode filter NumberOfIterationsUsed: " << erode->GetNumberOfIterationsUsed() << std::endl;

  writer->SetInput(erode->GetOutput());

  itk::SimpleFilterWatcher watchDilate(dilate);
  itk::SimpleFilterWatcher watchErode(erode);

  // Execute the filter
  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught:" << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
