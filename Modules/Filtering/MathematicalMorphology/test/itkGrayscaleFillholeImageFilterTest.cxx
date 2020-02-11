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

//

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"
#include "itkGrayscaleFillholeImageFilter.h"


int
itkGrayscaleFillholeImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << "  inputImageFile  ";
    std::cerr << " outputImageFile  " << std::endl;
    std::cerr << " fullyConnected " << std::endl;
    return EXIT_FAILURE;
  }


  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  constexpr unsigned int Dimension = 2;

  using InputPixelType = short;
  using OutputPixelType = short;
  using WritePixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using WriteImageType = itk::Image<WritePixelType, Dimension>;


  // readers/writers
  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<WriteImageType>;
  using RescaleType = itk::RescaleIntensityImageFilter<OutputImageType, WriteImageType>;

  // define the fillhole filter
  using FillholeFilterType = itk::GrayscaleFillholeImageFilter<InputImageType, OutputImageType>;


  // Creation of Reader and Writer filters
  ReaderType::Pointer  reader = ReaderType::New();
  WriterType::Pointer  writer = WriterType::New();
  RescaleType::Pointer rescaler = RescaleType::New();

  // Create the filter
  FillholeFilterType::Pointer fillhole = FillholeFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(fillhole, GrayscaleFillholeImageFilter, ImageToImageFilter);

  itk::SimpleFilterWatcher watcher(fillhole, "fillhole");
  watcher.QuietOn();

  auto fullyConnected = static_cast<bool>(atoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(fillhole, FullyConnected, fullyConnected);

  // Setup the input and output files
  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  // Setup the fillhole method
  fillhole->SetInput(reader->GetOutput());

  // Run the filter
  rescaler->SetInput(fillhole->GetOutput());
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);
  writer->SetInput(rescaler->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;
}
