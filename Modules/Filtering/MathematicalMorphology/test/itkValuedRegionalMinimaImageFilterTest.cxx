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

#include "itkValuedRegionalMinimaImageFilter.h"
#include "itkMaximumImageFilter.h"
#include "itkHConcaveImageFilter.h"
#include "itkInvertIntensityImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkAndImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


// A test routine for regional extrema using flooding
int
itkValuedRegionalMinimaImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImageFile"
              << " outputImageFile1"
              << " outputImageFile2"
              << " fullyConnected";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using FilterType = itk::ValuedRegionalMinimaImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, ValuedRegionalMinimaImageFilter, ValuedRegionalExtremaImageFilter);

  bool fullyConnected = std::stoi(argv[4]);
  ITK_TEST_SET_GET_BOOLEAN(filter, FullyConnected, fullyConnected);


  itk::SimpleFilterWatcher watcher(filter, "ValuedRegionalMinimaImageFilter");

  filter->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);


  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  // Produce the same output with other filters
  using ConcaveFilterType = itk::HConcaveImageFilter<ImageType, ImageType>;
  auto concaveFilter = ConcaveFilterType::New();
  concaveFilter->SetInput(reader->GetOutput());
  concaveFilter->SetFullyConnected(fullyConnected);
  concaveFilter->SetHeight(1);

  // Concave gives minima with value=1 and others with value = 0
  // Rescale the image so we have minima = 255 other = 0
  using RescaleFilterType = itk::RescaleIntensityImageFilter<ImageType, ImageType>;
  auto rescaler = RescaleFilterType::New();
  rescaler->SetInput(concaveFilter->GetOutput());
  rescaler->SetOutputMaximum(255);
  rescaler->SetOutputMinimum(0);

  // In the input image, select the values of the pixel at the minima
  using AndFilterType = itk::AndImageFilter<ImageType, ImageType, ImageType>;
  auto andFilter = AndFilterType::New();
  andFilter->SetInput(0, rescaler->GetOutput());
  andFilter->SetInput(1, reader->GetOutput());

  // All pixel which are not minima must have value = 255.
  // Get the non minima pixel by inverting the rescaled image
  // We will have minima value = 0 and non minima value = 255
  using InvertFilterType = itk::InvertIntensityImageFilter<ImageType, ImageType>;
  auto invertFilter = InvertFilterType::New();
  invertFilter->SetInput(rescaler->GetOutput());

  // Get the highest value from the and and invert filters. The minima have
  // value >= 0 in "a" image and the non minima have a value=0. In invert,
  // the non minima have a value=255 and the minima a value=0
  using MaxType = itk::MaximumImageFilter<ImageType, ImageType, ImageType>;
  auto max = MaxType::New();
  max->SetInput(0, invertFilter->GetOutput());
  max->SetInput(1, andFilter->GetOutput());

  auto writer2 = WriterType::New();
  writer2->SetInput(andFilter->GetOutput());
  writer2->SetFileName(argv[3]);
  writer2->Update();

  std::cerr << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
