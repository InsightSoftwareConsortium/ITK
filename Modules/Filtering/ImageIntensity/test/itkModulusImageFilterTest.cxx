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
#include "itkModulusImageFilter.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkModulusImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImage outputImage " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // get the distance map inside the spots
  // spot are already black so there is no need to invert the image
  using DistanceFilter = itk::DanielssonDistanceMapImageFilter<ImageType, ImageType>;
  DistanceFilter::Pointer distance = DistanceFilter::New();
  distance->SetInput(reader->GetOutput());

  using FilterType = itk::ModulusImageFilter<ImageType, ImageType>;
  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, ModulusImageFilter, BinaryGeneratorImageFilter);

  filter->SetInput(distance->GetOutput());

  FilterType::InputPixelType dividend = 8;
  filter->SetDividend(dividend);
  ITK_TEST_SET_GET_VALUE(dividend, filter->GetDividend())

  filter->InPlaceOn();

  itk::SimpleFilterWatcher watcher(filter);

  using ThresholdType = itk::RescaleIntensityImageFilter<ImageType, ImageType>;
  ThresholdType::Pointer rescale = ThresholdType::New();
  rescale->SetInput(filter->GetOutput());
  rescale->SetOutputMaximum(itk::NumericTraits<PixelType>::max());
  rescale->SetOutputMinimum(itk::NumericTraits<PixelType>::NonpositiveMin());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(rescale->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
