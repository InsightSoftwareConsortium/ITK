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

#include "itkBinaryGrindPeakImageFilter.h"
#include "itkTestingMacros.h"


int
itkBinaryGrindPeakImageFilterTest1(int argc, char * argv[])
{

  if (argc != 6)
  {
    std::cerr << "Usage: " << argv[0] << " input output fullyConnected foreground background" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  using BinaryGrindPeakImageFilterType = itk::BinaryGrindPeakImageFilter<ImageType>;
  BinaryGrindPeakImageFilterType::Pointer binaryGrindPeakImageFilter = BinaryGrindPeakImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(binaryGrindPeakImageFilter, BinaryGrindPeakImageFilter, ImageToImageFilter);

  binaryGrindPeakImageFilter->SetInput(reader->GetOutput());

  bool fullyConnected = std::stoi(argv[3]) != 0;
  binaryGrindPeakImageFilter->SetFullyConnected(fullyConnected);
  ITK_TEST_SET_GET_VALUE(fullyConnected, binaryGrindPeakImageFilter->GetFullyConnected());

  if (fullyConnected)
  {
    binaryGrindPeakImageFilter->FullyConnectedOn();
    ITK_TEST_SET_GET_VALUE(true, binaryGrindPeakImageFilter->GetFullyConnected());
  }
  else
  {
    binaryGrindPeakImageFilter->FullyConnectedOff();
    ITK_TEST_SET_GET_VALUE(false, binaryGrindPeakImageFilter->GetFullyConnected());
  }

  auto foregroundValue = static_cast<BinaryGrindPeakImageFilterType::InputImagePixelType>(std::stod(argv[4]));
  binaryGrindPeakImageFilter->SetForegroundValue(foregroundValue);
  ITK_TEST_SET_GET_VALUE(foregroundValue, binaryGrindPeakImageFilter->GetForegroundValue());

  auto backgroundValue = static_cast<BinaryGrindPeakImageFilterType::InputImagePixelType>(std::stoi(argv[5]));
  binaryGrindPeakImageFilter->SetBackgroundValue(backgroundValue);
  ITK_TEST_SET_GET_VALUE(backgroundValue, binaryGrindPeakImageFilter->GetBackgroundValue());


  itk::SimpleFilterWatcher watcher(binaryGrindPeakImageFilter, "BinaryGrindPeakImageFilter");

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(binaryGrindPeakImageFilter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
