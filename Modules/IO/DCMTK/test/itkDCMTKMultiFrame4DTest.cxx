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
#include <iostream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkDCMTKImageIO.h"
#include "itkSubtractImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkTestingMacros.h"

int
itkDCMTKMultiFrame4DTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing Parameters" << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputDicomFile" << std::endl;
    return EXIT_FAILURE;
  }
  using ImageType = itk::Image<unsigned short, 4>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  auto reader = ReaderType::New();
  reader->SetImageIO(itk::DCMTKImageIO::New());
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  ImageType::Pointer im = reader->GetOutput();
  std::cout << im;

  auto writer = WriterType::New();
  writer->SetInput(im);
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // don't want to set imageIO so re-instantiate reader
  reader = ReaderType::New();
  reader->SetFileName(argv[2]);

  using SubtractFilterType = itk::SubtractImageFilter<ImageType, ImageType, ImageType>;

  auto subtractFilter = SubtractFilterType::New();
  subtractFilter->SetInput1(im);
  subtractFilter->SetInput2(reader->GetOutput());

  using StatisticsFilterType = itk::StatisticsImageFilter<ImageType>;
  auto statisticsFilter = StatisticsFilterType::New();

  statisticsFilter->SetInput(subtractFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(statisticsFilter->Update());


  if (statisticsFilter->GetMinimum() != 0.0 || statisticsFilter->GetMaximum() != 0.0)
  {
    std::cerr << "file written doesn't match file read." << std::endl
              << "min(" << statisticsFilter->GetMinimum() << ") max(" << statisticsFilter->GetMaximum() << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
