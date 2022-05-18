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
#include "itkShiftScaleImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkImageRegionSplitterMultidimensional.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkTestingMacros.h"


int
itkStreamingImageFilterTest3(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << "  inputImageFile outputImageFile numberOfStreamDivisions" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string inputFilename = argv[1];
  const std::string outputFilename = argv[2];
  unsigned int      numberOfStreamDivisions = std::stoi(argv[3]);

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, 2>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(inputFilename);

  using SomeFilter = itk::ShiftScaleImageFilter<ImageType, ImageType>;
  auto filter = SomeFilter::New();
  filter->SetInput(reader->GetOutput());

  // monitor what's going on
  itk::PipelineMonitorImageFilter<ImageType>::Pointer monitor;
  monitor = itk::PipelineMonitorImageFilter<ImageType>::New();
  monitor->SetInput(filter->GetOutput());

  itk::ImageRegionSplitterMultidimensional::Pointer splitter;
  splitter = itk::ImageRegionSplitterMultidimensional::New();

  itk::StreamingImageFilter<ImageType, ImageType>::Pointer streamer;
  streamer = itk::StreamingImageFilter<ImageType, ImageType>::New();
  streamer->SetInput(monitor->GetOutput());
  streamer->SetNumberOfStreamDivisions(numberOfStreamDivisions);
  streamer->SetRegionSplitter(splitter);

  itk::WriteImage(streamer->GetOutput(), outputFilename);

  unsigned int expectedNumberOfStreams =
    splitter->GetNumberOfSplits(streamer->GetOutput()->GetLargestPossibleRegion(), numberOfStreamDivisions);

  std::cout << "ExpectedNumberOfStreams: " << expectedNumberOfStreams << std::endl;

  if (!monitor->VerifyAllInputCanStream(expectedNumberOfStreams))
  {
    std::cout << "Filter failed to execute as expected!" << std::endl;
    std::cout << monitor;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
