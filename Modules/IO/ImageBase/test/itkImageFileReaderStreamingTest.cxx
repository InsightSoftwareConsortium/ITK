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

#include <fstream>
#include "itkImageFileReader.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkTestingMacros.h"

int
itkImageFileReaderStreamingTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " input [expected-to-stream 1|0 [ force-no-streaming 1|0] ]" << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int numberOfDataPieces = 4;

  bool expectedToStream = true;
  if (argc > 2)
  {
    if (std::stoi(argv[2]) != 1)
    {
      expectedToStream = false;
    }
  }

  bool forceNoStreamingInput = false;
  if (argc > 3)
  {
    if (std::stoi(argv[3]) == 1)
    {
      forceNoStreamingInput = true;
    }
  }


  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, 3>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->SetUseStreaming(true);

  using MonitorFilter = itk::PipelineMonitorImageFilter<ImageType>;
  MonitorFilter::Pointer monitor = MonitorFilter::New();
  monitor->SetInput(reader->GetOutput());


  using StreamingFilter = itk::StreamingImageFilter<ImageType, ImageType>;
  StreamingFilter::Pointer streamer = StreamingFilter::New();
  streamer->SetInput(monitor->GetOutput());
  streamer->SetNumberOfStreamDivisions(numberOfDataPieces);


  try
  {
    if (forceNoStreamingInput)
    {
      monitor->UpdateLargestPossibleRegion();
    }
    else
    {
      streamer->Update();
    }
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }


  bool passed = true;
  if (expectedToStream)
  {
    if (!monitor->VerifyAllInputCanStream(numberOfDataPieces))
    {
      passed = false;
    }
  }
  else
  {
    if (!monitor->VerifyAllInputCanNotStream())
    {
      passed = false;
    }
  }


  if (!passed)
  {
    std::cout << monitor << std::endl;
    std::cout << "pipeline did not execute as expected!" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
