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

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkTestingMacros.h"

int
itkImageFileWriterStreamingTest1(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input output [existingFile [ no-streaming 1|0] ]" << std::endl;
    return EXIT_FAILURE;
  }

  // We remove the output file
  if (argc == 3)
  {
    itksys::SystemTools::RemoveFile(argv[2]);
  }
  else
  {
    // copy this file to over write
    itksys::SystemTools::CopyAFile(argv[3], argv[2]);
  }


  unsigned int numberOfDataPieces = 4;


  bool forceNoStreamingInput = false;
  if (argc > 4)
  {
    if (std::stoi(argv[4]) == 1)
    {
      forceNoStreamingInput = true;
    }
  }


  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, 3>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->SetUseStreaming(true);

  using MonitorFilter = itk::PipelineMonitorImageFilter<ImageType>;
  auto monitor = MonitorFilter::New();
  monitor->SetInput(reader->GetOutput());

  if (forceNoStreamingInput)
  {
    monitor->UpdateLargestPossibleRegion();
    monitor->VerifyAllInputCanNotStream();
  }

  // Setup the writer
  auto writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(monitor->GetOutput());
  writer->SetNumberOfStreamDivisions(numberOfDataPieces);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  bool passed = true;
  if (!forceNoStreamingInput)
  {
    if (!monitor->VerifyAllInputCanStream(numberOfDataPieces))
    {
      passed = false;
    }
  }
  else
  {
    if (!monitor->VerifyAllNoUpdate())
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
