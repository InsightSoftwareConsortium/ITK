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

#include "itkConvolutionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkStreamingImageFilter.h"
#include "itkTestingMacros.h"

int
itkConvolutionImageFilterTestInt(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Missing parameters." << '\n';
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImage kernelImage outputImage [normalizeImage] [outputRegionMode]" << '\n';
    return EXIT_FAILURE;
  }

  constexpr int ImageDimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader1 = ReaderType::New();
  reader1->SetFileName(argv[1]);

  auto reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  using ConvolutionFilterType = itk::ConvolutionImageFilter<ImageType>;
  auto convolver = ConvolutionFilterType::New();
  convolver->SetInput(reader1->GetOutput());
  convolver->SetKernelImage(reader2->GetOutput());

  const itk::SimpleFilterWatcher watcher(convolver, "filter");

  if (argc >= 5)
  {
    convolver->SetNormalize(static_cast<bool>(std::stoi(argv[4])));
  }

  if (argc >= 6)
  {
    const std::string outputRegionMode(argv[5]);
    if (outputRegionMode == "SAME")
    {
      convolver->SetOutputRegionModeToSame();
      std::cout << "OutputRegionMode set to SAME." << '\n';
    }
    else if (outputRegionMode == "VALID")
    {
      convolver->SetOutputRegionModeToValid();
      std::cout << "OutputRegionMode set to VALID." << '\n';
    }
    else
    {
      std::cerr << "Invalid OutputRegionMode '" << outputRegionMode << "'." << '\n';
      std::cerr << "Valid values are SAME or VALID." << '\n';
      return EXIT_FAILURE;
    }
  }

  using MonitorFilter = itk::PipelineMonitorImageFilter<ImageType>;

  auto monitor = MonitorFilter::New();
  monitor->SetInput(convolver->GetOutput());

  constexpr unsigned int                                         numberOfStreamDivisions = 4;
  const itk::StreamingImageFilter<ImageType, ImageType>::Pointer streamingFilter =
    itk::StreamingImageFilter<ImageType, ImageType>::New();
  streamingFilter->SetNumberOfStreamDivisions(numberOfStreamDivisions);
  streamingFilter->SetInput(monitor->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(argv[3]);
  writer->SetInput(streamingFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  if (!monitor->VerifyAllInputCanStream(numberOfStreamDivisions))
  {
    std::cerr << "ConvolutionImageFilter failed to stream as expected!" << '\n';
    std::cerr << monitor;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
