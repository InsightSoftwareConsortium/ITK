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
#include "itkVTKImageIO.h"

#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"

#include "itkPipelineMonitorImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkTestingMacros.h"

int
itkVTKImageIO2Test2(int argc, char * argv[])
{
  //
  // This test is designed to test the non-streaming capabilities of
  // VTKImageIO with tensors
  //

  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " outputFileName" << std::endl;
    return EXIT_FAILURE;
  }

  std::string outputFileName = argv[1];

  using PixelType = itk::SymmetricSecondRankTensor<double, 3>;
  using ImageType = itk::Image<PixelType, 3>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  // write a 10^3 image

  {
    // allocate an 10x10x10 image
    auto                image = ImageType::New();
    ImageType::SizeType imageSize;
    imageSize.Fill(10);
    image->SetRegions(imageSize);
    image->Allocate();

    unsigned int                        cnt = 0;
    itk::ImageRegionIterator<ImageType> i(image, image->GetLargestPossibleRegion());
    i.GoToBegin();
    while (!i.IsAtEnd())
    {
      // fill the image switching between these pixels
      switch (cnt * 3 % 5)
      {
        case 0:
          i.Set(itk::NumericTraits<PixelType>::ZeroValue());
          break;
        case 1:
          i.Set(itk::NumericTraits<PixelType>::OneValue());
          break;
        case 2:
          i.Set(itk::NumericTraits<PixelType>::OneValue());
          break;
      }
      ++cnt;
      ++i;
    }

    using IOType = itk::VTKImageIO;
    auto vtkIO = IOType::New();

    auto writer = WriterType::New();
    writer->SetImageIO(vtkIO);
    writer->SetInput(image);
    writer->SetFileName(outputFileName.c_str());
    writer->Update();
  }

  // check that a request to stream is not
  {
    using IOType = itk::VTKImageIO;
    auto vtkIO = IOType::New();

    auto reader = ReaderType::New();
    reader->SetFileName(outputFileName.c_str());
    reader->SetImageIO(vtkIO);

    using MonitorFilter = itk::PipelineMonitorImageFilter<ImageType>;
    auto monitor = MonitorFilter::New();
    monitor->SetInput(reader->GetOutput());
    constexpr unsigned int numberOfDataPieces = 10;


    using StreamingFilter = itk::StreamingImageFilter<ImageType, ImageType>;
    auto streamer = StreamingFilter::New();
    streamer->SetInput(monitor->GetOutput());
    streamer->SetNumberOfStreamDivisions(numberOfDataPieces);

    streamer->Update();

    bool passed = true;

    if (!monitor->VerifyAllInputCanNotStream())
    {
      passed = false;
    }

    if (!passed)
    {
      std::cout << monitor << std::endl;
      std::cout << "pipeline did not execute as expected!" << std::endl;
      return EXIT_FAILURE;
    }
  }

  // request to stream write but is should be denied
  {

    using IOType = itk::VTKImageIO;
    auto vtkIO = IOType::New();

    auto reader = ReaderType::New();
    reader->SetFileName(outputFileName.c_str());
    reader->SetImageIO(vtkIO);
    reader->UpdateLargestPossibleRegion();


    using MonitorFilter = itk::PipelineMonitorImageFilter<ImageType>;
    auto monitor = MonitorFilter::New();
    monitor->SetInput(reader->GetOutput());
    constexpr unsigned int numberOfDataPieces = 10;

    auto writer = WriterType::New();
    writer->SetImageIO(vtkIO);
    writer->SetInput(monitor->GetOutput());
    writer->SetNumberOfStreamDivisions(numberOfDataPieces);
    writer->SetFileName(outputFileName.c_str());

    writer->Update();

    bool passed = true;

    if (!monitor->VerifyAllInputCanNotStream())
    {
      passed = false;
    }

    if (!passed)
    {
      std::cout << monitor << std::endl;
      std::cout << "pipeline did not execute as expected!" << std::endl;
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
