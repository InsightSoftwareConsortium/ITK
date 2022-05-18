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

/**
 * Test program for itkGPUMeanImageFilter class
 *
 * This program creates a GPU Mean filter test pipelining.
 */

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMeanImageFilter.h"

#include "itkGPUImage.h"
#include "itkGPUKernelManager.h"
#include "itkGPUContextManager.h"
#include "itkGPUImageToImageFilter.h"
#include "itkGPUMeanImageFilter.h"

#include "itkTimeProbe.h"

/**
 * Testing GPU Mean Image Filter
 */
template <unsigned int VImageDimension>
int
runGPUMeanImageFilterTest(const std::string & inFile, const std::string & outFile)
{
  using InputPixelType = unsigned char;
  using OutputPixelType = unsigned char;

  using InputImageType = itk::GPUImage<InputPixelType, VImageDimension>;
  using OutputImageType = itk::GPUImage<OutputPixelType, VImageDimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  reader->SetFileName(inFile);
  writer->SetFileName(outFile);

  //
  // Note: We use regular itk filter type here but factory will automatically create
  //       GPU filter for Median filter and CPU filter for threshold filter.
  //
  using MeanFilterType = itk::MeanImageFilter<InputImageType, OutputImageType>;
  using GPUMeanFilterType = itk::GPUMeanImageFilter<InputImageType, OutputImageType>;

  // Mean filter kernel radius
  typename InputImageType::SizeType indexRadius;
  indexRadius[0] = 2; // radius along x
  indexRadius[1] = 2; // radius along y
  if (VImageDimension > 2)
  {
    indexRadius[2] = 2; // radius along z
  }

  // test 1~8 threads for CPU
  for (int numberOfWorkUnits = 1; numberOfWorkUnits <= 8; ++numberOfWorkUnits)
  {
    auto CPUFilter = MeanFilterType::New();

    itk::TimeProbe cputimer;
    cputimer.Start();

    CPUFilter->SetNumberOfWorkUnits(numberOfWorkUnits);

    CPUFilter->SetInput(reader->GetOutput());
    CPUFilter->SetRadius(indexRadius);
    CPUFilter->Update();

    cputimer.Stop();

    std::cout << "CPU mean filter took " << cputimer.GetMean() << " seconds with " << CPUFilter->GetNumberOfWorkUnits()
              << " work units.\n"
              << std::endl;

    // -------

    if (numberOfWorkUnits == 8)
    {
      auto GPUFilter = GPUMeanFilterType::New();

      itk::TimeProbe gputimer;
      gputimer.Start();

      GPUFilter->SetInput(reader->GetOutput());
      GPUFilter->SetRadius(indexRadius);
      GPUFilter->Update();
      GPUFilter->GetOutput()->UpdateBuffers(); // synchronization point (GPU->CPU memcpy)

      gputimer.Stop();
      std::cout << "GPU mean filter took " << gputimer.GetMean() << " seconds.\n" << std::endl;

      writer->SetInput(GPUFilter->GetOutput());
      writer->Update();
    }
  }

  return EXIT_SUCCESS;
}

int
itkGPUMeanImageFilterTest(int argc, char * argv[])
{
  if (!itk::IsGPUAvailable())
  {
    std::cerr << "OpenCL-enabled GPU is not present." << std::endl;
    return EXIT_FAILURE;
  }

  if (argc < 3)
  {
    std::cerr << "Error: missing arguments" << std::endl;
    std::cerr << "inputfile outputfile [num_dimensions]" << std::endl;
    return EXIT_FAILURE;
  }

  std::string inFile(argv[1]);
  std::string outFile(argv[2]);

  unsigned int dim = 3;
  if (argc >= 4)
  {
    dim = std::stoi(argv[3]);
  }

  if (dim == 2)
  {
    return runGPUMeanImageFilterTest<2>(inFile, outFile);
  }
  else if (dim == 3)
  {
    return runGPUMeanImageFilterTest<3>(inFile, outFile);
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dim << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
