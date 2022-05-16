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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkGPUImage.h"
#include "itkGPUKernelManager.h"
#include "itkGPUContextManager.h"
#include "itkGPUImageToImageFilter.h"
#include "itkGPUNeighborhoodOperatorImageFilter.h"

#include "itkTimeProbe.h"
#include "itkGaussianOperator.h"

#include "itkDiscreteGaussianImageFilter.h"
#include "itkGPUDiscreteGaussianImageFilter.h"

/**
 * Testing GPU Discrete Gaussian Image Filter
 */

template <unsigned int VImageDimension>
int
runGPUDiscreteGaussianImageFilterTest(const std::string & inFile, const std::string & outFile)
{

  using InputPixelType = float;
  using OutputPixelType = float;

  using InputImageType = itk::GPUImage<InputPixelType, VImageDimension>;
  using OutputImageType = itk::GPUImage<OutputPixelType, VImageDimension>;

  using CPUFilterType = itk::DiscreteGaussianImageFilter<InputImageType, OutputImageType>;
  using GPUFilterType = itk::GPUDiscreteGaussianImageFilter<InputImageType, OutputImageType>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  reader->SetFileName(inFile);
  writer->SetFileName(outFile);

  float variance = 4.0;

  // test 1~8 threads for CPU
  for (int numberOfWorkUnits = 1; numberOfWorkUnits <= 8; ++numberOfWorkUnits)
  {
    auto CPUFilter = CPUFilterType::New();

    itk::TimeProbe cputimer;
    cputimer.Start();

    CPUFilter->SetNumberOfWorkUnits(numberOfWorkUnits);

    CPUFilter->SetInput(reader->GetOutput());
    CPUFilter->SetVariance(variance);
    CPUFilter->Update();

    cputimer.Stop();

    std::cout << "CPU Gaussian Filter took " << cputimer.GetMean() << " seconds with "
              << CPUFilter->GetNumberOfWorkUnits() << " work units.\n"
              << std::endl;

    // -------

    if (numberOfWorkUnits == 8)
    {
      auto GPUFilter = GPUFilterType::New();

      itk::TimeProbe gputimer;
      gputimer.Start();

      GPUFilter->SetInput(reader->GetOutput());
      GPUFilter->SetVariance(variance);
      GPUFilter->Update();

      GPUFilter->GetOutput()->UpdateBuffers(); // synchronization point (GPU->CPU memcpy)

      gputimer.Stop();
      std::cout << "GPU Gaussian Filter took " << gputimer.GetMean() << " seconds.\n" << std::endl;

      // ---------------
      // RMS Error check
      // ---------------

      double                                    diff = 0;
      unsigned int                              nPix = 0;
      itk::ImageRegionIterator<OutputImageType> cit(CPUFilter->GetOutput(),
                                                    CPUFilter->GetOutput()->GetLargestPossibleRegion());
      itk::ImageRegionIterator<OutputImageType> git(GPUFilter->GetOutput(),
                                                    GPUFilter->GetOutput()->GetLargestPossibleRegion());

      for (cit.GoToBegin(), git.GoToBegin(); !cit.IsAtEnd(); ++cit, ++git)
      {
        double err = static_cast<double>(cit.Get()) - static_cast<double>(git.Get());
        //         if(err > 0.1 || static_cast<double>(cit.Get()) < 0.1) std::cout << "CPU : " <<
        //         static_cast<double>(cit.Get()) <<
        //         ", GPU : "
        //         << static_cast<double>(git.Get()) << std::endl;
        diff += err * err;
        nPix++;
      }

      writer->SetInput(GPUFilter->GetOutput());
      writer->Update();

      if (nPix > 0)
      {
        double RMSError = sqrt(diff / static_cast<double>(nPix));
        std::cout << "RMS Error : " << RMSError << std::endl;
        // the CPU filter operator has type double
        // but the double precision is not well-supported on most GPUs
        // and by most drivers at this time.  Therefore, the GPU filter
        // operator has type float
        // relax the RMS threshold here to allow for errors due to
        // differences in precision
        // NOTE:
        //   a threshold of 1.2e-5 worked on linux and Mac, but not Windows
        //   why?
        double RMSThreshold = 1.7e-5;
        if (itk::Math::isnan(RMSError))
        {
          std::cout << "RMS Error is NaN! nPix: " << nPix << std::endl;
          return EXIT_FAILURE;
        }
        if (RMSError > RMSThreshold)
        {
          std::cout << "RMS Error exceeds threshold (" << RMSThreshold << ")" << std::endl;
          return EXIT_FAILURE;
        }
      }
      else
      {
        std::cout << "No pixels in output!" << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  return EXIT_SUCCESS;
}

int
itkGPUDiscreteGaussianImageFilterTest(int argc, char * argv[])
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
    return runGPUDiscreteGaussianImageFilterTest<2>(inFile, outFile);
  }
  else if (dim == 3)
  {
    return runGPUDiscreteGaussianImageFilterTest<3>(inFile, outFile);
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dim << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
