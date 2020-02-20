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
#include <iostream>

#include "itkCastImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkSmoothingRecursiveYvvGaussianImageFilter.h"
#include "itkTimeProbe.h"

#include "itkGPUContextManager.h"
#include "itkGPUImage.h"
#include "itkGPUKernelManager.h"
#include "itkGPUSmoothingRecursiveYvvGaussianImageFilter.h"


// The double precision is not well-supported on most GPUs
// and by many drivers at this time.
// Relax the RMS threshold to allow for errors due to
// differences in precision when the GPU does not support
// double-precision.
// TODO: Test on Mac/Windows.

#ifdef WITH_DOUBLE
using PixelType = double;
#  define NRMSTH 5e-07
#else
using PixelType = float;
#  define NRMSTH 5e-04
#endif

template <typename ImageType>
int
runYvvGpuCpuSimilarityTest(const std::string & inFile, float mySigma)
{
  using InputPixelType = PixelType;
  using OutputPixelType = PixelType;

  using InputImageType = itk::GPUImage<InputPixelType, ImageType::ImageDimension>;
  using OutputImageType = itk::GPUImage<OutputPixelType, ImageType::ImageDimension>;
  using UnsignedCharImageType = itk::Image<unsigned char, ImageType::ImageDimension>;
  using CastFilterType = itk::CastImageFilter<OutputImageType, UnsignedCharImageType>;

  using CPUYvvFilterType = itk::SmoothingRecursiveYvvGaussianImageFilter<InputImageType, OutputImageType>;
  using GPUYvvFilterType = itk::GPUSmoothingRecursiveYvvGaussianImageFilter<InputImageType, OutputImageType>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<UnsignedCharImageType>;

  typename ReaderType::Pointer     reader = ReaderType::New();
  typename WriterType::Pointer     writer = WriterType::New();
  typename CastFilterType::Pointer castFilter = CastFilterType::New();

  typename WriterType::Pointer     writerCPU = WriterType::New();
  typename CastFilterType::Pointer castFilterCPU = CastFilterType::New();

  std::string outFile = "gpuYvvCompOutput";
  std::string outFileCPU = "cpuYvvCompOutput";

  if (ImageType::ImageDimension == 2)
  {
    outFile += ".jpg";
    outFileCPU += ".jpg";
  }
  else
  {
    outFile += ".tif";
    outFileCPU += ".tif";
  }

  reader->SetFileName(inFile);
  writer->SetFileName(outFile);
  writerCPU->SetFileName(outFileCPU);

  typename InputImageType::Pointer src = InputImageType::New();
  src = reader->GetOutput();
  reader->Update();

  bool someDiffFlag = false;

  std::cout << "\nUsing  " << inFile << std::endl;

  for (float sigma = 0.5; sigma <= mySigma; sigma *= 2)
  {
    typename CPUYvvFilterType::Pointer CPUFilter = CPUYvvFilterType::New();

    itk::TimeProbe cputimer;

    cputimer.Start();
    CPUFilter->SetInput(src);
    CPUFilter->SetSigma(sigma);
    CPUFilter->Update();
    cputimer.Stop();


    std::cout << "\nSize: " << src->GetLargestPossibleRegion().GetSize() << std::endl;
    std::cout << "CPU Recursive YVV Gaussian Filter took " << cputimer.GetMean() << " seconds with "
              << CPUFilter->GetNumberOfWorkUnits() << " threads." << std::endl;

    castFilterCPU->SetInput(CPUFilter->GetOutput());
    writerCPU->SetInput(castFilterCPU->GetOutput());
    writerCPU->Update();

    typename GPUYvvFilterType::Pointer GPUFilter = GPUYvvFilterType::New();

    itk::TimeProbe gputimer;

    gputimer.Start();
    GPUFilter->SetInput(reader->GetOutput());
    GPUFilter->SetSigma(sigma);
    GPUFilter->Update();
    GPUFilter->GetOutput()->UpdateBuffers(); // synchronization point (GPU->CPU memcpy)
    gputimer.Stop();

    using ImageCalculatorFilterType = itk::MinimumMaximumImageCalculator<ImageType>;

    castFilter->SetInput(GPUFilter->GetOutput());
    writer->SetInput(castFilter->GetOutput());
    writer->Update();

    // ---------------
    // RMS Error check
    // ---------------

    using ImageCalculatorFilterType = itk::MinimumMaximumImageCalculator<ImageType>;

    typename ImageCalculatorFilterType::Pointer imageCalculatorFilter = ImageCalculatorFilterType::New();
    imageCalculatorFilter->SetImage(CPUFilter->GetOutput());
    imageCalculatorFilter->Compute();

    double                                    maxPx = imageCalculatorFilter->GetMaximum();
    double                                    minPx = imageCalculatorFilter->GetMinimum();
    double                                    diff = 0;
    unsigned int                              nPix = 0;
    itk::ImageRegionIterator<OutputImageType> cit(CPUFilter->GetOutput(),
                                                  CPUFilter->GetOutput()->GetLargestPossibleRegion());
    itk::ImageRegionIterator<OutputImageType> git(GPUFilter->GetOutput(),
                                                  GPUFilter->GetOutput()->GetLargestPossibleRegion());

    for (cit.GoToBegin(), git.GoToBegin(); !cit.IsAtEnd(); ++cit, ++git)
    {
      double err = (double)(cit.Get()) - (double)(git.Get());
      //         if(err > 0.1 || (double)cit.Get() < 0.1) std::cout << "CPU : " << (double)(cit.Get()) << ", GPU : "
      //         << (double)(git.Get()) << std::endl;
      diff += err * err;
      nPix++;
    }


    if (nPix > 0)
    {
      double NormRMSError = sqrt(diff / (double)nPix) / (maxPx - minPx); //
      std::cout << "Normalised RMS Error with sigma = " << sigma << " : " << NormRMSError << std::endl;

      if (std::isnan(NormRMSError))
      {
        std::cout << "Normalised RMS Error with sigma = " << sigma << " is NaN! nPix: " << nPix << std::endl;
        return EXIT_FAILURE;
      }
      if (NormRMSError > NRMSTH)
      {
        std::cout << "Normalised RMS Error with sigma = " << sigma << " exceeds threshold (" << NRMSTH << ")"
                  << std::endl;
        someDiffFlag = true;
      }
    }
    else
    {
      std::cout << "No pixels in output!" << std::endl;
      return EXIT_FAILURE;
    }
  }

  if (someDiffFlag)
    return EXIT_FAILURE;

  return EXIT_SUCCESS;
}


int
itkYvvGpuCpuSimilarityTest(int argc, char * argv[])
{
  if (!itk::IsGPUAvailable())
  {
    std::cerr << "OpenCL-enabled GPU is not present." << std::endl;
    return EXIT_FAILURE;
  }

  if (argc < 3)
  {
    std::cerr << argc << ": Error: missing arguments: " << argv[1] << std::endl;
    std::cerr << "[Usage] " << argv[0] << " image_file ndimension [sigma]" << std::endl;
    return EXIT_FAILURE;
  }

  std::string  inputFilename(argv[1]);
  unsigned int dim = std::stoi(argv[2]);

  float sigma = 0.5;
  if (argc > 3 && std::stoi(argv[3]) >= 0.5)
  {
    sigma = std::stod(argv[3]);
  }

  if (dim == 2)
  {
    return runYvvGpuCpuSimilarityTest<itk::Image<PixelType, 2>>(inputFilename, sigma);
  }
  else if (dim == 3)
  {
    return runYvvGpuCpuSimilarityTest<itk::Image<PixelType, 3>>(inputFilename, sigma);
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dim << " selected." << std::endl;
  }
  return EXIT_FAILURE;
}
