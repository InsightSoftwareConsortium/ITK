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
 * Test program for GPUGradientAnisotropicDiffusionImageFilter class
 */
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkTimeProbe.h"
#include "itkImageRegionIterator.h"

#include "itkOpenCLUtil.h"
#include "itkGPUImage.h"
#include "itkGPUKernelManager.h"
#include "itkGPUContextManager.h"
#include "itkGPUImageToImageFilter.h"
#include "itkGPUGradientAnisotropicDiffusionImageFilter.h"

template <unsigned int VImageDimension>
int
runGPUGradientAnisotropicDiffusionImageFilterTest(const std::string & inFile, const std::string & outFile)
{
  using InputPixelType = float;
  using OutputPixelType = float;

  using InputImageType = itk::GPUImage<InputPixelType, VImageDimension>;
  using OutputImageType = itk::GPUImage<OutputPixelType, VImageDimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  reader->SetFileName(inFile);
  writer->SetFileName(outFile);

  // Create CPU/GPU anistorpic diffusion filter
  using CPUAnisoDiffFilterType = itk::GradientAnisotropicDiffusionImageFilter<InputImageType, OutputImageType>;
  using GPUAnisoDiffFilterType = itk::GPUGradientAnisotropicDiffusionImageFilter<InputImageType, OutputImageType>;

  auto CPUFilter = CPUAnisoDiffFilterType::New();
  auto GPUFilter = GPUAnisoDiffFilterType::New();

  reader->Update();

  // -------

  // test 1~8 threads for CPU
  std::vector<int> nThreadVec;
  nThreadVec.push_back(1);
  nThreadVec.push_back(2);
  nThreadVec.push_back(4);
  nThreadVec.push_back(8);
  nThreadVec.push_back(128);

  for (unsigned int idx = 0; idx < nThreadVec.size(); ++idx)
  {
    int            numberOfWorkUnits = nThreadVec[idx];
    itk::TimeProbe cputimer;
    cputimer.Start();

    CPUFilter->SetNumberOfWorkUnits(numberOfWorkUnits);

    CPUFilter->SetInput(reader->GetOutput());
    CPUFilter->SetNumberOfIterations(10);
    CPUFilter->SetTimeStep(0.0625); // 125 );
    CPUFilter->SetConductanceParameter(3.0);
    CPUFilter->UseImageSpacingOn();
    CPUFilter->Update();

    cputimer.Stop();

    std::cout << "CPU Anisotropic diffusion took " << cputimer.GetMean() << " seconds with "
              << CPUFilter->GetNumberOfWorkUnits() << " work units.\n"
              << std::endl;

    // -------

    if (idx == nThreadVec.size() - 1)
    {
      itk::TimeProbe gputimer;
      gputimer.Start();

      GPUFilter->SetInput(reader->GetOutput());
      GPUFilter->SetNumberOfIterations(10);
      GPUFilter->SetTimeStep(0.0625); // 125 );
      GPUFilter->SetConductanceParameter(3.0);
      GPUFilter->UseImageSpacingOn();
      try
      {
        GPUFilter->Update();
      }
      catch (const itk::ExceptionObject & excp)
      {
        std::cout << "Caught exception during GPUFilter->Update() " << excp << std::endl;
        return EXIT_FAILURE;
      }

      try
      {
        GPUFilter->GetOutput()->UpdateBuffers(); // synchronization point
      }
      catch (const itk::ExceptionObject & excp)
      {
        std::cout << "Caught exception during GPUFilter->GetOutput()->UpdateBuffers() " << excp << std::endl;
        return EXIT_FAILURE;
      }


      gputimer.Stop();
      std::cout << "GPU Anisotropic diffusion took " << gputimer.GetMean() << " seconds.\n" << std::endl;

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
        diff += err * err;
        nPix++;
      }
      writer->SetInput(GPUFilter->GetOutput()); // copy GPU->CPU implicilty

      // execute pipeline filter and write output
      writer->Update();

      if (nPix > 0)
      {
        double RMSError = sqrt(diff / static_cast<double>(nPix));
        std::cout << "RMS Error : " << RMSError << std::endl;
        double RMSThreshold = 10.0;
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

  GPUFilter = nullptr;                                      // explicit GPU object destruction test
  itk::GPUContextManager::GetInstance()->DestroyInstance(); // GPUContextManager singleton destruction test
  return EXIT_SUCCESS;
}

int
itkGPUGradientAnisotropicDiffusionImageFilterTest(int argc, char * argv[])
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
    return runGPUGradientAnisotropicDiffusionImageFilterTest<2>(inFile, outFile);
  }
  else if (dim == 3)
  {
    return runGPUGradientAnisotropicDiffusionImageFilterTest<3>(inFile, outFile);
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dim << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
