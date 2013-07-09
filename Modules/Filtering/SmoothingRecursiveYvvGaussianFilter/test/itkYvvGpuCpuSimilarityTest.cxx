#include <iostream>
#include <fstream>

#include "itkCastImageFilter.h"
#include "itkSmoothingRecursiveYvvGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTimeProbe.h"

#include "itkGPUImage.h"
#include "itkGPUKernelManager.h"
#include "itkGPUContextManager.h"
#include "itkGPUSmoothingRecursiveYvvGaussianImageFilter.h"


#ifdef WITH_DOUBLE
typedef double PixelType;
#else
typedef float PixelType;
#endif

template <typename ImageType>
int
runYvvGpuCpuSimilarityTest(const std::string & inFile, float mySigma)
{
  typedef PixelType InputPixelType;
  typedef PixelType OutputPixelType;

  typedef itk::GPUImage<InputPixelType, ImageType::ImageDimension>     InputImageType;
  typedef itk::GPUImage<OutputPixelType, ImageType::ImageDimension>    OutputImageType;
  typedef itk::Image<unsigned char, ImageType::ImageDimension>         UnsignedCharImageType;
  typedef itk::CastImageFilter<OutputImageType, UnsignedCharImageType> CastFilterType;

  typedef itk::SmoothingRecursiveYvvGaussianImageFilter<InputImageType, OutputImageType>    CPUYvvFilterType;
  typedef itk::GPUSmoothingRecursiveYvvGaussianImageFilter<InputImageType, OutputImageType> GPUYvvFilterType;

  typedef itk::ImageFileReader<InputImageType>        ReaderType;
  typedef itk::ImageFileWriter<UnsignedCharImageType> WriterType;

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

  for (float sigma = mySigma; sigma <= 32; sigma *= 2)
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
              << CPUFilter->GetNumberOfThreads() << " threads." << std::endl;

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

    std::cout << "GPU Recursive YVV Gaussian Filter took " << gputimer.GetMean() << " seconds.\n";

    castFilter->SetInput(GPUFilter->GetOutput());
    writer->SetInput(castFilter->GetOutput());
    writer->Update();

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
      double err = (double)(cit.Get()) - (double)(git.Get());
      //         if(err > 0.1 || (double)cit.Get() < 0.1) std::cout << "CPU : " << (double)(cit.Get()) << ", GPU : " <<
      //         (double)(git.Get()) << std::endl;
      diff += err * err;
      nPix++;
    }


    if (nPix > 0)
    {
      double RMSError = sqrt(diff / (double)nPix);
      std::cout << "RMS Error with sigma = " << sigma << " : " << RMSError << std::endl;
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
      if (vnl_math_isnan(RMSError))
      {
        std::cout << "RMS Error with sigma = " << sigma << " is NaN! nPix: " << nPix << std::endl;
        return EXIT_FAILURE;
      }
      if (RMSError > RMSThreshold)
      {
        std::cout << "RMS Error with sigma = " << sigma << " exceeds threshold (" << RMSThreshold << ")" << std::endl;
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
  unsigned int dim = atoi(argv[2]);

  float sigma = 0.5;
  if (argc > 3 && atoi(argv[3]) >= 0.5)
  {
    sigma = atof(argv[3]);
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
