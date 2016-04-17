/*=========================================================================
*
*  Copyright Insight Software Consortium
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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkGPUImage.h"
#include "itkGPUKernelManager.h"
#include "itkGPUContextManager.h"
#include "itkGPUImageToImageFilter.h"
#include "itkGPUNeighborhoodOperatorImageFilter.h"

#include "itkTimeProbe.h"
#include "itkGaussianOperator.h"

/**
 * Testing GPU Neighborhood Operator Image Filter
 */

template< unsigned int VImageDimension >
int runGPUNeighborhoodOperatorImageFilterTest(const std::string& inFile, const std::string& outFile)
{

  typedef float InputPixelType;
  typedef float OutputPixelType;

  typedef itk::GPUImage< InputPixelType,  VImageDimension >   InputImageType;
  typedef itk::GPUImage< OutputPixelType, VImageDimension >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  typename ReaderType::Pointer reader = ReaderType::New();
  typename WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( inFile );
  writer->SetFileName( outFile );

  typedef OutputPixelType    RealOutputPixelType;
  typedef itk::NumericTraits<RealOutputPixelType>::ValueType
                             RealOutputPixelValueType;

  typedef itk::NeighborhoodOperatorImageFilter< InputImageType, OutputImageType, RealOutputPixelValueType > NeighborhoodFilterType;
  typedef itk::GPUNeighborhoodOperatorImageFilter< InputImageType, OutputImageType, RealOutputPixelValueType > GPUNeighborhoodFilterType;

  // Create 1D Gaussian operator
  typedef itk::GaussianOperator< RealOutputPixelValueType, VImageDimension > OperatorType;

  OperatorType oper;
  oper.SetDirection(0);
  oper.SetVariance( 8.0 );
  oper.CreateDirectional();

  // test 1~8 threads for CPU
  for(int nThreads = 1; nThreads <= 8; nThreads++)
  {
    typename NeighborhoodFilterType::Pointer CPUFilter = NeighborhoodFilterType::New();

    itk::TimeProbe cputimer;
    cputimer.Start();

    CPUFilter->SetNumberOfThreads( nThreads );

    CPUFilter->SetInput( reader->GetOutput() );
    CPUFilter->SetOperator( oper );
    CPUFilter->Update();

    cputimer.Stop();

    std::cout << "CPU NeighborhoodFilter took " << cputimer.GetMean() << " seconds with "
              << CPUFilter->GetNumberOfThreads() << " threads.\n" << std::endl;

    // -------

    if( nThreads == 8 )
    {
      typename GPUNeighborhoodFilterType::Pointer GPUFilter = GPUNeighborhoodFilterType::New();

      itk::TimeProbe gputimer;
      gputimer.Start();

      GPUFilter->SetInput( reader->GetOutput() );
      GPUFilter->SetOperator( oper );
      GPUFilter->Update();

      GPUFilter->GetOutput()->UpdateBuffers(); // synchronization point (GPU->CPU memcpy)

      gputimer.Stop();
      std::cout << "GPU NeighborhoodFilter took " << gputimer.GetMean() << " seconds.\n" << std::endl;

      // ---------------
      // RMS Error check
      // ---------------

      double diff = 0;
      unsigned int nPix = 0;
      itk::ImageRegionIterator<OutputImageType> cit(CPUFilter->GetOutput(), CPUFilter->GetOutput()->GetLargestPossibleRegion());
      itk::ImageRegionIterator<OutputImageType> git(GPUFilter->GetOutput(), GPUFilter->GetOutput()->GetLargestPossibleRegion());

      for(cit.GoToBegin(), git.GoToBegin(); !cit.IsAtEnd(); ++cit, ++git)
      {
        double err = (double)(cit.Get()) - (double)(git.Get());
//         if(err > 0.1 || (double)(cit.Get()) < 0.1)   std::cout << "CPU : " << (double)(cit.Get()) << ", GPU : " << (double)(git.Get()) << std::endl;
        diff += err*err;
        nPix++;
      }

      writer->SetInput( GPUFilter->GetOutput() );
      writer->Update();

      if (nPix > 0)
      {
        double RMSError = sqrt( diff / (double)nPix );
        std::cout << "RMS Error : " << RMSError << std::endl;
        // the CPU filter operator has type double
        // but the double precision is not well-supported on most GPUs
        // and by most drivers at this time.  Therefore, the GPU filter
        // operator has type float
        // relax the RMS threshold here to allow for errors due to
        // differences in precision
        // NOTE:
        //   a threshold of 1.1e-5 worked on linux and Mac, but not Windows
        //   why?
        double RMSThreshold = 1.2e-5;
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

int itkGPUNeighborhoodOperatorImageFilterTest(int argc, char *argv[])
{
  if(!itk::IsGPUAvailable())
  {
    std::cerr << "OpenCL-enabled GPU is not present." << std::endl;
    return EXIT_FAILURE;
  }

  if( argc <  3 )
  {
    std::cerr << "Error: missing arguments" << std::endl;
    std::cerr << "inputfile outputfile [num_dimensions]" << std::endl;
    return EXIT_FAILURE;
  }

  std::string inFile( argv[1] );
  std::string outFile( argv[2] );

  unsigned int dim = 3;
  if( argc >= 4 )
  {
    dim = atoi( argv[3] );
  }

  if( dim == 2 )
  {
    return runGPUNeighborhoodOperatorImageFilterTest<2>(inFile, outFile);
  }
  else if( dim == 3 )
  {
    return runGPUNeighborhoodOperatorImageFilterTest<3>(inFile, outFile);
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dim << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
