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

/**
 * Test program for itkGPUBinaryThresholdImageFilter class
 *
 */

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkTimeProbe.h"

#include "itkGPUImage.h"
#include "itkGPUKernelManager.h"
#include "itkGPUContextManager.h"
#include "itkGPUImageToImageFilter.h"
#include "itkGPUBinaryThresholdImageFilter.h"

template< unsigned int VImageDimension >
int runGPUBinaryThresholdImageFilterTest(const std::string& inFile, const std::string& outFile)
{
  typedef   unsigned char  InputPixelType;
  typedef   unsigned char  OutputPixelType;

  typedef itk::GPUImage< InputPixelType,  VImageDimension >   InputImageType;
  typedef itk::GPUImage< OutputPixelType, VImageDimension >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  typename ReaderType::Pointer reader = ReaderType::New();
  typename WriterType::Pointer writer = WriterType::New();

  if(!itk::IsGPUAvailable())
  {
    std::cerr << "OpenCL-enabled GPU is not present." << std::endl;
    return EXIT_FAILURE;
  }

  reader->SetFileName( inFile );
  writer->SetFileName( outFile );

  typedef itk::BinaryThresholdImageFilter< InputImageType, OutputImageType > ThresholdFilterType;
  typedef itk::GPUBinaryThresholdImageFilter< InputImageType, OutputImageType > GPUThresholdFilterType;

  // threshold parameters
  const InputPixelType upperThreshold = 255;
  const InputPixelType lowerThreshold = 175;
  const OutputPixelType outsideValue = 0;
  const OutputPixelType insideValue  = 255;

  for(int nThreads = 1; nThreads <= 8; nThreads++)
  {
    typename ThresholdFilterType::Pointer CPUFilter = ThresholdFilterType::New();
    itk::TimeProbe cputimer;
    cputimer.Start();

    // build pipeline
    CPUFilter->SetNumberOfThreads( nThreads );

    CPUFilter->SetOutsideValue( outsideValue );
    CPUFilter->SetInsideValue(  insideValue  );
    CPUFilter->SetUpperThreshold( upperThreshold );
    CPUFilter->SetLowerThreshold( lowerThreshold );
    //CPUFilter->SetInPlace( true );
    CPUFilter->SetInput( reader->GetOutput() );
    CPUFilter->Update();

    cputimer.Stop();

    std::cout << "CPU binary threshold took " << cputimer.GetMean() << " seconds with "
              << CPUFilter->GetNumberOfThreads() << " threads.\n" << std::endl;

    if( nThreads == 8 )
    {
      typename GPUThresholdFilterType::Pointer GPUFilter = GPUThresholdFilterType::New();

      itk::TimeProbe gputimer;
      gputimer.Start();

      GPUFilter->SetOutsideValue( outsideValue );
      GPUFilter->SetInsideValue(  insideValue  );
      GPUFilter->SetUpperThreshold( upperThreshold );
      GPUFilter->SetLowerThreshold( lowerThreshold );
      //GPUFilter->SetInPlace( true );
      GPUFilter->SetInput( reader->GetOutput() );
      GPUFilter->Update();

      GPUFilter->GetOutput()->UpdateBuffers(); // synchronization point

      gputimer.Stop();

      std::cout << "GPU binary threshold took " << gputimer.GetMean() << " seconds.\n" << std::endl;

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
        diff += err*err;
        nPix++;
      }
      if (nPix > 0)
      {
        double RMSError = sqrt( diff / (double)nPix );
        std::cout << "RMS Error : " << RMSError << std::endl;
        double RMSThreshold = 0;
        writer->SetInput( GPUFilter->GetOutput() );

        // execute pipeline filter and write output
        writer->Update();

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

int itkGPUBinaryThresholdImageFilterTest(int argc, char *argv[])
{

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
    return runGPUBinaryThresholdImageFilterTest<2>(inFile, outFile);
  }
  else if( dim == 3 )
  {
    return runGPUBinaryThresholdImageFilterTest<3>(inFile, outFile);
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dim << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
