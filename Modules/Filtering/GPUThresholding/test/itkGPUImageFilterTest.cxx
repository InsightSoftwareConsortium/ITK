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
 * Test program for itkGPUImageToImageFilter class
 *
 * This program creates a GPU Mean filter and a CPU threshold filter using
 * object factory framework and test pipelining of GPU and CPU filters.
 */
//#include "pathToOpenCLSourceCode.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMeanImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"

#include "itkGPUImage.h"
#include "itkGPUKernelManager.h"
#include "itkGPUContextManager.h"
#include "itkGPUImageToImageFilter.h"
#include "itkGPUMeanImageFilter.h"

#include <string>

template< unsigned int VImageDimension >
int runGPUImageFilterTest(const std::string& inFile, const std::string& outFile)
{
  typedef   unsigned char  InputPixelType;
  typedef   unsigned char  OutputPixelType;

  typedef itk::Image< InputPixelType,  VImageDimension >   InputImageType;
  typedef itk::Image< OutputPixelType, VImageDimension >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  typename ReaderType::Pointer reader = ReaderType::New();
  typename WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( inFile );
  writer->SetFileName( outFile );

  //
  // Note: We use regular itk filter type here but factory will automatically create
  //       GPU filter for Median filter and CPU filter for threshold filter.
  //
  typedef itk::MeanImageFilter< InputImageType, OutputImageType > MeanFilterType;
  typedef itk::BinaryThresholdImageFilter< InputImageType, OutputImageType > ThresholdFilterType;

  typename MeanFilterType::Pointer filter1 = MeanFilterType::New();
  typename MeanFilterType::Pointer filter2 = MeanFilterType::New();
  typename ThresholdFilterType::Pointer filter3 = ThresholdFilterType::New();

  // Mean filter kernel radius
  typename InputImageType::SizeType indexRadius;
  indexRadius[0] = 2; // radius along x
  indexRadius[1] = 2; // radius along y
  if( VImageDimension > 2 )
  {
    indexRadius[2] = 2; // radius along z
  }

  // threshold parameters
  const InputPixelType upperThreshold = 255;
  const InputPixelType lowerThreshold = 175;
  const OutputPixelType outsideValue = 0;
  const OutputPixelType insideValue  = 255;

  filter1->SetRadius( indexRadius );
  filter2->SetRadius( indexRadius );
  filter3->SetOutsideValue( outsideValue );
  filter3->SetInsideValue(  insideValue  );
  filter3->SetUpperThreshold( upperThreshold );
  filter3->SetLowerThreshold( lowerThreshold );

  // build pipeline
  filter1->SetInput( reader->GetOutput() ); // copy CPU->GPU implicilty
  filter2->SetInput( filter1->GetOutput() );
  filter3->SetInput( filter2->GetOutput() );
  writer->SetInput( filter3->GetOutput() ); // copy GPU->CPU implicilty

  // execute pipeline filter and write output
  writer->Update();

  return EXIT_SUCCESS;
}

int itkGPUImageFilterTest(int argc, char *argv[])
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
    return runGPUImageFilterTest<2>(inFile, outFile);
  }
  else if( dim == 3 )
  {
    return runGPUImageFilterTest<3>(inFile, outFile);
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dim << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
