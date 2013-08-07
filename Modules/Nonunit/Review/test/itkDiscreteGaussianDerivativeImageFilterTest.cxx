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
// Author : Ivan Macia (imacia@vicomtech.org)
// Date   : 02/01/2009
// VICOMTech, Spain
// http://www.vicomtech.org
// Description : calculates the gaussian derivatives at non-zero
// points of a gaussian input image. For derivative calculation the class
// itkDiscreteGaussianDerivativeImageFilter is used. This example operates on 2D images.


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkDiscreteGaussianDerivativeImageFilter.h"
#include "itkSimpleFilterWatcher.h"


int itkDiscreteGaussianDerivativeImageFilterTest( int argc, char* argv[] )
{
  // Verify the number of parameters in the command line
  if( argc < 6 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputFileName outputFileName orderX orderY sigma (maximum_error) (maximum_kernel_width)" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;
  //const unsigned int Dimension = 3;
  typedef   float             PixelType;
  typedef   unsigned short    OutputPixelType;

  typedef   itk::Image<PixelType, Dimension>          ImageType;
  typedef   itk::Image<OutputPixelType, Dimension>    OutputImageType;

  typedef   itk::ImageFileReader< ImageType >     ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  typedef   itk::DiscreteGaussianDerivativeImageFilter< ImageType, ImageType >   DerivativeFilterType;
  DerivativeFilterType::Pointer derivativeFilter = DerivativeFilterType::New();
  derivativeFilter->SetInput( reader->GetOutput() );

  // Now proceed to apply the gaussian derivative filter in both directions

  double maxError = 0.001;
  unsigned int maxKernelWidth = 100;

  if( argc >= 7 )
    {
    maxError = atof( argv[6] );
    }
  else if( argc >= 8 )
    {
    maxError = atof( argv[6] );
    maxKernelWidth = atoi( argv[7] );
    }

  DerivativeFilterType::OrderArrayType order;
  order[0] = atoi( argv[3] );
  order[1] = atoi( argv[4] );

  double variance = atof( argv[5] );
  variance *= variance;

  derivativeFilter->SetMaximumError( maxError );
  derivativeFilter->SetMaximumKernelWidth( maxKernelWidth );
  derivativeFilter->SetVariance( variance );
  derivativeFilter->SetOrder( order );
  itk::SimpleFilterWatcher watcher(derivativeFilter, "derivativeFilter");

  typedef   itk::RescaleIntensityImageFilter< ImageType, OutputImageType >
    RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum( itk::NumericTraits<OutputPixelType>::min() );
  rescaler->SetOutputMaximum( itk::NumericTraits<OutputPixelType>::max() );
  rescaler->SetInput( derivativeFilter->GetOutput() );

  typedef   itk::ImageFileWriter< OutputImageType >   WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( rescaler->GetOutput() );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
