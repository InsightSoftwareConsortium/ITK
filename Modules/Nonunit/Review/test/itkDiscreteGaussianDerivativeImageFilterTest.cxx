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
#include "itkRescaleIntensityImageFilter.h"
#include "itkDiscreteGaussianDerivativeImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


/** Calculate the Gaussian derivatives at non-zero points of a Gaussian
 * input image. For derivative calculation the class
 * itkDiscreteGaussianDerivativeImageFilter is used.
 * This example operates on 2D images.
*/
int itkDiscreteGaussianDerivativeImageFilterTest( int argc, char* argv[] )
{
  if( argc < 6 )
    {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0]
      << "inputFileName"
        " outputFileName"
        " orderX"
        " orderY"
        " sigma"
        " [maximumError]"
        " [maximumKernelWidth]" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef float           PixelType;
  typedef unsigned short  OutputPixelType;

  typedef itk::Image< PixelType, Dimension >        ImageType;
  typedef itk::Image< OutputPixelType, Dimension >  OutputImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );


  typedef itk::DiscreteGaussianDerivativeImageFilter< ImageType, ImageType >
    DerivativeFilterType;
  DerivativeFilterType::Pointer derivativeFilter = DerivativeFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( derivativeFilter,
    DiscreteGaussianDerivativeImageFilter, ImageToImageFilter );


  itk::SimpleFilterWatcher watcher( derivativeFilter,
    "DiscreteGaussianDerivativeImageFilter" );

  derivativeFilter->SetInput( reader->GetOutput() );

  // Now proceed to apply the Gaussian derivative filter in both directions

  DerivativeFilterType::OrderArrayType order;
  order[0] = atoi( argv[3] );
  order[1] = atoi( argv[4] );

  derivativeFilter->SetOrder( order );
  TEST_SET_GET_VALUE( order, derivativeFilter->GetOrder() );

  double sigma = atof( argv[5] );

  DerivativeFilterType::ArrayType::ValueType maxErrorVal = 0.001;
  int maxKernelWidth = 100;

  if( argc > 7 )
    {
    maxErrorVal = static_cast< DerivativeFilterType::ArrayType::ValueType >(
      atof( argv[6] ) );
    }
  else if( argc > 8 )
    {
    maxKernelWidth = atoi( argv[7] );
    }

  DerivativeFilterType::ArrayType variance;
  variance.Fill( sigma * sigma );

  derivativeFilter->SetVariance( variance );
  TEST_SET_GET_VALUE( variance, derivativeFilter->GetVariance() );

  DerivativeFilterType::ArrayType maxError;
  maxError.Fill( maxErrorVal );

  derivativeFilter->SetMaximumError( maxErrorVal );
  TEST_SET_GET_VALUE( maxError, derivativeFilter->GetMaximumError() );

  derivativeFilter->SetMaximumKernelWidth( maxKernelWidth );
  TEST_SET_GET_VALUE( maxKernelWidth, derivativeFilter->GetMaximumKernelWidth() );

  bool useImageSpacing = true;
  TEST_SET_GET_BOOLEAN( derivativeFilter, UseImageSpacing, useImageSpacing );

  bool normalizeAcrossScale = false;
  TEST_SET_GET_BOOLEAN( derivativeFilter, NormalizeAcrossScale, normalizeAcrossScale );

  unsigned int internalNumberOfStreamDivisions =
    DerivativeFilterType::InputImageType::GetImageDimension() *
    DerivativeFilterType::InputImageType::GetImageDimension();
  derivativeFilter->SetInternalNumberOfStreamDivisions(
    internalNumberOfStreamDivisions );
  TEST_SET_GET_VALUE( internalNumberOfStreamDivisions,
    derivativeFilter->GetInternalNumberOfStreamDivisions() );


  typedef itk::RescaleIntensityImageFilter< ImageType, OutputImageType >
    RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum( itk::NumericTraits<OutputPixelType>::min() );
  rescaler->SetOutputMaximum( itk::NumericTraits<OutputPixelType>::max() );
  rescaler->SetInput( derivativeFilter->GetOutput() );


  // Write the output image
  typedef itk::ImageFileWriter< OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( rescaler->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
