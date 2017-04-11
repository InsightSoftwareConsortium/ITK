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
#include "itkDiscreteGradientMagnitudeGaussianImageFunction.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkTestingMacros.h"


template < int VDimension >
int itkDiscreteGradientMagnitudeGaussianImageFunctionTestND( int argc, char* argv[] )
{
  const unsigned int Dimension = VDimension;

  typedef float                               PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  // Read input image
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );

  ImageType *inputImage = reader->GetOutput();


  // Create the itk::DiscreteGradientMagnitudeGaussianImageFunction
  typedef itk::DiscreteGradientMagnitudeGaussianImageFunction< ImageType, PixelType >
    DiscreteGradientMagnitudeGaussianFunctionType;
  typename DiscreteGradientMagnitudeGaussianFunctionType::Pointer function =
    DiscreteGradientMagnitudeGaussianFunctionType::New();

  function->SetInputImage( inputImage );


  // Set up operator parameters
  double sigma = atof( argv[3] );

  double maxError = 0.001;
  unsigned int maxKernelWidth = 100;
  typename DiscreteGradientMagnitudeGaussianFunctionType::InterpolationModeType interpolationMode =
    DiscreteGradientMagnitudeGaussianFunctionType::NearestNeighbourInterpolation;
  bool useImageSpacing = true;

  if( argc > 4 )
    {
    maxError = atof( argv[4] );
    }
  if( argc > 5 )
    {
    maxKernelWidth = atoi( argv[5] );
    }
  if( argc > 6 )
    {
    interpolationMode =
      static_cast< typename DiscreteGradientMagnitudeGaussianFunctionType::InterpolationModeType >(
      atoi( argv[6] ) );
    }
  if( argc > 7 )
    {
    useImageSpacing = static_cast< bool >( atoi( argv[7] ) );
    }

  double varianceValue = sigma * sigma;
  typename DiscreteGradientMagnitudeGaussianFunctionType::VarianceArrayType varianceArray;
  varianceArray.Fill( varianceValue );

  function->SetVariance( varianceArray );
  TEST_SET_GET_VALUE( varianceArray, function->GetVariance() );

  // Increase code coverage calling other variations of the SetVariance method
  function->SetVariance( varianceValue );
  TEST_SET_GET_VALUE( varianceArray, function->GetVariance() );

  // Test itkSetVectorMacro
  double varianceVector[DiscreteGradientMagnitudeGaussianFunctionType::VarianceArrayType::Length];
  for( unsigned int i = 0;
    i < DiscreteGradientMagnitudeGaussianFunctionType::VarianceArrayType::Length;
    i++ )
    {
      varianceVector[i] = varianceValue;
    }
  function->SetVariance( varianceVector );
  TEST_SET_GET_VALUE( varianceArray, function->GetVariance() );


  function->SetMaximumError( maxError );
  TEST_SET_GET_VALUE( maxError, function->GetMaximumError() );

  function->SetMaximumKernelWidth( maxKernelWidth );
  TEST_SET_GET_VALUE( maxKernelWidth, function->GetMaximumKernelWidth() );

  bool normalizeAcrossScale = true;
  TEST_SET_GET_BOOLEAN( function, NormalizeAcrossScale, normalizeAcrossScale );

  TEST_SET_GET_BOOLEAN( function, UseImageSpacing, useImageSpacing );

  function->SetInterpolationMode( interpolationMode );
  TEST_SET_GET_VALUE( interpolationMode, function->GetInterpolationMode() );


  function->Initialize();


  // Create image for storing result
  typename ImageType::Pointer output = ImageType::New();
  output->SetSpacing( inputImage->GetSpacing() );
  output->SetOrigin( inputImage->GetOrigin() );
  output->SetDirection( inputImage->GetDirection() );
  output->SetLargestPossibleRegion( inputImage->GetLargestPossibleRegion() );
  output->SetRequestedRegion( inputImage->GetRequestedRegion() );
  output->SetBufferedRegion( inputImage->GetBufferedRegion() );
  output->Allocate();
  output->FillBuffer( itk::NumericTraits<PixelType>::ZeroValue() );


  // Step over input and output images
  typedef itk::ImageRegionConstIterator< ImageType > ConstIteratorType;
  typedef itk::ImageRegionIterator< ImageType >      IteratorType;

  ConstIteratorType it ( inputImage, inputImage->GetRequestedRegion() );
  it.GoToBegin();
  IteratorType out( output, output->GetRequestedRegion() );
  out.GoToBegin();

  typedef typename DiscreteGradientMagnitudeGaussianFunctionType::PointType  PointType;
  PointType point;
  typedef typename DiscreteGradientMagnitudeGaussianFunctionType::ContinuousIndexType ContinuousIndexType;
  ContinuousIndexType cindex;
  const unsigned long nop = inputImage->GetRequestedRegion().GetNumberOfPixels();
  unsigned long pixelNumber = 0;
  while( !it.IsAtEnd() )
    {
    // To test all available Evaluate functions, we split it in three parts.
    if ( pixelNumber < nop / 3 )
      {
      out.Set( function->EvaluateAtIndex( it.GetIndex() ) );
      }
    else if ( pixelNumber < nop * 2 / 3 )
      {
      inputImage->TransformIndexToPhysicalPoint( it.GetIndex(), point );
      out.Set( function->Evaluate( point ) );
      }
    else
      {
      inputImage->TransformIndexToPhysicalPoint( it.GetIndex(), point );
      inputImage->TransformPhysicalPointToContinuousIndex( point, cindex );
      out.Set( function->EvaluateAtContinuousIndex( cindex ) );
      }
    ++it;
    ++out;
    ++pixelNumber;
    }

  // Rescale output
  typedef unsigned char                                                     OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension >                          OutputImageType;
  typedef itk::RescaleIntensityImageFilter< ImageType, OutputImageType >    RescaleType;
  typename RescaleType::Pointer rescaler = RescaleType::New();
  rescaler->SetInput( output );
  rescaler->SetOutputMinimum( itk::NumericTraits<OutputPixelType>::min() );
  rescaler->SetOutputMaximum( itk::NumericTraits<OutputPixelType>::max() );

  // Write the output image
  typedef itk::ImageFileWriter< OutputImageType > WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( rescaler->GetOutput() );


  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}

int itkDiscreteGradientMagnitudeGaussianImageFunctionTest( int argc, char* argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0]
      << "inputFileName"
        " outputFileName"
        " sigma"
        " [maximumError]"
        " [maximumKernelWidth]"
        " [interpolator]: 0: NearestNeighbourInterpolation; 1: LinearInterpolation"
        " useImageSpacing"
        << std::endl;
    return EXIT_FAILURE;
    }


  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  const unsigned int Dimension = 2;

  typedef float                               PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef itk::DiscreteGradientMagnitudeGaussianImageFunction< ImageType, PixelType >
    DiscreteGradientMagnitudeGaussianFunctionType;
  DiscreteGradientMagnitudeGaussianFunctionType::Pointer function =
    DiscreteGradientMagnitudeGaussianFunctionType::New();

  EXERCISE_BASIC_OBJECT_METHODS( function,
    DiscreteGradientMagnitudeGaussianImageFunction, ImageFunction );


  return itkDiscreteGradientMagnitudeGaussianImageFunctionTestND< Dimension >( argc, argv );
}
