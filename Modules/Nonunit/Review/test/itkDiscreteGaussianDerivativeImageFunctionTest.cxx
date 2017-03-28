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
#include "itkDiscreteGaussianDerivativeImageFunction.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkTestingMacros.h"


template < int VDimension >
int itkDiscreteGaussianDerivativeImageFunctionTestND( int argc, char* argv[] )
{
  const unsigned int Dimension = VDimension;

  typedef float                               PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  // Read the input image
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );

  ImageType *inputImage = reader->GetOutput();


  // Create the itk::DiscreteGaussianDerivativeImageFunction
  typedef itk::DiscreteGaussianDerivativeImageFunction< ImageType, PixelType >
    GaussianDerivativeImageFunctionType;
  typename GaussianDerivativeImageFunctionType::Pointer function =
    GaussianDerivativeImageFunctionType::New();

  function->SetInputImage( inputImage );


  // Set up operator parameters
  typename GaussianDerivativeImageFunctionType::OrderArrayType order;
  for( unsigned int i = 0; i < order.Size(); i++ )
    {
    order[i] = static_cast<
      typename GaussianDerivativeImageFunctionType::OrderArrayType::ValueType >(
      atoi( argv[3] ) );
    }

  double sigma = atof( argv[4] );

  double maxError = 0.001;
  unsigned int maxKernelWidth = 100;
  typename GaussianDerivativeImageFunctionType::InterpolationModeType interpolationMode =
    GaussianDerivativeImageFunctionType::NearestNeighbourInterpolation;

  if( argc > 5 )
    {
    maxError = atof( argv[5] );
    }
  if( argc > 6 )
    {
    maxKernelWidth = atoi( argv[6] );
    }
  if( argc > 7 )
    {
    interpolationMode =
      static_cast< typename GaussianDerivativeImageFunctionType::InterpolationModeType >(
      atoi( argv[7] ) );
    }


  function->SetOrder( order );
  TEST_SET_GET_VALUE( order, function->GetOrder() );

  typename GaussianDerivativeImageFunctionType::VarianceArrayType variance;
  variance.Fill( sigma * sigma );

  function->SetSigma( sigma );
  TEST_SET_GET_VALUE( variance, function->GetVariance() );

  function->SetMaximumError( maxError );
  TEST_SET_GET_VALUE( maxError, function->GetMaximumError() );

  function->SetMaximumKernelWidth( maxKernelWidth );
  TEST_SET_GET_VALUE( maxKernelWidth, function->GetMaximumKernelWidth() );

  bool normalizeAcrossScale = true;
  TEST_SET_GET_BOOLEAN( function, NormalizeAcrossScale, normalizeAcrossScale );

  bool useImageSpacing = true;
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

  typedef typename GaussianDerivativeImageFunctionType::PointType  PointType;
  PointType point;
  typedef typename GaussianDerivativeImageFunctionType::ContinuousIndexType ContinuousIndexType;
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
  typedef unsigned char                                                   OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension >                        OutputImageType;
  typedef itk::RescaleIntensityImageFilter< ImageType, OutputImageType >  RescaleType;

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

int itkDiscreteGaussianDerivativeImageFunctionTest( int argc, char* argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0]
      << "inputFileName"
        " outputFileName"
        " order"
        " sigma"
        " [maximumError]"
        " [maximumKernelWidth]" << std::endl;
    return EXIT_FAILURE;
    }


  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  const unsigned int Dimension = 2;

  typedef float                               PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef itk::DiscreteGaussianDerivativeImageFunction< ImageType, PixelType >
    GaussianDerivativeImageFunctionType;
  GaussianDerivativeImageFunctionType::Pointer function =
    GaussianDerivativeImageFunctionType::New();

  EXERCISE_BASIC_OBJECT_METHODS( function,
    DiscreteGaussianDerivativeImageFunction, ImageFunction );


  return itkDiscreteGaussianDerivativeImageFunctionTestND< Dimension >( argc, argv );
}
