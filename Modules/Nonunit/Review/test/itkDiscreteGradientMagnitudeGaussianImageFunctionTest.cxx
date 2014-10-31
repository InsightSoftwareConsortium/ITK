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

template < int VDimension >
int itkDiscreteGradientMagnitudeGaussianImageFunctionTestND( int argc, char* argv[] )
{

  // Verify the number of parameters in the command line
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "inputFileName outputFileName sigma (maximum_error) (maximum_kernel_width)" << std::endl;
    return EXIT_FAILURE;
    }

  // Define the dimension of the images
  const unsigned int Dimension = VDimension;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;

  // Read input
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
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
  ImageType *inputImage = reader->GetOutput();
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

  // Setup operator parameters
  double variance = atof( argv[3] );
  variance *= variance;

  double maxError = 0.001;
  unsigned int maxKernelWidth = 100;
  if( argc == 5 )
    {
    maxError = atof( argv[4] );
    }
  else if( argc > 5 )
    {
    maxError = atof( argv[4] );
    maxKernelWidth = atoi( argv[5] );
    }

  // Create function
  typedef itk::DiscreteGradientMagnitudeGaussianImageFunction< ImageType, PixelType >
    DiscreteGradientMagnitudeGaussianFunctionType;
  typename DiscreteGradientMagnitudeGaussianFunctionType::Pointer function =
    DiscreteGradientMagnitudeGaussianFunctionType::New();
  function->SetInputImage( inputImage );
  function->SetMaximumError( maxError );
  function->SetMaximumKernelWidth( maxKernelWidth );
  function->SetVariance( variance );
  function->SetNormalizeAcrossScale( true );
  function->SetUseImageSpacing( true );
  function->SetInterpolationMode( DiscreteGradientMagnitudeGaussianFunctionType::NearestNeighbourInterpolation );
  function->Initialize();

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

  // Write output
  typedef itk::ImageFileWriter< OutputImageType > WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( rescaler->GetOutput() );
  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject &err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  // Test some functions
  typedef typename DiscreteGradientMagnitudeGaussianFunctionType::VarianceArrayType VarianceArrayType;
  VarianceArrayType varReturned = function->GetVariance();
  for ( unsigned int i = 0; i < Dimension; ++i )
  {
    if ( varReturned[ i ] != variance )
    {
      std::cout << "GetVariance()[" << i << "] failed. Expected: "
        << variance
        << " but got: "
        << varReturned[ i ] << std::endl;
      return EXIT_FAILURE;
    }
  }
  if ( function->GetMaximumError() != maxError )
  {
    std::cout << "GetMaximumError failed. Expected: "
      << maxError
      << " but got: "
      << function->GetMaximumError() << std::endl;
    return EXIT_FAILURE;
  }
  if ( function->GetNormalizeAcrossScale() != true )
  {
    std::cout << "GetNormalizeAcrossScale failed. Expected: "
      << true
      << " but got: "
      << function->GetNormalizeAcrossScale() << std::endl;
    return EXIT_FAILURE;
  }
  if ( function->GetUseImageSpacing() != true )
  {
    std::cout << "GetUseImageSpacing failed. Expected: "
      << true
      << " but got: "
      << function->GetUseImageSpacing() << std::endl;
    return EXIT_FAILURE;
  }
  if ( function->GetMaximumKernelWidth() != maxKernelWidth )
  {
    std::cout << "GetMaximumKernelWidth failed. Expected: "
      << maxKernelWidth
      << " but got: "
      << function->GetMaximumKernelWidth() << std::endl;
    return EXIT_FAILURE;
  }
  if ( function->GetInterpolationMode() != DiscreteGradientMagnitudeGaussianFunctionType::NearestNeighbourInterpolation )
  {
    std::cout << "GetInterpolationMode failed. Expected: "
      << DiscreteGradientMagnitudeGaussianFunctionType::NearestNeighbourInterpolation
      << " but got: "
      << function->GetInterpolationMode() << std::endl;
    return EXIT_FAILURE;
  }

  // Call PrintSelf.
  function->Print( std::cout );

  return EXIT_SUCCESS;
}

int itkDiscreteGradientMagnitudeGaussianImageFunctionTest(int argc, char* argv[] )
{
  return itkDiscreteGradientMagnitudeGaussianImageFunctionTestND< 2 >( argc, argv );
}
