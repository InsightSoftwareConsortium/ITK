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
#include "itkDiscreteHessianGaussianImageFunction.h"
#include "itkMath.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkTestingMacros.h"


template < int VDimension >
int itkDiscreteHessianGaussianImageFunctionTestND( int argc, char* argv[] )
{
  const unsigned int Dimension = VDimension;

  typedef float                               PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  // Read input
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );


  // Create the itk::DiscreteHessianGaussianImageFunction
  typedef itk::DiscreteHessianGaussianImageFunction< ImageType, PixelType >
    HessianGaussianImageFunctionType;
  typename HessianGaussianImageFunctionType::TensorType hessian;
  typename HessianGaussianImageFunctionType::TensorType::EigenValuesArrayType eigenValues;

  typename HessianGaussianImageFunctionType::Pointer function =
    HessianGaussianImageFunctionType::New();


  function->SetInputImage( reader->GetOutput() );


  // Set up operator parameters
  double sigma = atof( argv[3] );

  double maxError = 0.001;
  unsigned int maxKernelWidth = 100;
  if( argc > 4 )
    {
    maxError = atof( argv[4] );
    }
  if( argc > 5 )
    {
    maxKernelWidth = atoi( argv[5] );
    }

  double varianceValue = sigma * sigma;
  typename HessianGaussianImageFunctionType::VarianceArrayType varianceArray;
  varianceArray.Fill( varianceValue );

  function->SetVariance( varianceArray );
  TEST_SET_GET_VALUE( varianceArray, function->GetVariance() );

  // Increase code coverage calling other variations of the SetVariance method
  function->SetVariance( varianceValue );
  TEST_SET_GET_VALUE( varianceArray, function->GetVariance() );

  // Test itkSetVectorMacro
  double varianceVector[HessianGaussianImageFunctionType::VarianceArrayType::Length];
  for( unsigned int i = 0;
    i < HessianGaussianImageFunctionType::VarianceArrayType::Length;
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

  bool useImageSpacing = true;
  TEST_SET_GET_BOOLEAN( function, UseImageSpacing, useImageSpacing );

  typename HessianGaussianImageFunctionType::InterpolationModeType interpolationMode =
    HessianGaussianImageFunctionType::NearestNeighbourInterpolation;

  function->SetInterpolationMode( interpolationMode );
  TEST_SET_GET_VALUE( interpolationMode, function->GetInterpolationMode() );


  function->Initialize();


  // Create image for storing result
  typedef typename ImageType::Pointer ImageTypePointer;

  ImageTypePointer output = ImageType::New();
  output->SetSpacing( reader->GetOutput()->GetSpacing() );
  output->SetOrigin( reader->GetOutput()->GetOrigin() );
  output->SetDirection( reader->GetOutput()->GetDirection() );
  output->SetLargestPossibleRegion( reader->GetOutput()->GetLargestPossibleRegion() );
  output->SetRequestedRegion( reader->GetOutput()->GetRequestedRegion() );
  output->SetBufferedRegion( reader->GetOutput()->GetBufferedRegion() );
  output->Allocate();
  output->FillBuffer( itk::NumericTraits<PixelType>::ZeroValue() );


  // Step over input and output images
  typedef itk::ImageRegionConstIterator< ImageType > ConstIteratorType;
  typedef itk::ImageRegionIterator< ImageType >      IteratorType;

  ConstIteratorType it( reader->GetOutput(), reader->GetOutput()->GetRequestedRegion() );
  it.GoToBegin();
  IteratorType outIter( output, output->GetRequestedRegion() );

  typedef typename HessianGaussianImageFunctionType::PointType  PointType;
  PointType point;
  typedef typename HessianGaussianImageFunctionType::ContinuousIndexType ContinuousIndexType;
  ContinuousIndexType cindex;
  const unsigned long nop = reader->GetOutput()->GetRequestedRegion().GetNumberOfPixels();
  unsigned long pixelNumber = 0;
  while( !it.IsAtEnd() )
    {
    if ( pixelNumber < nop / 3 )
      {
      hessian = function->EvaluateAtIndex( it.GetIndex() );
      }
    else if ( pixelNumber < nop * 2 / 3 )
      {
      reader->GetOutput()->TransformIndexToPhysicalPoint( it.GetIndex(), point );
      hessian = function->Evaluate( point );
      }
    else
      {
      reader->GetOutput()->TransformIndexToPhysicalPoint( it.GetIndex(), point );
      reader->GetOutput()->TransformPhysicalPointToContinuousIndex( point, cindex );
      hessian = function->EvaluateAtContinuousIndex( cindex );
      }

    hessian.ComputeEigenValues( eigenValues );

    PixelType maxEigen = eigenValues[0];
    for (unsigned int i = 1; i < Dimension; ++i)
      {
      maxEigen = std::max(eigenValues[i], maxEigen);
      }

    outIter.Set( maxEigen );
    ++outIter;

    ++it;
    ++pixelNumber;
    }

  // Write the output image
  typedef itk::ImageFileWriter< ImageType > WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( output );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  // Check that VarianceArrayType can be changed
  typedef typename HessianGaussianImageFunctionType::VarianceArrayType VarianceArrayType;
  VarianceArrayType varReturned = function->GetVariance();

  VarianceArrayType varChanged = varReturned;
  for( unsigned int i = 0; i < varChanged.Size(); ++i )
    {
    varChanged[i] *= 2.0;
    }
  function->SetVariance( varChanged );
  varReturned = function->GetVariance();
  for( unsigned int i = 0; i < Dimension; ++i )
    {
    if( varReturned[i] != varChanged[i] )
      {
      std::cout << "Test failed!" << std::endl;
      std::cout << "Error in GetVariance() at index [" << i << "]" << std::endl;
      std::cout << "Expected: " << varChanged[i] << ", but got: "
        << varReturned[i] << std::endl;
      return EXIT_FAILURE;
      }
    }


  double piValues[Dimension];
  for( unsigned int i = 0; i < Dimension; ++i )
    {
    piValues[i] = itk::Math::pi;
    }
  function->SetVariance( piValues );
  varReturned = function->GetVariance();
  for( unsigned int i = 0; i < Dimension; ++i )
    {
    if( itk::Math::NotAlmostEquals( varReturned[i], itk::Math::pi ) )
      {
      std::cout << "Test failed!" << std::endl;
      std::cout << "Error in GetVariance() at index [" << i << "]" << std::endl;
      std::cout << "Expected: " << itk::Math::pi << ", but got: "
        << varReturned[i] << std::endl;
      return EXIT_FAILURE;
      }
    }


  // Exercise another interpolation mode: LinearInterpolation
  {
  function->SetInterpolationMode( HessianGaussianImageFunctionType::LinearInterpolation );
  const ImageType * inputImage = reader->GetOutput();
  typename ImageType::RegionType region = inputImage->GetBufferedRegion();
  typename ImageType::SizeType size = region.GetSize();
  typename ImageType::IndexType index = region.GetIndex();
  // Aim for the pixel at the center of the image
  for( unsigned int i = 0; i < Dimension; ++i )
    {
    index[i] += size[i] / 2;
    }

  hessian = function->EvaluateAtIndex( index );
  inputImage->TransformIndexToPhysicalPoint( index, point );
  hessian = function->Evaluate( point );

  // Exercise the fractional computation of the linear interpolator
  for( unsigned int i = 0; i < Dimension; ++i )
    {
    cindex[i] = static_cast<double>( index[i] ) + 0.5;
    }

  hessian = function->EvaluateAtContinuousIndex( cindex );
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}

int itkDiscreteHessianGaussianImageFunctionTest( int argc, char* argv[] )
{
  if( argc < 4 )
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0]
      << "inputFileName"
        " outputFileName"
        " sigma"
        " [maximumError]"
        " [maximumKernelWidth]" << std::endl;
    return EXIT_FAILURE;
  }


  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  const unsigned int Dimension = 3;

  typedef float                               PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;


  typedef itk::DiscreteHessianGaussianImageFunction< ImageType, PixelType >
    HessianGaussianImageFunctionType;
  HessianGaussianImageFunctionType::Pointer function =
    HessianGaussianImageFunctionType::New();

  EXERCISE_BASIC_OBJECT_METHODS( function,
    DiscreteHessianGaussianImageFunction, ImageFunction );


  return itkDiscreteHessianGaussianImageFunctionTestND< Dimension >( argc, argv );
}
