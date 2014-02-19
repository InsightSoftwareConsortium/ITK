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
#include "itkRescaleIntensityImageFilter.h"
#include "itkTestingMacros.h"

template < int VDimension >
int itkDiscreteHessianGaussianImageFunctionTestND( int argc, char* argv[] )
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
  typedef float                             PixelType;
  typedef itk::Image<PixelType, Dimension>  ImageType;

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

  // Create images for storing result
  typedef typename ImageType::Pointer ImageTypePointer;
  std::vector<ImageTypePointer> outputs;
  for( unsigned int i=0; i<Dimension; i++ )
    {
    ImageTypePointer output = ImageType::New();
    output->SetSpacing( reader->GetOutput()->GetSpacing() );
    output->SetOrigin( reader->GetOutput()->GetOrigin() );
    output->SetDirection( reader->GetOutput()->GetDirection() );
    output->SetLargestPossibleRegion( reader->GetOutput()->GetLargestPossibleRegion() );
    output->SetRequestedRegion( reader->GetOutput()->GetRequestedRegion() );
    output->SetBufferedRegion( reader->GetOutput()->GetBufferedRegion() );
    output->Allocate();
    output->FillBuffer( itk::NumericTraits<PixelType>::Zero );
    outputs.push_back( output );
    }

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
  typedef itk::DiscreteHessianGaussianImageFunction< ImageType, PixelType >
    HessianGaussianImageFunctionType;
  typename HessianGaussianImageFunctionType::TensorType hessian;
  typename HessianGaussianImageFunctionType::TensorType::EigenValuesArrayType eigenValues;
  typename HessianGaussianImageFunctionType::Pointer function =
    HessianGaussianImageFunctionType::New();
  function->SetInputImage( reader->GetOutput() );
  function->SetMaximumError( maxError );
  function->SetMaximumKernelWidth( maxKernelWidth );
  function->SetVariance( variance );
  function->SetNormalizeAcrossScale( true );
  function->SetUseImageSpacing( true );
  function->SetInterpolationMode( HessianGaussianImageFunctionType::NearestNeighbourInterpolation );
  function->Initialize( );

  // Step over input and output images
  typedef itk::ImageRegionConstIterator< ImageType > ConstIteratorType;
  typedef itk::ImageRegionIterator< ImageType >      IteratorType;

  ConstIteratorType it ( reader->GetOutput(), reader->GetOutput()->GetRequestedRegion() );
  it.GoToBegin();
  std::vector< IteratorType > outs;
  for( unsigned int i=0; i<Dimension; i++ )
    {
    IteratorType out( outputs[i], outputs[i]->GetRequestedRegion() );
    out.GoToBegin();
    outs.push_back( out );
    }

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

    for( unsigned int i=0; i<Dimension; i++ )
      {
      outs[i].Set( eigenValues[i] );
      ++outs[i];
      }
    ++it;
    ++pixelNumber;
    }

  // Write outputs
  typedef unsigned char                                  OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension >       OutputImageType;
  typedef itk::ImageFileWriter< OutputImageType >        WriterType;

  typename WriterType::Pointer writer = WriterType::New();

  typedef itk::RescaleIntensityImageFilter< ImageType, OutputImageType > RescaleType;

  typename RescaleType::Pointer rescaler = RescaleType::New();

  rescaler->SetOutputMinimum( itk::NumericTraits<OutputPixelType>::min() );
  rescaler->SetOutputMaximum( itk::NumericTraits<OutputPixelType>::max() );

  for( unsigned int i=0; i<Dimension; i++ )
    {
    try
      {
      // Rescale
      rescaler->SetInput( outputs[i] );

      // Write
      char filename[255];
      sprintf( filename, argv[2], i );
      writer->SetFileName( filename );
      writer->SetInput( rescaler->GetOutput() );
      writer->Update();
      rescaler->GetOutput()->DisconnectPipeline( );
      outputs[i]->DisconnectPipeline( );
      }
    catch ( itk::ExceptionObject &err)
      {
      std::cout << "ExceptionObject caught !" << std::endl;
      std::cout << err << std::endl;
      return EXIT_FAILURE;
      }
    }

  const bool trueValue = true;
  const bool falseValue = false;

  // Test some functions
  typedef typename HessianGaussianImageFunctionType::VarianceArrayType VarianceArrayType;
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

  // Check that VarianceArrayType can be changed
  VarianceArrayType varChanged = varReturned;
  for ( unsigned int i = 0; i < Dimension; ++i )
  {
    varChanged[i] *= 2.0;
  }
  function->SetVariance( varChanged );
  varReturned = function->GetVariance();
  for ( unsigned int i = 0; i < Dimension; ++i )
  {
    if ( varReturned[ i ] != varChanged[i] )
    {
      std::cout << "GetVariance()[" << i << "] failed. Expected: "
        << varChanged[i]
        << " but got: "
        << varReturned[ i ] << std::endl;
      return EXIT_FAILURE;
    }
  }

  const double pivalue = 3.1415;
  double pivalues[Dimension];
  for ( unsigned int i = 0; i < Dimension; ++i )
  {
    pivalues[i] = pivalue;
  }
  function->SetVariance( pivalues );
  varReturned = function->GetVariance();
  for ( unsigned int i = 0; i < Dimension; ++i )
  {
    if ( varReturned[ i ] != pivalue )
    {
      std::cout << "GetVariance()[" << i << "] failed. Expected: "
        << pivalue
        << " but got: "
        << varReturned[ i ] << std::endl;
      return EXIT_FAILURE;
    }
  }


  TEST_SET_GET_VALUE( maxError, function->GetMaximumError() );

  function->NormalizeAcrossScaleOn();
  TEST_SET_GET_VALUE( trueValue, function->GetNormalizeAcrossScale() );
  function->NormalizeAcrossScaleOff();
  TEST_SET_GET_VALUE( falseValue, function->GetNormalizeAcrossScale() );
  function->SetNormalizeAcrossScale(trueValue);
  TEST_SET_GET_VALUE( trueValue, function->GetNormalizeAcrossScale() );
  function->SetNormalizeAcrossScale(falseValue);
  TEST_SET_GET_VALUE( falseValue, function->GetNormalizeAcrossScale() );

  function->UseImageSpacingOn();
  TEST_SET_GET_VALUE( trueValue, function->GetUseImageSpacing() );
  function->UseImageSpacingOff();
  TEST_SET_GET_VALUE( falseValue, function->GetUseImageSpacing() );
  function->SetUseImageSpacing(trueValue);
  TEST_SET_GET_VALUE( trueValue, function->GetUseImageSpacing() );
  function->SetUseImageSpacing(falseValue);
  TEST_SET_GET_VALUE( falseValue, function->GetUseImageSpacing() );


  if ( function->GetMaximumKernelWidth() != maxKernelWidth )
  {
    std::cout << "GetMaximumKernelWidth failed. Expected: "
      << maxKernelWidth
      << " but got: "
      << function->GetMaximumKernelWidth() << std::endl;
    return EXIT_FAILURE;
  }
  if ( function->GetInterpolationMode() != HessianGaussianImageFunctionType::NearestNeighbourInterpolation )
  {
    std::cout << "GetInterpolationMode failed. Expected: "
      << HessianGaussianImageFunctionType::NearestNeighbourInterpolation
      << " but got: "
      << function->GetInterpolationMode() << std::endl;
    return EXIT_FAILURE;
  }

  // Call PrintSelf.
  function->Print( std::cout );

  // Exercise another interpolation mode: LinearInterpolation
  {
  function->SetInterpolationMode( HessianGaussianImageFunctionType::LinearInterpolation );
  const ImageType * inputImage = reader->GetOutput();
  typename ImageType::RegionType region = inputImage->GetBufferedRegion();
  typename ImageType::SizeType size = region.GetSize();
  typename ImageType::IndexType index = region.GetIndex();
  // Aim for the pixel at the center of the image
  for( unsigned int i=0; i<Dimension; ++i )
    {
    index[i] += size[i] / 2;
    }
  hessian = function->EvaluateAtIndex( index );
  inputImage->TransformIndexToPhysicalPoint( index, point );
  hessian = function->Evaluate( point );

  // Exercise the fractional computation of the linear interpolator
  for( unsigned int i=0; i<Dimension; ++i )
    {
    cindex[i] = static_cast<double>( index[i] ) + 0.5;
    }
  hessian = function->EvaluateAtContinuousIndex( cindex );
  }

  return EXIT_SUCCESS;
}

int itkDiscreteHessianGaussianImageFunctionTest(int argc, char* argv[] )
{
  return itkDiscreteHessianGaussianImageFunctionTestND< 3 >( argc, argv );
}
