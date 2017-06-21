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

#include "itkApproximateSignedDistanceMapImageFilter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkNumericTraits.h"
#include "itkTestingMacros.h"


// Anonymous namespace
namespace
{

// Simple signed distance function
template <typename TPoint>
double
SimpleSignedDistance( const TPoint & p )
{
  TPoint center;
  center.Fill( 32 );
  double radius = 16;

  double accum = 0.0;
  for( unsigned int j = 0; j < TPoint::PointDimension; j++ )
    {
    accum += itk::Math::sqr( p[j] - center[j] );
    }
  accum = std::sqrt( accum );
  return ( accum - radius );
}

}

int itkApproximateSignedDistanceMapImageFilterTest( int argc, char* argv[] )
{
  if(argc < 3)
    {
    std::cerr << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << argv[0] << " insideValue outputImage" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int ImageDimension = 2;

  typedef unsigned int  InputPixelType;
  typedef float         OutputPixelType;
  typedef short         WriterPixelType;
  typedef double        PointPixelType;

  typedef itk::Image< InputPixelType, ImageDimension >  InputImageType;
  typedef itk::Image< OutputPixelType, ImageDimension > OutputImageType;
  typedef itk::Image< WriterPixelType, ImageDimension > WriterImageType;
  typedef itk::Point< PointPixelType, ImageDimension >  PointType;

  // Make a binary input image based on the signed distance function
  // using the inside and outside values
  const InputPixelType insideValue =  atoi( argv[1] );
  const InputPixelType outsideValue = 0;

  InputImageType::Pointer image = InputImageType::New();
  InputImageType::SizeType size;
  size.Fill( 64 );
  InputImageType::RegionType region( size );

  image->SetRegions( region );
  image->Allocate();

  typedef itk::ImageRegionIteratorWithIndex< InputImageType > InputIteratorType;
  InputIteratorType iter( image, region );
  iter.GoToBegin();

  while( !iter.IsAtEnd() )
    {
    PointType point;
    image->TransformIndexToPhysicalPoint( iter.GetIndex(), point );
    iter.Set( SimpleSignedDistance( point ) > 0 ? outsideValue : insideValue );
    ++iter;
    }

  // Set up the filter
  typedef itk::ApproximateSignedDistanceMapImageFilter< InputImageType, OutputImageType >
    DistanceMapFilterType;
  DistanceMapFilterType::Pointer signedDistanceMapFilter =
    DistanceMapFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( signedDistanceMapFilter,
    ApproximateSignedDistanceMapImageFilter, ImageToImageFilter );

  signedDistanceMapFilter->SetInput( image );

  signedDistanceMapFilter->SetInsideValue( insideValue );
  TEST_SET_GET_VALUE( insideValue, signedDistanceMapFilter->GetInsideValue() );

  signedDistanceMapFilter->SetOutsideValue( outsideValue );
  TEST_SET_GET_VALUE( outsideValue, signedDistanceMapFilter->GetOutsideValue() );

  TRY_EXPECT_NO_EXCEPTION( signedDistanceMapFilter->Update() );


  // Write the output image
  typedef itk::ShiftScaleImageFilter< OutputImageType, WriterImageType >
    RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetInput( signedDistanceMapFilter->GetOutput() );
  rescaler->SetScale( 1000 );

  TRY_EXPECT_NO_EXCEPTION( rescaler->Update() );


  if( rescaler->GetUnderflowCount() + rescaler->GetOverflowCount() > 0 )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Under-/overflow when scaling distances before writing distance map to disc: " << std::endl
      << "Underflow: " << rescaler->GetUnderflowCount() << std::endl
      << "Overflow: " << rescaler->GetOverflowCount() << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ImageFileWriter< WriterImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( rescaler->GetOutput() );
  writer->SetFileName( argv[2] );


  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  OutputPixelType maxDistance = 0;

  typedef itk::ImageRegionConstIteratorWithIndex< OutputImageType > OutputIteratorType;
  OutputIteratorType oIt( signedDistanceMapFilter->GetOutput(),
    signedDistanceMapFilter->GetOutput()->GetLargestPossibleRegion() );
  oIt.GoToBegin();

  while( !oIt.IsAtEnd() )
    {
    PointType point;
    image->TransformIndexToPhysicalPoint( oIt.GetIndex(), point );
    OutputPixelType distance =
      itk::Math::abs( oIt.Get() - SimpleSignedDistance( point ) );
    if( distance > maxDistance )
      {
      maxDistance = distance;
      }
    ++oIt;
    }

  // Regression test
  OutputPixelType maxAllowedDistance = 2;
  if( maxDistance > maxAllowedDistance )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "The output image had pixels too far away from the correct distance." << std::endl;
    std::cout << "The maximum error was: "
      <<  static_cast< itk::NumericTraits< OutputPixelType >::PrintType >( maxDistance ) << std::endl;
    std::cout << "The maximum allowed error is: "
      << static_cast< itk::NumericTraits< OutputPixelType >::PrintType >( maxAllowedDistance ) << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}
