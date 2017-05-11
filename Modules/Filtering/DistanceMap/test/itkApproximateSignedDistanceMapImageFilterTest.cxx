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
#include "itkTestingMacros.h"
#include "itkImageFileWriter.h"

namespace{

// simple signed distance function
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

int itkApproximateSignedDistanceMapImageFilterTest(int argc, char* argv[] )
{
  if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " InsideValue OutputImage\n";
    return EXIT_FAILURE;
    }

  const unsigned int ImageDimension = 2;
  typedef unsigned int  InputPixelType;
  typedef float         PixelType;
  typedef short         OutputPixelType;

  typedef itk::Image<InputPixelType,ImageDimension>  InputImageType;
  typedef itk::Image<PixelType,ImageDimension>       ImageType;
  typedef itk::Image<OutputPixelType,ImageDimension> OutputImageType;
  typedef itk::Point<double,ImageDimension>          PointType;

  // Make a binary input image based on the signed distance function
  // using the inside and outside values
  const InputPixelType InsideValue  = atoi( argv[1] );
  const InputPixelType OutsideValue = 0;

  InputImageType::Pointer image = InputImageType::New();
  InputImageType::SizeType size;
  size.Fill( 64 );
  InputImageType::RegionType region( size );

  image->SetRegions( region );
  image->Allocate();

  typedef itk::ImageRegionIteratorWithIndex<InputImageType> InputIterator;
  InputIterator iter( image, region );
  iter.GoToBegin();

  while( !iter.IsAtEnd() )
    {
    PointType point;
    image->TransformIndexToPhysicalPoint( iter.GetIndex(), point );
    iter.Set( SimpleSignedDistance( point ) > 0 ? OutsideValue : InsideValue );
    ++iter;
    }


  // Set up  image filter
  typedef itk::ApproximateSignedDistanceMapImageFilter<InputImageType,ImageType> DistanceType;
  DistanceType::Pointer distance = DistanceType::New();
  distance->SetInput( image );
  distance->SetInsideValue( InsideValue );
  distance->SetOutsideValue( OutsideValue );
  TEST_SET_GET_VALUE( InsideValue, distance->GetInsideValue() );
  TEST_SET_GET_VALUE( OutsideValue, distance->GetOutsideValue() );

  try
    {

    distance->Update();

    typedef itk::ShiftScaleImageFilter< ImageType, OutputImageType >  RescaleType;
    RescaleType::Pointer rescale = RescaleType::New();
    rescale->SetInput( distance->GetOutput() );
    rescale->SetScale( 1000 );
    rescale->Update();
    if( rescale->GetUnderflowCount() + rescale->GetOverflowCount() > 0 )
      {
        std::cerr << "Under-/overflow when scaling distances before writing distance map to disc." << std::endl;
      }
    typedef itk::ImageFileWriter<OutputImageType> OutputWriterType;
    OutputWriterType::Pointer owriter = OutputWriterType::New();
    owriter->SetInput( rescale->GetOutput() );
    owriter->SetFileName( argv[2] );
    owriter->Update();
    }
  catch (itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return EXIT_FAILURE;
    }

  PixelType maxDeviation = 0;

  typedef itk::ImageRegionConstIteratorWithIndex<ImageType> OutputIterator;
  OutputIterator oiter( distance->GetOutput(),
                        distance->GetOutput()->GetLargestPossibleRegion() );
  oiter.GoToBegin();

  while( !oiter.IsAtEnd() )
    {
    PointType point;
    image->TransformIndexToPhysicalPoint( oiter.GetIndex(), point );
    OutputPixelType deviation =
      itk::Math::abs(oiter.Get() - SimpleSignedDistance(point) );
    if (deviation > maxDeviation )
      {
      maxDeviation = deviation;
      }
    ++oiter;
    }
  std::cout << "dev " << maxDeviation << std::endl;

  // Exercise other member functions
  distance->Print( std::cout );

  std::cout << "The maximum error was " << maxDeviation << std::endl;
  if ( maxDeviation > 2 )
    {
    std::cout << "The output image had pixels too far away from the correct distance. ";
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;

}
