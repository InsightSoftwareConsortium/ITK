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
#include "itkRescaleIntensityImageFilter.h"

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
    accum += vnl_math_sqr( p[j] - center[j] );
    }
  accum = std::sqrt( accum );
  return ( accum - radius );
}

}

int itkApproximateSignedDistanceMapImageFilterTest(int argc, char* argv[] )
{
  if(argc < 2)
    {
    std::cerr << "Usage: " << argv[0] << " OutputImage\n";
    return EXIT_FAILURE;
    }

  const unsigned int ImageDimension = 2;
  typedef unsigned int  InputPixelType;
  typedef float         OutputPixelType;
  typedef unsigned char WriterPixelType;

  typedef itk::Image<InputPixelType,ImageDimension>  InputImageType;
  typedef itk::Image<OutputPixelType,ImageDimension> OutputImageType;
  typedef itk::Image<WriterPixelType,ImageDimension> WriterImageType;
  typedef itk::Point<double,ImageDimension>          PointType;

  // Make a binary input image based on the signed distance function
  // using the inside and outside values
  const InputPixelType InsideValue = 100;
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
  typedef itk::ApproximateSignedDistanceMapImageFilter<InputImageType,OutputImageType> DistanceType;
  DistanceType::Pointer distance = DistanceType::New();
  distance->SetInput( image );
  distance->SetInsideValue(InsideValue);
  if( distance->GetInsideValue() != InsideValue )
    {
    std::cerr <<"distance->GetInsideValue() != InsideValue" <<std::endl;
    return EXIT_FAILURE;
    }
  distance->SetOutsideValue(OutsideValue);
  if( distance->GetOutsideValue() != OutsideValue )
    {
    std::cerr <<"distance->GetOutsideValue() != OutsideValue" <<std::endl;
    return EXIT_FAILURE;
    }


  try
    {

    distance->Update();

    typedef itk::RescaleIntensityImageFilter<OutputImageType, WriterImageType> RescaleType;
    RescaleType::Pointer rescale = RescaleType::New();
    rescale->SetInput(distance->GetOutput());
    rescale->SetOutputMinimum(0);
    rescale->SetOutputMaximum(255);

    typedef itk::ImageFileWriter<WriterImageType> OutputWriterType;
    OutputWriterType::Pointer owriter = OutputWriterType::New();
    owriter->SetInput( rescale->GetOutput() );
    owriter->SetFileName( argv[1] );
    owriter->Update();
    }
  catch (itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return EXIT_FAILURE;
    }

  OutputPixelType maxDeviation = 0;

  typedef itk::ImageRegionConstIteratorWithIndex<OutputImageType> OutputIterator;
  OutputIterator oiter( distance->GetOutput(),
                       distance->GetOutput()->GetLargestPossibleRegion() );
  oiter.GoToBegin();

  while( !oiter.IsAtEnd() )
    {
    PointType point;
    image->TransformIndexToPhysicalPoint( oiter.GetIndex(), point );
    OutputPixelType deviation =
      vnl_math_abs(oiter.Get() - SimpleSignedDistance(point) );
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
