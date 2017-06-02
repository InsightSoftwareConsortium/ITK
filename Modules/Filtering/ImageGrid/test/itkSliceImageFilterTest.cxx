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

#include <itkSliceImageFilter.h>
#include <gtest/gtest.h>

#include "itkPhysicalPointImageSource.h"
#include "itkGaussianImageSource.h"
#include "itkImageRegionConstIterator.h"

// This test verifies the principle that the SliceImageFilter should
// not change the physical location of the signal. This is done by
// constructing an image of points, each corresponding to the phycical
// location of the center of the voxel, then verifying that the value
// still matches the physcial location.

namespace
{

// This function checks that all values in an image are equivalent to
// the physical point of the image.
template <typename TImageType>
bool CheckValueIsPhysicalPoint( const TImageType *img )
{

  typedef itk::ImageRegionConstIterator<TImageType> IteratorType;
  IteratorType it(img, img->GetBufferedRegion() );

  bool match = true;

  typename TImageType::PointType pt;
  img->TransformIndexToPhysicalPoint( it.GetIndex(), pt );
  while( !it.IsAtEnd() )
    {
    img->TransformIndexToPhysicalPoint( it.GetIndex(), pt );
    for ( unsigned int i = 0; i < TImageType::ImageDimension; ++i )
      {
      EXPECT_DOUBLE_EQ( pt[i], it.Get()[i] ) << "Index: " << it.GetIndex() << " Point: " << pt << " Value: " << it.Get() << std::endl, match = false;
      }

    ++it;
    }
  return match;
}

template <typename TImageType>
typename TImageType::Pointer RunFilter( const TImageType *img,
                                        typename TImageType::IndexType start,
                                        typename TImageType::IndexType stop,
                                        int step[TImageType::ImageDimension]
  )
{
  typedef itk::SliceImageFilter<TImageType, TImageType> FilterType;
  typename FilterType::Pointer sliceFilter = FilterType::New();

  sliceFilter->SetInput( img );
  sliceFilter->SetStart( start );
  sliceFilter->SetStop( stop );
  sliceFilter->SetStep( step );
  sliceFilter->UpdateLargestPossibleRegion();

  return sliceFilter->GetOutput();
}


}

TEST(SliceImageFilterTests, PhysicalPoint1)
{
  const unsigned int ImageDimension = 2;
  typedef itk::Point<double, ImageDimension>    PixelType;
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  typedef itk::PhysicalPointImageSource<ImageType> SourceType;
  SourceType::Pointer source = SourceType::New();


  // these size are chosen as a power of two and a prime number.
  SourceType::SizeValueType size[] = {128,127};
  source->SetSize( size );

  float origin[] = {1.1f, 2.22f};
  source->SetOrigin( origin );


  int step[ImageDimension];
  ImageType::IndexType start;
  ImageType::IndexType stop;
  stop[0] = size[0];
  stop[1] = size[1];

  for( start[0] = 0; start[0] < 10; ++start[0] )
    for( start[1] = 0; start[1] < 10; ++start[1] )
      for( step[0] = 1; step[0] < 10; ++step[0] )
        for( step[1] = 1; step[1] < 10; ++step[1] )
          {

          ImageType::Pointer img;

          img = RunFilter<ImageType>( source->GetOutput(), start, stop, step );

          EXPECT_TRUE( CheckValueIsPhysicalPoint( img.GetPointer() ) ) << "== Failed - step:" << step[0] << " " << step[1] << " start: " <<start[0] << " " << start[1] << std::endl;
          }

}

TEST(SliceImageFilterTests, PhysicalPoint2)
{
  const unsigned int ImageDimension = 2;
  typedef itk::Point<double, ImageDimension>    PixelType;
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  typedef itk::PhysicalPointImageSource<ImageType> SourceType;
  SourceType::Pointer source = SourceType::New();


  // these size are chosen as a power of two and a prime number.
  SourceType::SizeValueType size[] = {128,127};
  source->SetSize( size );

  float origin[] = {3.33f, 4.4444f};
  source->SetOrigin( origin );


  int step[ImageDimension] = {3,4};
  ImageType::IndexType start;
  ImageType::IndexType stop;

  ASSERT_TRUE(size[0] > 10);
  ASSERT_TRUE(size[1] > 10);

  for( start[0] = 0; start[0] < 10; ++start[0] )
    for( start[1] = 0; start[1] < 10; ++start[1] )
      for( stop[0] = size[0]; stop[0] > (ImageType::IndexValueType)(size[0] - 10); --stop[0] )
        for( stop[1] = size[1]; stop[1] > (ImageType::IndexValueType)(size[1] -10); --stop[1] )
          {

          ImageType::Pointer img;

          img = RunFilter<ImageType>( source->GetOutput(), start, stop, step );

          EXPECT_TRUE( CheckValueIsPhysicalPoint( img.GetPointer() ) ) <<
            "== Failed - step:" << step[0] << " " << step[1] << " start: " <<start[0] << " " << start[1]  <<
            " stop: " << stop[0] << " " << stop[1] << std::endl;
          }

}


TEST(SliceImageFilterTests, PhysicalPoint3)
{
  const unsigned int ImageDimension = 2;
  typedef itk::Point<double, ImageDimension>    PixelType;
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  typedef itk::PhysicalPointImageSource<ImageType> SourceType;
  SourceType::Pointer source = SourceType::New();


  // these size are chosen as a power of two and a prime number.
  SourceType::SizeValueType size[] = {16,17};
  source->SetSize( size );

  float origin[] = {3.33f, 4.4444f};
  source->SetOrigin( origin );

  int step[ImageDimension] = {-2,-2};
  ImageType::IndexType start;
  ImageType::IndexType stop;

  for( start[0] = 10; start[0] < 20; ++start[0] )
    for( start[1] = 10; start[1] < 20; ++start[1] )
      for( stop[0] = -5; stop[0] > 12; --stop[0] )
        for( stop[1] = -5; stop[1] > 12; --stop[1] )
          {

          ImageType::Pointer img;

          img = RunFilter<ImageType>( source->GetOutput(), start, stop, step );

          EXPECT_TRUE( CheckValueIsPhysicalPoint( img.GetPointer() ) ) <<
            "== Failed - step:" << step[0] << " " << step[1] <<
            " start: " << start[0] << " " << start[1]  << " stop: " << stop[0] << " " << stop[1] << std::endl;
          }

}


TEST(SliceImageFilterTests,Empty)
{
  const unsigned int ImageDimension = 2;
  typedef itk::Point<double, ImageDimension>    PixelType;
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  typedef itk::PhysicalPointImageSource<ImageType> SourceType;
  SourceType::Pointer source = SourceType::New();


  // these size are chosen as a power of two and a prime number.
  SourceType::SizeValueType size[] = {32,32};
  source->SetSize( size );

  int step[ImageDimension] = {1,1};
  ImageType::IndexType start;
  start.Fill( 10 );
  ImageType::IndexType stop;
  stop.Fill( 10 );


  ImageType::Pointer img;

  img = RunFilter<ImageType>( source->GetOutput(), start, stop, step );
  std::cout << img;

  for( unsigned int i = 0; i < ImageDimension; ++i)
    {
    EXPECT_EQ( 0u, img->GetLargestPossibleRegion().GetSize()[i] );
    }

  start[0] = 2;
  start[1] = 2;
  stop[0] = 10;
  stop[1] = 2;

  img = RunFilter<ImageType>( source->GetOutput(), start, stop, step );

  EXPECT_EQ( 8u, img->GetLargestPossibleRegion().GetSize()[0] );
  EXPECT_EQ( 0u, img->GetLargestPossibleRegion().GetSize()[1] );

}

TEST(SliceImageFilterTests,Coverage)
{
  const unsigned int ImageDimension = 3;
  typedef itk::Image<float, ImageDimension> ImageType;

  typedef itk::SliceImageFilter<ImageType, ImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();
  std::cout << filter;

  FilterType::IndexType idx;
  idx.Fill( 10 );

  filter->SetStart(idx);
  EXPECT_EQ( idx, filter->GetStart() );

  idx.Fill(11);
  filter->SetStart(11);
  EXPECT_EQ( idx, filter->GetStart() );

  idx.Fill(12);
  filter->SetStop(idx);
  EXPECT_EQ( idx, filter->GetStop() );

  idx.Fill(13);
  filter->SetStop(13);
  EXPECT_EQ( idx, filter->GetStop() );

  FilterType::ArrayType a;
  a.Fill(14);
  filter->SetStep(a);
  EXPECT_EQ( a, filter->GetStep() );

  a.Fill(15);
  filter->SetStep(15);
  EXPECT_EQ(a, filter->GetStep() );
}

TEST(SliceImageFilterTests,Sizes)
{
  const unsigned int ImageDimension = 3;
  typedef itk::Image<float, ImageDimension> ImageType;

  typedef itk::GaussianImageSource<ImageType> SourceType;
  SourceType::Pointer source = SourceType::New();

  SourceType::SizeType size = {{64,64,64}};
  source->SetSize(size);
  source->ReleaseDataFlagOn();

  typedef itk::SliceImageFilter<ImageType, ImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( source->GetOutput() );
  // check with default start, stop, step
  EXPECT_NO_THROW(filter->Update());
  EXPECT_EQ(filter->GetOutput()->GetLargestPossibleRegion().GetSize(), size) << "Check full size for defaults";


  filter = FilterType::New();
  filter->SetInput( source->GetOutput() );
  filter->SetStart( 64 );
  filter->SetStop(-1);
  filter->SetStep(-1);
  EXPECT_NO_THROW(filter->Update());
  EXPECT_EQ(filter->GetOutput()->GetLargestPossibleRegion().GetSize(), size) << "Check full size for negative step";

  std::cout << "Filter size: " << filter->GetOutput()->GetLargestPossibleRegion() << std::endl;
  std::cout << "Filter buffered size: " << filter->GetOutput()->GetBufferedRegion() << std::endl;
  std::cout << "Input size: " << size << std::endl;
}

TEST(SliceImageFilterTests,ExceptionalCases)
{
  const unsigned int ImageDimension = 3;
  typedef itk::Image<float, ImageDimension> ImageType;

  typedef itk::GaussianImageSource<ImageType> SourceType;
  SourceType::Pointer source = SourceType::New();


  typedef itk::SliceImageFilter<ImageType, ImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( source->GetOutput() );

  filter->SetStep( 0 );
  EXPECT_ANY_THROW( filter->Update() ) << "Check with 0 step";

  // for some reason after the above exception, the pipeline is not
  // clean, and Verify input information will not get executed again,
  // so just create a new filter...
  filter = FilterType::New();
  filter->SetInput( source->GetOutput() );
  filter->SetStart(10000);
  filter->SetStop(10001);
  EXPECT_NO_THROW( filter->Update() ) << "Check with over-sized start stop are clamped to zero";
  EXPECT_EQ( 0u, filter->GetOutput()->GetLargestPossibleRegion().GetNumberOfPixels() );


  filter = FilterType::New();
  filter->SetInput( source->GetOutput() );
  filter->SetStart(12);
  filter->SetStop(10);
  EXPECT_NO_THROW( filter->Update() ) << "Check stop is clamped";
  EXPECT_EQ( 0u, filter->GetOutput()->GetLargestPossibleRegion().GetNumberOfPixels() );


  filter = FilterType::New();
  filter->SetInput( source->GetOutput() );
  filter->SetStart(-12);
  filter->SetStop(-10);
  EXPECT_NO_THROW( filter->Update() ) << "Check undersized start and stop";
  EXPECT_EQ( 0u, filter->GetOutput()->GetLargestPossibleRegion().GetNumberOfPixels() );

  filter = FilterType::New();
  filter->SetInput( source->GetOutput() );
  filter->SetStart(-12);
  filter->SetStop(-10);
  filter->SetStep(-1);
  EXPECT_NO_THROW( filter->Update() ) << "Check undersized start and stop";
  EXPECT_EQ( 0u, filter->GetOutput()->GetLargestPossibleRegion().GetNumberOfPixels() );

}
