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

#include "itkVectorResampleImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"
#include "itkTestingMacros.h"

int itkVectorResampleImageFilterTest( int argc, char * argv[] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " outputImage" << std::endl;
    return EXIT_FAILURE;
    }

  const     unsigned int    Dimension = 2;
  typedef   unsigned char   PixelComponentType;

  typedef itk::RGBPixel< PixelComponentType > PixelType;
  typedef itk::Image< PixelType,  Dimension > ImageType;

  typedef itk::VectorResampleImageFilter< ImageType, ImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, VectorResampleImageFilter,
    ImageToImageFilter );

  FilterWatcher watcher(filter);

  typedef itk::VectorLinearInterpolateImageFunction<
    ImageType, double > InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  filter->SetInterpolator( interpolator );
  TEST_SET_GET_VALUE( interpolator, filter->GetInterpolator() );

  typedef itk::IdentityTransform< double, Dimension > TransformType;
  TransformType::Pointer transform = TransformType::New();

  filter->SetTransform( transform );
  TEST_SET_GET_VALUE( transform, filter->GetTransform() );

  ImageType::SpacingType spacing;
  spacing.Fill( 1.0 );

  ImageType::PointType origin;
  origin.Fill( 0.0 );

  ImageType::RegionType     region;
  ImageType::SizeType       size;
  ImageType::IndexType      start;

  size[0] = 128;
  size[1] = 128;

  start[0] = 0;
  start[1] = 0;

  region.SetSize( size );
  region.SetIndex( start );

  ImageType::Pointer image = ImageType::New();

  image->SetOrigin( origin );
  image->SetSpacing( spacing );
  image->SetRegions( region );
  image->Allocate();

  PixelType pixelValue;

  itk::ImageRegionIteratorWithIndex< ImageType > it( image, region );

  // Fill the image with some color pattern
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    ImageType::IndexType index = it.GetIndex();
    pixelValue.SetRed( index[0] * 2 );
    pixelValue.SetGreen( index[0] + index[1] );
    pixelValue.SetBlue( index[1] * 2 );
    it.Set( pixelValue );
    ++it;
    }

  PixelType blackValue;
  blackValue.Fill( 0 );

  filter->SetDefaultPixelValue( blackValue );
  TEST_SET_GET_VALUE( blackValue, filter->GetDefaultPixelValue() );

  // Set the spacing for the resampling
  spacing[0] *= 2.0;
  spacing[1] *= 2.0;

  filter->SetOutputSpacing( spacing );
  TEST_SET_GET_VALUE( spacing, filter->GetOutputSpacing() );

  // Keep the input image origin
  filter->SetOutputOrigin( origin );
  TEST_SET_GET_VALUE( origin, filter->GetOutputOrigin() );

  // Set the size
  size[0] /= 2;
  size[1] /= 2;

  filter->SetSize( size );
  TEST_SET_GET_VALUE( size, filter->GetSize() );

  // Set the output direction
  FilterType::DirectionType outputDirection =
    image->GetDirection();

  filter->SetOutputDirection( outputDirection );
  TEST_SET_GET_VALUE( outputDirection, filter->GetOutputDirection() );

  // Set the start index
  FilterType::IndexType outputStartIndex =
    image->GetLargestPossibleRegion().GetIndex();

  filter->SetOutputStartIndex( outputStartIndex );
  TEST_SET_GET_VALUE( outputStartIndex, filter->GetOutputStartIndex() );


  filter->SetInput( image );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  // Write an image for regression testing
  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput (filter->GetOutput());
  writer->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
