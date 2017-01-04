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

#include "itkPhysicalPointImageSource.h"

#include "itkImageFileWriter.h"

#include "itkVectorImage.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkTestingMacros.h"


namespace {
template< typename TImageType >
int itkPhysicalPointImageSourceTest( const std::string &fname,
                                     typename TImageType::SizeType &size,
                                     typename TImageType::SpacingType &spacing,
                                     typename TImageType::PointType &origin,
                                     typename TImageType::DirectionType &direction )
{

  typedef TImageType ImageType;

  typedef itk::PhysicalPointImageSource< ImageType > PhysicalPointImageSourceType;

  typename PhysicalPointImageSourceType::Pointer physicalPointImageSource =
    PhysicalPointImageSourceType::New();

  TRY_EXPECT_NO_EXCEPTION( physicalPointImageSource->UpdateLargestPossibleRegion() );

  physicalPointImageSource->SetSize(size);
  physicalPointImageSource->SetSpacing(spacing);
  physicalPointImageSource->SetOrigin(origin);
  physicalPointImageSource->SetDirection( direction );

  TRY_EXPECT_NO_EXCEPTION( physicalPointImageSource->UpdateLargestPossibleRegion() );

  typedef itk::Image< typename itk::NumericTraits< typename ImageType::PixelType >::ValueType, ImageType::ImageDimension >
    ValueImageType;

  typedef itk::VectorIndexSelectionCastImageFilter< ImageType, ValueImageType > ValueImageCastFilter;
  typename ValueImageCastFilter::Pointer vif = ValueImageCastFilter::New();
  vif->SetInput( physicalPointImageSource->GetOutput() );
  vif->SetIndex( 0 );

  typedef itk::ImageFileWriter< ValueImageType > WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( fname );
  writer->SetInput( vif->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

   return EXIT_SUCCESS;
}

}
int itkPhysicalPointImageSourceTest( int argc, char *argv[] )
{
  if ( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " outputImage whichTest [ theta ]" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int ImageDimension = 2;
  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, ImageDimension > ImageType;

  itk::Size< ImageDimension > size;
  size.Fill( 64 );

  ImageType::SpacingType spacing( 1.0 );
  ImageType::PointType origin( 0.0 );
  ImageType::DirectionType direction;
  direction.SetIdentity();

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  typedef itk::PhysicalPointImageSource< itk::Image< itk::Point< double, ImageDimension >, ImageDimension > >
    PhysicalPointImageSourceType;

  // Instantiate the filter
  PhysicalPointImageSourceType::Pointer physicalPointImageSource =
    PhysicalPointImageSourceType::New();

  EXERCISE_BASIC_OBJECT_METHODS( physicalPointImageSource,
    PhysicalPointImageSource, GenerateImageSource );


  double theta = 0;
  if( argc >= 4 )
    {
    theta = atof( argv[3] );
    }

  int testStatus = EXIT_SUCCESS;
  if( atoi( argv[2] ) == 0 )
    {
    testStatus =
      itkPhysicalPointImageSourceTest< itk::Image< itk::Point< double, ImageDimension >, ImageDimension > >(
      std::string( argv[1] ), size, spacing, origin, direction );
    }
  else if( atoi( argv[2] ) == 1 )
    {
    testStatus = itkPhysicalPointImageSourceTest< itk::VectorImage< double, ImageDimension > >(
      std::string( argv[1] ), size, spacing, origin, direction );
    }
  else if( atoi( argv[2] ) == 2 )
    {
    spacing.Fill( 1.123 );
    origin.Fill( -0.987 );
    testStatus =
      itkPhysicalPointImageSourceTest< itk::Image< itk::Point< float, ImageDimension>, ImageDimension > >(
      std::string( argv[1] ), size, spacing, origin, direction );
    }
  else
    {
    itk::SpacePrecisionType M[] = { std::cos( theta ), -std::sin( theta ),
      std::sin( theta ), std::cos( theta ) };

    direction = vnl_matrix< itk::SpacePrecisionType >( M, 2, 2 );
    testStatus = itkPhysicalPointImageSourceTest< itk::VectorImage< float, ImageDimension > >(
      std::string( argv[1] ), size, spacing, origin, direction );
    }

  std::cout << "Test finished" << std::endl;
  return testStatus;
}
