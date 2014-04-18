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

namespace {
template< typename TImageType >
int itkPhysicalPointImageSourceTest(  const std::string &fname,
                                      typename TImageType::SizeType &size,
                                      typename TImageType::SpacingType &spacing,
                                      typename TImageType::PointType &origin,
                                      typename TImageType::DirectionType &direction)
{

  typedef TImageType ImageType;

  typedef itk::PhysicalPointImageSource< ImageType > PointerSourceType;

  typename PointerSourceType::Pointer source = PointerSourceType::New();
  source->SetSize(size);
  source->SetSpacing(spacing);
  source->SetOrigin(origin);
  source->SetDirection( direction );

  try
    {
    source->UpdateLargestPossibleRegion();
     }
   catch (itk::ExceptionObject & err)
     {
     std::cout << "Exception in filter execution!" << std::endl;
     std::cout << err << std::endl;
     return EXIT_FAILURE;
     }

  typedef itk::Image< typename itk::NumericTraits< typename ImageType::PixelType >::ValueType, ImageType::ImageDimension> ValueImageType;

  typedef itk::VectorIndexSelectionCastImageFilter< ImageType, ValueImageType > ValueImageCastFilter;
  typename ValueImageCastFilter::Pointer vif = ValueImageCastFilter::New();
  vif->SetInput( source->GetOutput() );
  vif->SetIndex(0);

    typedef itk::ImageFileWriter<ValueImageType> WriterType;
   typename WriterType::Pointer writer = WriterType::New();
   writer->SetFileName( fname );
   writer->SetInput( vif->GetOutput() );
   try
     {
     std::cout << "Writing image to " << fname << std::endl;
     writer->Update();
     }
   catch (itk::ExceptionObject & err)
     {
     std::cout << "Exception in writing!" << std::endl;
     std::cout << err << std::endl;
     return EXIT_FAILURE;
     }
   return EXIT_SUCCESS;
}

}
int itkPhysicalPointImageSourceTest( int argc, char *argv[] )
{
  double theta = 0;
  const unsigned int ImageDimension = 2;

  if ( argc < 3 )
    {
    std::cout << "Usage: " << argv[0] << " outputImage whichTest [ theta ]" << std::endl;
    return EXIT_FAILURE;
    }
  if ( argc >= 4 )
    {
    theta = atof( argv[3] );
    }

  itk::Size<ImageDimension> size;
  size.Fill(64);

  typedef itk::Image<unsigned char,ImageDimension> ImageType;
  ImageType::SpacingType spacing(1.0);
  ImageType::PointType origin(0.0);
  ImageType::DirectionType direction;

  direction.SetIdentity();

  int test;
  if ( atoi( argv[2] ) == 0 )
    {
    test = itkPhysicalPointImageSourceTest<itk::Image<itk::Point<double, ImageDimension>, ImageDimension > >( std::string( argv[1] ), size, spacing, origin, direction );
    }
  else if ( atoi( argv[2] ) == 1 )
    {
    test = itkPhysicalPointImageSourceTest<itk::VectorImage<double, ImageDimension> >( std::string( argv[1] ), size, spacing, origin, direction );
    }
  else if ( atoi( argv[2] ) == 2 )
    {
    spacing.Fill( 1.123 );
    origin.Fill( -0.987 );
    test = itkPhysicalPointImageSourceTest<itk::Image<itk::Point<float, ImageDimension>, ImageDimension > >( std::string( argv[1] ), size, spacing, origin, direction );
    }
  else
    {
    itk::SpacePrecisionType M[] = { std::cos( theta ), -std::sin( theta ),
                    std::sin( theta ), std::cos( theta ) };

    direction = vnl_matrix<itk::SpacePrecisionType>( M, 2, 2);
    test = itkPhysicalPointImageSourceTest< itk::VectorImage<float, ImageDimension> >( std::string( argv[1] ), size, spacing, origin, direction );
    }

  return test;
}
