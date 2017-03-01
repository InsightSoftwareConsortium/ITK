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

#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkTimeProbesCollectorBase.h"
#include "itkTIFFImageIO.h"
#include "itkTestingMacros.h"

#define SPECIFIC_IMAGEIO_MODULE_TEST

namespace
{

template< typename TImage >
int itkLargeTIFFImageWriteReadTestHelper( std::string filename, typename TImage::SizeType size )
{
  typedef TImage                        ImageType;
  typedef typename ImageType::PixelType PixelType;

  typedef itk::ImageFileWriter< ImageType >   WriterType;
  typedef itk::ImageFileReader< ImageType >   ReaderType;

  typedef itk::ImageRegionIterator< ImageType >       IteratorType;
  typedef itk::ImageRegionConstIterator< ImageType >  ConstIteratorType;

  typedef itk::SizeValueType                 SizeValueType;

  typename ImageType::RegionType region;
  typename ImageType::IndexType index;

  PixelType pixelValue;

  itk::TimeProbesCollectorBase chronometer;

  {
  // Write block
  typename ImageType::Pointer image = ImageType::New();
  index.Fill( 0 );
  region.SetSize( size );
  region.SetIndex( index );

  image->SetRegions( region );

  SizeValueType numberOfPixels = 1;
  for (unsigned int i = 0; i < ImageType::ImageDimension; ++i )
    {
    numberOfPixels *= static_cast< SizeValueType >( region.GetSize( i ) );
    }

  const SizeValueType oneMegaByte = SizeValueType( 1024 ) *
    SizeValueType( 1024 );

  const SizeValueType sizeInBytes = sizeof(PixelType) * numberOfPixels;

  const SizeValueType sizeInMegaBytes = sizeInBytes / oneMegaByte;


  std::cout << "Trying to allocate an image of size " << sizeInMegaBytes
    << " MB " << std::endl;

  chronometer.Start( "Allocate" );
  image->Allocate();
  chronometer.Stop( "Allocate" );

  std::cout << "Initializing pixel values" << std::endl;

  IteratorType itr( image, region );
  itr.GoToBegin();

  pixelValue = itk::NumericTraits< PixelType >::ZeroValue();

  chronometer.Start( "Initializing" );
  while( !itr.IsAtEnd() )
    {
    itr.Set( pixelValue );
    ++pixelValue;
    ++itr;
    }
  chronometer.Stop( "Initializing" );

  std::cout << "Trying to write the image to disk" << std::endl;

  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput( image );
  writer->SetFileName( filename );

  chronometer.Start( "Write" );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  chronometer.Stop( "Write" );

  image = ITK_NULLPTR;
  } // end write block to free the memory

  std::cout << "Trying to read the image back from disk" << std::endl;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( filename );

  itk::TIFFImageIO::Pointer io = itk::TIFFImageIO::New();
  reader->SetImageIO( io );

  chronometer.Start( "Read" );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );

  chronometer.Stop( "Read" );

  typename ImageType::ConstPointer readImage = reader->GetOutput();

  ConstIteratorType ritr( readImage, region );

  ritr.GoToBegin();

  std::cout << "Comparing the pixel values" << std::endl;

  pixelValue = itk::NumericTraits< PixelType >::ZeroValue();

  chronometer.Start( "Compare" );
  while( !ritr.IsAtEnd() )
    {
    if( ritr.Get() != pixelValue )
      {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error while comparing pixel value at index: " << ritr.GetIndex() << std::endl;
      std::cerr << "Expected: " << pixelValue << ", but got: " << ritr.Get() << std::endl;
      return EXIT_FAILURE;
      }

    ++pixelValue;
    ++ritr;
    }
  chronometer.Stop( "Compare" );

  chronometer.Report( std::cout );

  std::cout << std::endl;
  std::cout << "Test PASSED !" << std::endl;

  return EXIT_SUCCESS;
}
}

int itkLargeTIFFImageWriteReadTest( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cout << "Usage: " << argv[0]
      << " outputFileName"
      << " numberOfPixelsInOneDimension"
      << " [numberOfZslices]" << std::endl;
    return EXIT_FAILURE;
    }

  const std::string filename = argv[1];

  if( argc == 3 )
    {
    const unsigned int Dimension = 2;

    typedef unsigned short                      PixelType;
    typedef itk::Image< PixelType, Dimension>   ImageType;

    ImageType::SizeType size;

    size.Fill( atol( argv[2] ) );

    return itkLargeTIFFImageWriteReadTestHelper< ImageType >( filename, size );
    }
  else
    {
    const unsigned int Dimension = 3;

    typedef unsigned short                      PixelType;
    typedef itk::Image< PixelType, Dimension>   ImageType;

    ImageType::SizeType size;

    size.Fill( atol( argv[2] ) );
    size[2] = atol( argv[3] );

    return itkLargeTIFFImageWriteReadTestHelper< ImageType >( filename, size );
    }

}
