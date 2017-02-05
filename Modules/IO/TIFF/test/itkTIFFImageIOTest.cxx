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
#include "itkTIFFImageIO.h"
#include "itkTestingMacros.h"
#include <fstream>

namespace
{

template< typename TImage >
bool TestMultipleReads( const std::string &fname, TImage*)
{
  typedef TImage                          ImageType;
  typedef itk::ImageFileReader<ImageType> ReaderType;

  typename ReaderType::Pointer reader = ReaderType::New();

  itk::TIFFImageIO::Pointer io = itk::TIFFImageIO::New();
  reader->SetFileName( fname.c_str() );
  reader->SetImageIO( io );

  try
    {
    reader->GetOutput()->SetRequestedRegionToLargestPossibleRegion();
    reader->GetOutput()->UpdateOutputInformation();
    reader->GetOutput()->PropagateRequestedRegion();
    reader->GetOutput()->UpdateOutputData();
    reader->GetOutput()->ReleaseData();
    reader->GetOutput()->UpdateOutputData();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader for bug  " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  return true;
}

#define SPECIFIC_IMAGEIO_MODULE_TEST

template< typename TImage >
int itkTIFFImageIOTestHelper( int, char * argv[] )
{
  typedef TImage                            ImageType;
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

  typename ReaderType::Pointer reader = ReaderType::New();
  typename WriterType::Pointer writer = WriterType::New();

  itk::TIFFImageIO::Pointer io = itk::TIFFImageIO::New();
  reader->SetFileName( argv[1] );
  reader->SetImageIO( io );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );

  // Test 2 reads with only one ReadImageInformation
  TestMultipleReads< ImageType >( argv[1], ITK_NULLPTR );


  typename ImageType::Pointer image = reader->GetOutput();

  image->Print( std::cout );

  typename ImageType::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region << std::endl;

  // Generate test image
  writer->SetInput( reader->GetOutput() );
  writer->SetFileName(argv[2]);
  writer->SetImageIO(io);

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
}

int itkTIFFImageIOTest( int argc, char* argv[] )
{

  unsigned int dimension = 2;
  unsigned int pixelType = 1;

  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0]
              << " Input Output [dimensionality (default: 2)]"
              << "[pixeltype: 1:uchar(default); 2:ushort; 3:short; 4:float]" << std::endl;
    return EXIT_FAILURE;
    }
  else if( argc == 4 )
    {
    dimension = atoi( argv[3] );
    }
  else if( argc == 5 )
    {
    dimension = atoi( argv[3] );
    pixelType = atoi( argv[4] );
    }

  if( dimension == 2 && pixelType == 1 )
    {
    typedef itk::RGBPixel< unsigned char > PixelType;
    return itkTIFFImageIOTestHelper< itk::Image<PixelType, 2> >( argc, argv );
    }
  else if (dimension == 2 && pixelType == 2 )
    {
    typedef itk::RGBPixel< unsigned short > PixelType;
    return itkTIFFImageIOTestHelper< itk::Image<PixelType, 2> >( argc, argv );
    }
  else if( dimension == 2 && pixelType == 3 )
    {
    typedef itk::RGBPixel<short> PixelType;
    return itkTIFFImageIOTestHelper< itk::Image<PixelType, 2> >( argc, argv );
    }
  else if( dimension == 3 && pixelType == 1 )
    {
    return itkTIFFImageIOTestHelper< itk::Image<unsigned char, 3> >( argc, argv );
    }
  else if( dimension == 3 && pixelType == 2 )
    {
    return itkTIFFImageIOTestHelper< itk::Image<unsigned short, 3> >( argc, argv );
    }
  else if( dimension == 3 && pixelType == 3 )
    {
    return itkTIFFImageIOTestHelper< itk::Image<short, 3> >( argc, argv );
    }
  else if (dimension == 3 && pixelType == 4)
    {
    return itkTIFFImageIOTestHelper< itk::Image<float, 3> >( argc, argv );
    }
  else if( dimension == 4 && pixelType == 1 )
    {
    return itkTIFFImageIOTestHelper< itk::Image<unsigned char, 4> >( argc, argv );
    }
  else if( dimension == 4 && pixelType == 2 )
    {
    return itkTIFFImageIOTestHelper< itk::Image<unsigned short, 4> >( argc, argv );
    }
  else if( dimension == 4 && pixelType == 3 )
    {
    itk::Image<short, 4>::Pointer dummy;
    return itkTIFFImageIOTestHelper< itk::Image<short, 4> >( argc, argv );
    }
  else if( dimension == 4 && pixelType == 4 )
    {
    return itkTIFFImageIOTestHelper< itk::Image<float, 4> >( argc, argv );
    }
  else
    {
    std::cerr << "Test failed!" << argv[0] << std::endl;
    std::cerr << " Unsupported dimensionality or pixelType provided." << std::endl;
    std::cerr << " Supported dimensionality: [2-4]; (default: 2)" << std::endl;
    std::cerr << " Supported pixelType: [1:uchar(default); 2:ushort; 3:short; 4:float]" << std::endl;
    return EXIT_FAILURE;
    }
}
