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
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include <fstream>


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkBMPImageIOTest( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " input output" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int    Dimension = 2;
  typedef unsigned char ComponentType;

  typedef itk::RGBPixel< ComponentType >      PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  itk::ImageFileReader< ImageType >::Pointer reader =
    itk::ImageFileReader< ImageType >::New();

  reader->SetFileName( argv[1] );

  reader->UpdateOutputInformation();

  std::cout << "PixelType: "
    << reader->GetImageIO()->GetPixelTypeAsString( reader->GetImageIO()->GetPixelType() )
    << std::endl;
  std::cout << "ComponentType: "
    << reader->GetImageIO()->GetComponentTypeAsString( reader->GetImageIO()->GetComponentType() )
    << std::endl;
  std::cout << "NumberOfComponents: "
    << reader->GetImageIO()->GetNumberOfComponents() << std::endl;

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );

  ImageType::Pointer image = reader->GetOutput();

  image->Print( std::cout );

  ImageType::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "LargestPossibleRegion " << region;

  // Print the IO
  reader->GetImageIO()->Print( std::cout );

  // Generate test image
  itk::ImageFileWriter< ImageType >::Pointer writer;
  writer = itk::ImageFileWriter< ImageType >::New();
  writer->SetInput( reader->GetOutput() );
  writer->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  // Print the IO
  writer->GetImageIO()->Print( std::cout );

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
