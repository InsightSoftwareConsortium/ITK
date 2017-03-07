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

#include <iostream>
#include <algorithm>
#include "itkPNGImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMetaDataDictionary.h"
#include "itkTestingMacros.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkPNGImageIOTestPalette( int argc, char * argv[] )
{
  if( argc != 5 )
    {
    std::cerr << "Usage: " << argv[0]
      << " input"
      << " output"
      << " expandRGBPalette"
      << " isPaletteImage"
      << std::endl;
    return EXIT_FAILURE;
    }
  const unsigned long                         Dimension = 2;
  typedef unsigned char                       ScalarPixelType;

  typedef itk::Image< ScalarPixelType, Dimension >  ScalarImageType;
  typedef itk::ImageFileReader< ScalarImageType >   ReaderType;
  typedef itk::ImageFileWriter< ScalarImageType >   WriterType;
  typedef itk::PNGImageIO                           IOType;

  IOType::Pointer io = IOType::New();

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();


  EXERCISE_BASIC_OBJECT_METHODS( io, PNGImageIO, ImageIOBase );

  const bool expandRGBPalette = static_cast< bool >( atoi(argv[3]) );
  const bool isPaletteImage   = static_cast< bool >( atoi(argv[4]) );
  TEST_SET_GET_BOOLEAN( io, ExpandRGBPalette, expandRGBPalette );

  // Exercise exception cases
  size_t sizeOfActualIORegion = io->GetIORegion().GetNumberOfPixels() *
    ( io->GetComponentSize() * io->GetNumberOfComponents() );
  char *loadBuffer = new char[sizeOfActualIORegion];

  TRY_EXPECT_EXCEPTION( io->Read( loadBuffer ) );


  io->SetFileName( argv[1] );
  reader->SetFileName( argv[1] );
  reader->SetImageIO( io );

  if( io->CanReadFile( "" ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "No filename specified." << std::endl;
    std::cout << "CanReadFile: "
      << "Expected false but got true" << std::endl;
    return EXIT_FAILURE;
    }

  if( !io->SupportsDimension( Dimension ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "itk::PNGImageIO does not support dimension: " << Dimension << std::endl;
    return EXIT_FAILURE;
    }

  if( io->CanStreamRead() )
    {
    std::cout << "itk::PNGImageIO can stream read" << std::endl;
    }
  else
    {
    std::cout << "itk::PNGImageIO cannot stream read" << std::endl;
    }


  if( !io->CanReadFile( argv[1] ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "itk::PNGImageIO cannot read file "
      << argv[1] << std::endl;
    return EXIT_FAILURE;
    }

  // Try reading
  TRY_EXPECT_NO_EXCEPTION( reader->Update() );

  // Try Palette reading and scalar image reading
  if( io->GetExpandRGBPalette() )
    {
      if( isPaletteImage )
        {
        std::cout << "Input is a defined as palette image, expanding to RGB. " << std::endl;
        }
      else
        {
        std::cout << "Input is a defined as a non palette image. " << std::endl;
        }
    }
  else
    {
    if( isPaletteImage )
      {
      std::cout << "Input is a defined as palette image, trying to read it as scalar. " << std::endl;
      }
    }

  if( !io->GetExpandRGBPalette() && isPaletteImage )
    {
    if( io->GetIsReadAsScalarPlusPalette() )
      {
      // print palette
      std::cout << "Image successfully read as Scalar." << std::endl;
      IOType::PaletteType palette = io->GetColorPalette();
      std::cout << "Palette: " << std::endl;
      for( unsigned int i = 0; i < palette.size(); ++i )
        {
        std::cout << "[" << i << "]:" << palette[i] << std::endl;
        }
      }
    else
      {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Could not read image data of this palette image as scalar."
                << std::endl;
      return EXIT_SUCCESS;
      }
    }
  else
    {
    std::cout << "Image read as Greyscale (conversion)." << std::endl;
    }

  // Try writing
  writer->SetInput( reader->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->SetImageIO( io );

  TRY_EXPECT_NO_EXCEPTION( writer->Write() );

  // Exercise other methods
  itk::ImageIOBase::SizeType pixelStride = io->GetPixelStride();
  std::cout << "PixelStride: "
    << itk::NumericTraits< itk::ImageIOBase::SizeType >::PrintType( pixelStride )
    << std::endl;

  // ToDo
  // When the palette has made into the Metadata Dictionary (as opposed to the ImageIO):
  // Use the MetaDataDictionary
  // io->SetMetaDataDictionary();


  // Release memory
  delete[] loadBuffer;

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
