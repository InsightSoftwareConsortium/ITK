/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIOTest4.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <fstream>
#include "itkRawImageIO.h"
#include "itkImageFileReader.h"
#include "itkImage.h"
#include "itkImageLinearIteratorWithIndex.h"


int itkRawImageIOTest4(int, char**)
{

  typedef unsigned short PixelType;
  const unsigned int ImageDimension = 2;

  typedef itk::RawImageIO<PixelType,ImageDimension> IOType;
  IOType::Pointer io = IOType::New();

  io->SetFileTypeToBinary();
  io->SetByteOrderToBigEndian();

  char filename[] = "test.raw";
  unsigned int dims[ImageDimension] = { 5, 5 };

  // Create the binary file
  ofstream outputFile;
#ifdef Win32
  outputFile.open( filename , ios::binary );
#else
  outputFile.open( filename );
#endif

  if( outputFile.fail() )
    {
    std::cerr << "itkRawImageIOTest4:Error writing the test file" << std::endl;
    return EXIT_FAILURE;
    }

  PixelType value = itk::NumericTraits< PixelType >::Zero;
  typedef itk::PixelTraits< PixelType >::ValueType ComponentType;
  typedef itk::ByteSwapper< ComponentType >              ByteSwapperType;
  unsigned int numberOfPixels = dims[0] * dims[1];
  for( unsigned int i = 0; i < numberOfPixels; i++ )
    {
    PixelType swappedValue = value;
    // make sure that the file is written in 
    // BigEndian regardless of the platform
    ByteSwapperType::SwapFromSystemToBigEndian( &swappedValue );
    outputFile.write( reinterpret_cast<char*>(&swappedValue), 
                      sizeof(swappedValue) );
    ++value;
    }
  outputFile.close();
 
  if( outputFile.fail() )
    {
    std::cerr << "itkRawImageIOTest4:Error writing the test file" << std::endl;
    return EXIT_FAILURE;
    }

 
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    io->SetDimensions( j, dims[j] );
    }


  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> ReaderType;

  
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( filename );
  reader->SetImageIO( io );
  reader->Update();


  typedef itk::ImageLinearIteratorWithIndex<ImageType> Iterator;
  Iterator it( reader->GetOutput(), reader->GetOutput()->GetBufferedRegion() );

  it.GoToBegin();
  it.SetDirection( 0 ); 

  value = itk::NumericTraits< PixelType >::Zero;
  while ( !it.IsAtEnd() )
    {
    while ( !it.IsAtEndOfLine() )
      {
      PixelType readValue = it.Get();
      std::cout << readValue << " ";
      if( readValue != value )
        {
        std::cerr << "At index " << it.GetIndex() << std::endl;
        std::cerr << "the value " << value << " was expected  " << std::endl;
        std::cerr << "but value " << readValue << " was read  " << std::endl;
        return EXIT_FAILURE;
        }
      ++it;
      ++value;
      }
    std::cout << std::endl;
    it.NextLine();
    }

  std::cout << "Test PASSED !" << std::endl;
  return EXIT_SUCCESS;

}

