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






// Helper class for reading a file and checking the content 
template< typename TImageType >
class RawImageIOReadFileTester
{

public:
// Only single method of this class
int Read( const char *filename , bool ReadBigEndian, unsigned int dims[] )
  {

    const unsigned int ImageDimension = TImageType::ImageDimension;

    typedef typename TImageType::PixelType   PixelType;
    typedef itk::ImageFileReader<TImageType> ReaderType;
    typedef itk::RawImageIO<PixelType,ImageDimension> IOType;

    typename IOType::Pointer io = IOType::New();

    io->SetFileTypeToBinary();
    
    if( ReadBigEndian )
      {
      io->SetByteOrderToBigEndian();
      }
    else 
      {
      io->SetByteOrderToLittleEndian();
      }


    for( unsigned int j = 0; j < TImageType::ImageDimension; j++ )
      {
      io->SetDimensions( j, dims[j] );
      }


    
    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName( filename );
    reader->SetImageIO( io );
    reader->Update();


    typedef itk::ImageLinearIteratorWithIndex<TImageType> Iterator;
    Iterator it( reader->GetOutput(), reader->GetOutput()->GetBufferedRegion() );

    it.GoToBegin();
    it.SetDirection( 0 ); 


    PixelType value = itk::NumericTraits< PixelType >::Zero;
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
    return EXIT_SUCCESS;
  }

};









int itkRawImageIOTest4(int, char**)
{

  typedef unsigned short PixelType;
  const unsigned int ImageDimension = 2;

  typedef itk::RawImageIO<PixelType,ImageDimension> IOType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;

  char filenameBigEndian[]     = "testBinaryBigEndian.raw";
  char filenameLittleEndian[]  = "testBinaryLittleEndian.raw";

  unsigned int dims[ImageDimension] = { 5, 5 };

  typedef itk::PixelTraits< PixelType >::ValueType ComponentType;
  typedef itk::ByteSwapper< ComponentType >              ByteSwapperType;

  PixelType value = itk::NumericTraits< PixelType >::Zero;
  unsigned int numberOfPixels = dims[0] * dims[1];




  // Create the BigEndian binary file
  std::ofstream outputFile1;
#ifdef _WIN32
  outputFile1.open( filenameBigEndian , std::ios::out | std::ios::binary );
#else
  outputFile1.open( filenameBigEndian );
#endif

  if( outputFile1.fail() )
    {
    std::cerr << "itkRawImageIOTest4:Error writing the test file" << std::endl;
    return EXIT_FAILURE;
    }

  for( unsigned int i = 0; i < numberOfPixels; i++ )
    {
    PixelType swappedValue = value;
    // make sure that the file is written in 
    // BigEndian regardless of the platform
    ByteSwapperType::SwapFromSystemToBigEndian( &swappedValue );
    outputFile1.write( reinterpret_cast<char*>(&swappedValue), 
                      sizeof(swappedValue) );
    ++value;
    }
  outputFile1.close();
 
  if( outputFile1.fail() )
    {
    std::cerr << "itkRawImageIOTest4:Error writing the test file" << std::endl;
    return EXIT_FAILURE;
    }




  // Create the LittleEndian binary file
  std::ofstream outputFile2;
#ifdef _WIN32
  outputFile2.open( filenameLittleEndian , std::ios::out | std::ios::binary );
#else
  outputFile2.open( filenameLittleEndian );
#endif

  if( outputFile2.fail() )
    {
    std::cerr << "itkRawImageIOTest4:Error writing the test file" << std::endl;
    return EXIT_FAILURE;
    }

  value = itk::NumericTraits< PixelType >::Zero;
  for( unsigned int i = 0; i < numberOfPixels; i++ )
    {
    PixelType swappedValue = value;
    // make sure that the file is written in 
    // LittleEndian regardless of the platform
    ByteSwapperType::SwapFromSystemToLittleEndian( &swappedValue );
    outputFile2.write( reinterpret_cast<char*>(&swappedValue), 
                      sizeof(swappedValue) );
    ++value;
    }
  outputFile2.close();
 
  if( outputFile2.fail() )
    {
    std::cerr << "itkRawImageIOTest4:Error writing the test file" << std::endl;
    return EXIT_FAILURE;
    }



  RawImageIOReadFileTester<ImageType>  readTester;


  int status = EXIT_FAILURE;

  std::cout << "Testing read of Big Endian File" << std::endl;
  bool fileIsBigEndian = true;
  status = readTester.Read( filenameBigEndian, fileIsBigEndian, dims );
  if( status==EXIT_FAILURE )
    {
    std::cerr << "Reading Raw BigEndian FAILED !!" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "Reading Raw BigEndian PASSED !!" << std::endl << std::endl;
    }

  std::cout << "Testing read of Little Endian File" << std::endl;
  fileIsBigEndian = false;
  status = readTester.Read( filenameLittleEndian, fileIsBigEndian, dims );
  if( status==EXIT_FAILURE )
    {
    std::cerr << "Reading Raw LittleEndian FAILED !!" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "Reading Raw LittleEndian PASSED !!" << std::endl << std::endl;
    }

  std::cout << "Test PASSED !!" << std::endl << std::endl;

  return EXIT_SUCCESS;

}


