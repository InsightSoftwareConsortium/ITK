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

#include <fstream>
#include "itkRawImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageLinearIteratorWithIndex.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST


// Helper class for reading a file and checking the content
template< typename TImageType >
class RawImageIOReadFileTester
{
public:
// Only single method of this class
int Read( const char *filename , bool ReadBigEndian, unsigned int dims[] )
  {

    const unsigned int ImageDimension = TImageType::ImageDimension;

    typedef typename TImageType::PixelType            PixelType;
    typedef itk::ImageFileReader<TImageType>          ReaderType;
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

    try
      {
      reader->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception while reading file " << filename << std::endl;
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << "Reading file " << filename << " succeeded " << std::endl;

    typedef itk::ImageLinearIteratorWithIndex<TImageType> Iterator;
    Iterator it( reader->GetOutput(), reader->GetOutput()->GetBufferedRegion() );

    it.GoToBegin();
    it.SetDirection( 0 );


    PixelType value = itk::NumericTraits< PixelType >::ZeroValue();
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

int itkRawImageIOTest4(int argc, char*argv[])
{

  typedef unsigned short PixelType;
  const unsigned int ImageDimension = 2;

  typedef itk::Image<PixelType,ImageDimension>      ImageType;

  unsigned int dims[ImageDimension] = { 5, 5 };

  typedef itk::PixelTraits< PixelType >::ValueType ComponentType;
  typedef itk::ByteSwapper< ComponentType >        ByteSwapperType;

  PixelType value = itk::NumericTraits< PixelType >::ZeroValue();
  unsigned int numberOfPixels = dims[0] * dims[1];

  if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " Output1 Output2\n";
    return EXIT_FAILURE;
    }


  // Create the BigEndian binary file
  std::ofstream outputFile1(argv[1], std::ios::out);
  outputFile1.close();
#ifdef _WIN32
  outputFile1.open( argv[1] , std::ios::out | std::ios::binary );
#else
  outputFile1.open( argv[1] );
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
  std::ofstream outputFile2(argv[2], std::ios::out);
  outputFile2.close();
#ifdef _WIN32
  outputFile2.open( argv[2] , std::ios::out | std::ios::binary );
#else
  outputFile2.open( argv[2] );
#endif

  if( outputFile2.fail() )
    {
    std::cerr << "itkRawImageIOTest4:Error writing the test file" << std::endl;
    return EXIT_FAILURE;
    }

  value = itk::NumericTraits< PixelType >::ZeroValue();
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


  int status;

  std::cout << "Testing read of Big Endian File" << std::endl;
  bool fileIsBigEndian = true;
  status = readTester.Read( argv[1], fileIsBigEndian, dims );
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
  status = readTester.Read( argv[2], fileIsBigEndian, dims );
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
