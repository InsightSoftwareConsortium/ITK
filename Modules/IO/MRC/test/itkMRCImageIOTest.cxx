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
#include "itkMRCImageIO.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

static unsigned int m_CallNumber = 0;

template <typename TImageType>
class MRCImageIOTester
{
public:
  virtual int Test(int argc, char* argv[] );

  virtual ~MRCImageIOTester(){ }

  static bool Write( const std::string &filePrefix, std::string &outputPath );

  static bool Read( const std::string &filePrefix,
                    std::string &outputPath,
                    unsigned int index );

};

template <typename TImageType>
bool MRCImageIOTester<TImageType>
::Write( const std::string &filePrefix, std::string &outputPath )
{
  try
    {
    ++m_CallNumber;

    typedef TImageType                    ImageType;
    typedef typename ImageType::PixelType PixelType;

    // allocate an 10x10x10 image
    typename ImageType::Pointer image = ImageType::New();
    typename ImageType::SizeType m_ImageSize;
    m_ImageSize.Fill(10);
    image->SetRegions( m_ImageSize );
    image->Allocate();

    unsigned int cnt = 0;
    itk::ImageRegionIterator< ImageType > i( image, image->GetLargestPossibleRegion() );
    i.GoToBegin();
    while (!i.IsAtEnd() )
      {
      // fill the image switching between these pixels
      switch (cnt%4)
        {
        case 0:
          i.Set( itk::NumericTraits<PixelType>::ZeroValue() );
          break;
        case 1:
          i.Set( itk::NumericTraits<PixelType>::OneValue() );
          break;
        case 2:
          i.Set( itk::NumericTraits<PixelType>::min( PixelType() ) );
          break;
        case 3:
          i.Set( itk::NumericTraits<PixelType>::max( PixelType() ) );
        }
      ++cnt;
      ++i;
      }

    typedef itk::ImageFileWriter<ImageType> ImageFileWriterType;
    typename ImageFileWriterType::Pointer writer = ImageFileWriterType::New();
    writer->SetInput( image );

    // force use of MRCImageIO
    typedef itk::MRCImageIO IOType;
    IOType::Pointer mrcIO = IOType::New();
    writer->SetImageIO(mrcIO);

    std::ostringstream m_NameWithIndex;
    m_NameWithIndex << filePrefix << "_" << m_CallNumber << ".mrc";

    std::ostringstream m_OutputFileName;

#if defined(WIN32) // windows
    // if it ends in \\ just append the name
    if (outputPath[outputPath.size()-1] == '\\')
      {
      m_OutputFileName << outputPath << m_NameWithIndex.str();
      }
    else
      {
      m_OutputFileName << outputPath << "\\" << m_NameWithIndex.str();
      }

#else /// POSIX UNIX

    // if it ends in / just append the name
    if (outputPath[outputPath.size()-1] == '/')
      {
      m_OutputFileName << outputPath << m_NameWithIndex.str();
      }
    // otherwise, add / and the name
    else
      {
      m_OutputFileName << outputPath << "/" << m_NameWithIndex.str();
      }
#endif

    writer->SetFileName( m_OutputFileName.str() );
    writer->Update();

    // test the CanWriteFile function after the fact (should always be true at
    // this point)
    if (!mrcIO->CanWriteFile(m_OutputFileName.str().c_str() ) )
      {
      return false;
      }

    return true;
    }
  catch(itk::ExceptionObject &e)
    {
    std::cout << e.GetDescription() << std::endl;
    return false;
    }

}

template <typename TImageType>
bool MRCImageIOTester<TImageType>
::Read( const std::string &filePrefix,
        std::string &outputPath,
        unsigned int index )
{
  try
    {
    typedef TImageType                    ImageType;
    typedef typename ImageType::PixelType PixelType;

    typedef itk::ImageFileReader<ImageType> ImageFileReaderType;
    typename ImageFileReaderType::Pointer reader = ImageFileReaderType::New();

    // force use of MRCImageIO
    typedef itk::MRCImageIO IOType;
    IOType::Pointer mrcIO = IOType::New();
    reader->SetImageIO(mrcIO);

    // construct the image filename
    std::ostringstream m_NameWithIndex;
    m_NameWithIndex << filePrefix << "_" << index << ".mrc";

    std::ostringstream m_OutputFileName;

#if defined(WIN32) // windows
    // if it ends in \\ just append the name
    if (outputPath[outputPath.size()-1] == '\\')
      {
      m_OutputFileName << outputPath << m_NameWithIndex.str();
      }
    else
      {
      m_OutputFileName << outputPath << "\\" << m_NameWithIndex.str();
      }

#else /// POSIX UNIX

    // if it ends in / just append the name
    if (outputPath[outputPath.size()-1] == '/')
      {
      m_OutputFileName << outputPath << m_NameWithIndex.str();
      }
    // otherwise, add / and the name
    else
      {
      m_OutputFileName << outputPath << "/" << m_NameWithIndex.str();
      }
#endif
    reader->SetFileName( m_OutputFileName.str() );

    // read the image
    typename ImageType::Pointer image = reader->GetOutput();
    reader->Update();

    // test the CanReadFile function after the fact (should always be true at
    // this point)
    if (!mrcIO->CanReadFile(m_OutputFileName.str().c_str() ) )
      {
      return false;
      }

    // check the size
    typename ImageType::RegionType region = image->GetLargestPossibleRegion();
    typename ImageType::SizeType size = region.GetSize();
    bool sizeGood = true;
    for (unsigned int i = 0; i < ImageType::GetImageDimension(); i++)
      {
      if (size[i] != 10)
        {
        sizeGood = false;
        break;
        }
      }
    if (!sizeGood)
      {
      std::cout << "Error: Size didn't read properly" << std::endl;
      return false;
      }

    // check each pixel
    bool                                  pixelsGood = true;
    unsigned int                          cnt = 0;
    itk::ImageRegionIterator< ImageType > iter( image, region );
    iter.GoToBegin();
    while (!iter.IsAtEnd() && pixelsGood)
      {
      // check image switching between these pixels
      switch (cnt%4)
        {
        case 0:
          if (itk::Math::NotExactlyEquals(iter.Get(), itk::NumericTraits<PixelType>::ZeroValue()) )
            {
            pixelsGood = false;
            }
          break;
        case 1:
          if (itk::Math::NotExactlyEquals(iter.Get(), itk::NumericTraits<PixelType>::OneValue()) )
            {
            pixelsGood = false;
            }
          break;
        case 2:
          if (itk::Math::NotExactlyEquals(iter.Get(), itk::NumericTraits<PixelType>::min( PixelType() )) )
            {
            pixelsGood = false;
            }
          break;
        case 3:
          if (itk::Math::NotExactlyEquals(iter.Get(), itk::NumericTraits<PixelType>::max( PixelType() )) )
            {
            pixelsGood = false;
            }
        }
      ++cnt;
      ++iter;
      }

    if (!pixelsGood)
      {
      std::cout << "Error: Pixels didn't read properly" << std::endl;
      return false;
      }

    // reading successful, so return true
    return true;

    }
  catch(itk::ExceptionObject &e)
    {
    std::cout << e.GetDescription() << std::endl;
    return false;
    }
}

// int MRCImageIOTester::Test(int argc, char* argv[] )
int itkMRCImageIOTest(int argc, char* argv[])
{

  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " outputPath" << std::endl;
    return EXIT_FAILURE;
    }

  std::string outputPath = argv[1];
  std::string filePrefix = argv[0];

  //
  // test all usable pixeltypes
  //

  // unsigned char
  typedef itk::Image<unsigned char, 3> ImageTypeUnsignedChar3;
  if (!(MRCImageIOTester< ImageTypeUnsignedChar3 >::Write(filePrefix, outputPath) ) )
    {
    std::cout << "[FAILED] writing (unsighed char)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (unsighed char)" << std::endl;
  if (!(MRCImageIOTester< ImageTypeUnsignedChar3 >::Read(filePrefix, outputPath, m_CallNumber) ) )
    {
    std::cout << "[FAILED] reading (unsighed char)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (unsighed char)" << std::endl;

  // short
  typedef itk::Image<short, 3> ImageTypeShort3;
  if (!(MRCImageIOTester< ImageTypeShort3 >::Write(filePrefix, outputPath) ) )
    {
    std::cout << "[FAILED] writing (short)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (short)" << std::endl;
  if (!(MRCImageIOTester< ImageTypeShort3 >::Read(filePrefix, outputPath, m_CallNumber) ) )
    {
    std::cout << "[FAILED] reading (short)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (short)" << std::endl;

  // float
  typedef itk::Image<float, 3> ImageTypeFloat3;
  if (!(MRCImageIOTester< ImageTypeFloat3 >::Write(filePrefix, outputPath) ) )
    {
    std::cout << "[FAILED] writing (float)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (float)" << std::endl;
  if (!(MRCImageIOTester< ImageTypeFloat3 >::Read(filePrefix, outputPath, m_CallNumber) ) )
    {
    std::cout << "[FAILED] reading (float)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (float)" << std::endl;

  // unsigned short
  typedef itk::Image<unsigned short, 3> ImageTypeUnsignedShort3;
  if (!(MRCImageIOTester< ImageTypeUnsignedShort3 >::Write(filePrefix, outputPath) ) )
    {
    std::cout << "[FAILED] writing (unsighed short)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (unsighed short)" << std::endl;
  if (!(MRCImageIOTester< ImageTypeUnsignedShort3 >::Read(filePrefix, outputPath, m_CallNumber) ) )
    {
    std::cout << "[FAILED] reading (unsighed short)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (unsighed short)" << std::endl;

  // RGBPixel<unsigned char>
  typedef itk::RGBPixel<unsigned char> PixelTypeRGB;
  typedef itk::Image<PixelTypeRGB, 3>  ImageTypeRGB3;
  if (!(MRCImageIOTester< ImageTypeRGB3 >::Write(filePrefix, outputPath) ) )
    {
    std::cout << "[FAILED] writing (RGBPixel<unsighed char>)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (RGBPixel<unsigned char>)" << std::endl;
  if (!(MRCImageIOTester< ImageTypeRGB3 >::Read(filePrefix, outputPath, m_CallNumber) ) )
    {
    std::cout << "[FAILED] reading (RGBPixel<unsigned char>)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (RGBPixel<unsigned char>)" << std::endl;

  // complex<float>
  typedef std::complex<float>                  PixelTypeComplexFloat;
  typedef itk::Image<PixelTypeComplexFloat, 3> ImageTypeComplexFloat3;
  if (!(MRCImageIOTester< ImageTypeComplexFloat3 >::Write(filePrefix, outputPath) ) )
    {
    std::cout << "[FAILED] writing (complex<float>)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (complex<float>)" << std::endl;
  if (!(MRCImageIOTester< ImageTypeComplexFloat3 >::Read(filePrefix, outputPath, m_CallNumber) ) )
    {
    std::cout << "[FAILED] reading (complex<float>)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (complex<float>)" << std::endl;

  //
  // test additional usable dimensions
  //

  // 1D
  typedef itk::Image<unsigned char, 1> ImageTypeUnsignedChar1;
  if (!(MRCImageIOTester< ImageTypeUnsignedChar1 >::Write(filePrefix, outputPath) ) )
    {
    std::cout << "[FAILED] writing (1D)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (1D)" << std::endl;
  if (!(MRCImageIOTester< ImageTypeUnsignedChar1 >::Read(filePrefix, outputPath, m_CallNumber) ) )
    {
    std::cout << "[FAILED] reading (1D)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (1D)" << std::endl;

  // 2D
  typedef itk::Image<unsigned char, 2> ImageTypeUnsignedChar2;
  if (!(MRCImageIOTester< ImageTypeUnsignedChar2 >::Write(filePrefix, outputPath) ) )
    {
    std::cout << "[FAILED] writing (2D)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (2D)" << std::endl;
  if (!(MRCImageIOTester< ImageTypeUnsignedChar2 >::Read(filePrefix, outputPath, m_CallNumber) ) )
    {
    std::cout << "[FAILED] reading (2D)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (2D)" << std::endl;

  //
  // expect exceptions with the following
  //
  typedef itk::Image<double> ImageTypeDouble;
  if (MRCImageIOTester< ImageTypeDouble >::Write(filePrefix, outputPath) )
    {
    std::cout << "[FAILED] didn't throw exception (Image<double>)"
              << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] threw exception (Image<double>)" << std::endl;

  typedef itk::Image<int> ImageTypeInt;
  if (MRCImageIOTester< ImageTypeInt >::Write(filePrefix, outputPath) )
    {
    std::cout << "[FAILED] didn't throw exception (Image<int>)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] threw exception (Image<int>)" << std::endl;

  typedef itk::Image<unsigned long> ImageTypeUnsignedLong;
  if (MRCImageIOTester< ImageTypeUnsignedLong >::Write(filePrefix, outputPath) )
    {
    std::cout << "[FAILED] didn't throw exception (Image<unsigned long>)"
              << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] threw exception (Image<unsigned long>)" << std::endl;

  //
  // test unusable dimensions
  //
  typedef itk::Image<unsigned char, 4> ImageTypeUnsignedChar4;
  if (MRCImageIOTester< ImageTypeUnsignedChar4 >::Write(filePrefix, outputPath) )
    {
    std::cout << "[FAILED] incorrectly returned true (4D)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] threw exception (4D)" << std::endl;

  typedef itk::Image<unsigned char, 5> ImageTypeUnsignedChar5;
  if (MRCImageIOTester< ImageTypeUnsignedChar5 >::Write(filePrefix, outputPath) )
    {
    std::cout << "[FAILED] incorrectly returned true (5D)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] threw exception (5D)" << std::endl;

  //
  // use print methods
  //
  typedef itk::MRCImageIO IOType;
  IOType::Pointer mrcIO = IOType::New();

  EXERCISE_BASIC_OBJECT_METHODS( mrcIO, MRCImageIO, StreamingImageIOBase );

  //
  // All tests succeeded
  //
  return EXIT_SUCCESS;
}
