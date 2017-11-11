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
#include "itkVTKImageIO.h"

#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkTestingMacros.h"
#include "itkMath.h"


static unsigned int m_CallNumber;

const unsigned int TEST_VECTOR_PIXEL_DIM = 3;

template <typename TPixelType, unsigned int VImageDimension>
class VTKImageIOTester
{
public:

  virtual int Test(int argc, char* argv[] );

  virtual ~VTKImageIOTester(){};

  static std::string
  SetupFileName( const std::string &filePrefix,
                 const std::string &fileExtension,
                 std::string &outputPath )
    {
    std::ostringstream m_NameWithIndex;
    m_NameWithIndex << filePrefix << "_" << m_CallNumber << "." << fileExtension;

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

    return m_OutputFileName.str();
    }

  static bool
  Write( const std::string &filePrefix,
         std::string &outputPath,
         bool ascii )
    {
    try
      {
      ++m_CallNumber;

      typedef TPixelType                              PixelType;
      typedef itk::Image<PixelType,VImageDimension>   ImageType;


      // force use of VTKImageIO
      typedef itk::VTKImageIO IOType;
      IOType::Pointer vtkIO = IOType::New();
      if (ascii)
        {
        vtkIO->SetFileTypeToASCII();
        }
      else
        {
        vtkIO->SetFileTypeToBinary();
        }

      typedef itk::ImageFileWriter<ImageType> ImageFileWriterType;
      typename ImageFileWriterType::Pointer writer = ImageFileWriterType::New();
      writer->SetImageIO( vtkIO );

      // allocate an 10x10x10 image
      typename ImageType::Pointer image = ImageType::New();
      typename ImageType::SizeType imageSize;
      imageSize.Fill(10);
      image->SetRegions( imageSize );
      image->Allocate();

      unsigned int cnt = 0;
      itk::ImageRegionIterator< ImageType > i( image, image->GetLargestPossibleRegion() );
      i.GoToBegin();
      while (! i.IsAtEnd() )
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
            i.Set( itk::NumericTraits<PixelType>::OneValue() );
            break;
          case 3:
            i.Set( itk::NumericTraits<PixelType>::ZeroValue() );
          }
        ++cnt;
        ++i;
        }

      writer->SetInput( image );

      std::string m_OutputFileName =
        VTKImageIOTester<char,3>::SetupFileName(filePrefix, "vtk", outputPath);

      writer->SetFileName( m_OutputFileName );
      writer->Update();

      // test the CanWriteFile function after the fact (should always
      // be true at this point)
      if (!vtkIO->CanWriteFile(m_OutputFileName.c_str()))
        {
        return false;
        }

      return true;

      }
    catch (itk::ExceptionObject &e)
      {
      std::cout << e << std::endl;
      return false;
      }
    }

  static bool
  Read( const std::string &filePrefix,
        std::string &outputPath,
        bool ascii )
    {
    try
      {
      typedef TPixelType                              PixelType;
      typedef itk::Image<PixelType,VImageDimension>   ImageType;

      typedef itk::ImageFileReader<ImageType> ImageFileReaderType;
      typename ImageFileReaderType::Pointer reader = ImageFileReaderType::New();

      // force use of VTKImageIO
      typedef itk::VTKImageIO IOType;
      IOType::Pointer vtkIO = IOType::New();
      reader->SetImageIO(vtkIO);

      // set ascii or binary
      if (ascii)
        {
        vtkIO->SetFileTypeToASCII();
        }
      else
        {
        vtkIO->SetFileTypeToBinary();
        }

      std::string m_OutputFileName = VTKImageIOTester::SetupFileName( filePrefix, "vtk", outputPath );
      reader->SetFileName( m_OutputFileName );

      // Check that the correct content was written to the header.
      std::ifstream istrm( m_OutputFileName.c_str() );
      char firstline[25];
      istrm.getline( firstline, 24 );
      istrm.close();
      if( strncmp( firstline, "# vtk DataFile Version ", 24 ) != 0 )
        {
        std::cout << "Header string was not written properly." << std::endl;
        return false;
        }

      // read the image
      typename ImageType::Pointer image = reader->GetOutput();
      reader->Update();

      // test the CanReadFile function after the fact (should always
      // be true at this point)
      if (!vtkIO->CanReadFile(m_OutputFileName.c_str()))
        {
        return false;
        }

      // check the size
      typename ImageType::RegionType region =
        image->GetLargestPossibleRegion();
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
      bool pixelsGood = true;
      unsigned int cnt = 0;
      itk::ImageRegionIterator< ImageType > iter( image, region );
      iter.GoToBegin();

      while (! iter.IsAtEnd() && pixelsGood)
        {
        // check image switching between these pixels
        switch (cnt%4)
          {
          case 0:
// Comparison with complex???
            if (itk::Math::NotExactlyEquals(iter.Get(), itk::NumericTraits<PixelType>::ZeroValue()))
              {
              pixelsGood = false;
              }
            break;
          case 1:
            if (itk::Math::NotExactlyEquals(iter.Get(), itk::NumericTraits<PixelType>::OneValue()))
              {
              pixelsGood = false;
              }
            break;
          case 2:
            if (itk::Math::NotExactlyEquals(iter.Get(), itk::NumericTraits<PixelType>::OneValue()))
              {
              pixelsGood = false;
              }
            break;
          case 3:
            if (itk::Math::NotExactlyEquals(iter.Get(), itk::NumericTraits<PixelType>::ZeroValue()))
              {
              pixelsGood = false;
              }
            break;
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
      std::cout << e << std::endl;
      return false;
      }
    }

  static bool
  CanReadFileTest( const std::string &filePrefix,
                   const std::string &fileExtension,
                   std::string &outputPath )
    {
    typedef itk::VTKImageIO IOType;
    IOType::Pointer vtkIO = IOType::New();

    std::string fileName =
      VTKImageIOTester::SetupFileName(filePrefix, fileExtension, outputPath);

    return vtkIO->CanReadFile( fileName.c_str() );
    }

  static bool
  CanWriteFileTest( const std::string &filePrefix,
                    const std::string &fileExtension,
                    std::string &outputPath )
    {
    typedef itk::VTKImageIO IOType;
    IOType::Pointer vtkIO = IOType::New();

    std::string fileName =
      VTKImageIOTester::SetupFileName(filePrefix, fileExtension, outputPath);

    return vtkIO->CanWriteFile( fileName.c_str() );
    }
};

template<typename TScalar>
int Test1AsciiBinary(std::string filePrefix, std::string outputPath,
    std::string typeName, bool ascii, bool read = true)
{
  std::string ab;
  if (ascii)
    {
    ab = "ascii";
    }
  else
    {
    ab = "binary";
    }

  if (!(VTKImageIOTester<TScalar, 3>::Write( filePrefix, outputPath, true )))
    {
    std::cout << "[FAILED] writing " << typeName << " - " << ab << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing " << typeName << " - " << ab << std::endl;

  if (!read)
    {
    return EXIT_SUCCESS;
    }

  if (!(VTKImageIOTester<TScalar, 3>::Read( filePrefix, outputPath, true )))
    {
    std::cout << "[FAILED] reading " << typeName << " - " << ab << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading " << typeName << " - " << ab << std::endl;

  return EXIT_SUCCESS;
}


template<typename TScalar>
int Test1Type(std::string filePrefix, std::string outputPath,
    std::string typeName, bool read = true)
{
  int status = 0;
  status += Test1AsciiBinary<TScalar>(filePrefix, outputPath, typeName, true, read);
  status += Test1AsciiBinary<TScalar>(filePrefix, outputPath, typeName, false, read);
  return status;
}

int itkVTKImageIO2Test(int argc, char* argv[])
{

  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " outputPath" << std::endl;
    return EXIT_FAILURE;
    }

  std::string filePrefix = argv[0];
  std::string outputPath = argv[1];

  //
  // test all usable pixel types
  //

  int status = 0;

  status += Test1Type<unsigned char>(filePrefix, outputPath, "unsigned char");
  status += Test1Type<char>(filePrefix, outputPath, "char");
  status += Test1Type<unsigned short>(filePrefix, outputPath, "unsigned short");
  status += Test1Type<short>(filePrefix, outputPath, "short");
  status += Test1Type<unsigned int>(filePrefix, outputPath, "unsigned int");
  status += Test1Type<int>(filePrefix, outputPath, "int");
  status += Test1Type<unsigned long>(filePrefix, outputPath, "unsigned long");
  status += Test1Type<long>(filePrefix, outputPath, "long");
  status += Test1Type<unsigned long long>(filePrefix, outputPath, "unsigned long long");
  status += Test1Type<long long>(filePrefix, outputPath, "long long");

  status += Test1Type<float>(filePrefix, outputPath, "float");
  status += Test1Type<double>(filePrefix, outputPath, "double");
  status += Test1Type<itk::RGBPixel<unsigned char> >(filePrefix, outputPath, "RGBPixel<unsigned char>");
  status += Test1Type<itk::RGBAPixel<unsigned char> >(filePrefix, outputPath, "RGBAPixel<unsigned char>");
  status += Test1Type<itk::Vector<int, TEST_VECTOR_PIXEL_DIM> >(filePrefix, outputPath, "Vector<int>");
  status += Test1Type<itk::Vector<double, TEST_VECTOR_PIXEL_DIM> >(filePrefix, outputPath, "Vector<double>");
  status += Test1Type<itk::SymmetricSecondRankTensor<double, 3> >(filePrefix, outputPath, "SymmetricSecondRankTensor<double, 3>");
  std::cout << "Writing a 2 dimension tensor is possible, but reading is not." << std::endl;
  status += Test1Type<itk::SymmetricSecondRankTensor<double, 2> >(filePrefix, outputPath, "SymmetricSecondRankTensor<double, 2>", false);


  //
  // Test bad paths
  //

  // read bad file extension
  if ( VTKImageIOTester<char,3>::CanReadFileTest( filePrefix, "bad", outputPath ))
    {
    std::cout << "[FAILED] didn't properly reject bad file extension for reading" << std::endl;
    status++;
    }
  std::cout << "[PASSED] rejected bad file extension for reading" << std::endl;

  // read bad file name
  if ( VTKImageIOTester<char,3>::CanReadFileTest( "BadFile", "vtk", outputPath ))
    {
    std::cout << "[FAILED] didn't properly reject bad file name for reading" << std::endl;
    status++;
    }
  std::cout << "[PASSED] rejected bad file name for reading" << std::endl;

  // write bad file extension
  if ( VTKImageIOTester<char,3>::CanWriteFileTest( filePrefix, "bad", outputPath ))
    {
    std::cout << "[FAILED] didn't properly reject bad file extension for writing" << std::endl;
    status++;
    }
  std::cout << "[PASSED] rejected bad file extension for writing" << std::endl;

  // write bad file extension when the string ".vtk" is part of the prefix
  if ( VTKImageIOTester<char,3>::CanWriteFileTest( filePrefix + ".vtk", "bad", outputPath ))
    {
    std::cout << "[FAILED] didn't properly reject bad file extension for writing" << std::endl;
    status++;
    }
  std::cout << "[PASSED] rejected bad file extension for writing" << std::endl;

  //
  // use print methods
  //
  typedef itk::VTKImageIO IOType;
  IOType::Pointer vtkIO = IOType::New();

  EXERCISE_BASIC_OBJECT_METHODS( vtkIO, VTKImageIO, StreamingImageIOBase );

  std::cout << "All tests finished!" << std::endl;
  return status;
}
