/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRCImageIOTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkRGBPixel.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkMRCImageIO.h"


namespace MRCImageIOTest {

class MRCImageIOTester
{
  
public:
  virtual int Test(int argc, char* argv[] );
  
  virtual ~MRCImageIOTester(){};
  
protected:
  
  static unsigned int m_CallNumber;

  template <typename TImageType>
  bool WriteMRCImage( const std::string &filePrefix, std::string &outputPath );
  
  template <typename TImageType>
  bool ReadMRCImage( const std::string &filePrefix, 
                     std::string &outputPath, 
                     unsigned int index );
};

template <typename TImageType>
bool MRCImageIOTester::WriteMRCImage( const std::string &filePrefix, std::string &outputPath )  
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
    itk::ImageRegionIterator< ImageType > 
            i( image, image->GetLargestPossibleRegion() );
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
          i.Set( itk::NumericTraits<PixelType>::min( PixelType()) );
          break;
        case 3:
          i.Set( itk::NumericTraits<PixelType>::max( PixelType()) );
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
    
    // test the CanWriteFile function after the fact (should always be true at this point)
    if (!mrcIO->CanWriteFile(m_OutputFileName.str().c_str()))
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
bool MRCImageIOTester::ReadMRCImage( const std::string &filePrefix, 
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
    
    // test the CanReadFile function after the fact (should always be true at this point)
    if (!mrcIO->CanReadFile(m_OutputFileName.str().c_str()))
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
          if (iter.Get() != itk::NumericTraits<PixelType>::ZeroValue())
            {
            pixelsGood = false;
            }
          break;
        case 1:
          if (iter.Get() != itk::NumericTraits<PixelType>::OneValue())
            {
            pixelsGood = false;
            }
          break;
        case 2:
          if (iter.Get() != itk::NumericTraits<PixelType>::min( PixelType()))
            {
            pixelsGood = false;
            }
          break;
        case 3:
          if (iter.Get() != itk::NumericTraits<PixelType>::max( PixelType()))
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
    

    // reading successfull, so return true
    return true;
    
    }
  catch(itk::ExceptionObject &e)
    {
    std::cout << e.GetDescription() << std::endl;
    return false;
    }
}


int MRCImageIOTester::Test(int argc, char* argv[] )
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
  if (!(this->WriteMRCImage< itk::Image<unsigned char, 3> >(filePrefix, outputPath)))
    {
    std::cout << "[FAILED] writing (unsighed char)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (unsighed char)" << std::endl;
  if (!(this->ReadMRCImage< itk::Image<unsigned char, 3> >(filePrefix, outputPath, m_CallNumber)))
    {
    std::cout << "[FAILED] reading (unsighed char)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (unsighed char)" << std::endl;
  
  // short
  if (!(this->WriteMRCImage< itk::Image<short, 3> >(filePrefix, outputPath)))
    {
    std::cout << "[FAILED] writing (short)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (short)" << std::endl;
  if (!(this->ReadMRCImage< itk::Image<short, 3> >(filePrefix, outputPath, m_CallNumber)))
    {
    std::cout << "[FAILED] reading (short)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (short)" << std::endl;
  
  // float
  if (!(this->WriteMRCImage< itk::Image<float, 3> >(filePrefix, outputPath)))
    {
    std::cout << "[FAILED] writing (float)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (float)" << std::endl;
  if (!(this->ReadMRCImage< itk::Image<float, 3> >(filePrefix, outputPath, m_CallNumber)))
    {
    std::cout << "[FAILED] reading (float)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (float)" << std::endl;
  
  // unsigned short
  if (!(this->WriteMRCImage< itk::Image<unsigned short, 3> >(filePrefix, outputPath)))
    {
    std::cout << "[FAILED] writing (unsighed short)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (unsighed short)" << std::endl;
  if (!(this->ReadMRCImage< itk::Image<unsigned short, 3> >(filePrefix, outputPath, m_CallNumber)))
    {
    std::cout << "[FAILED] reading (unsighed short)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (unsighed short)" << std::endl;
  
  // RGBPixel<unsigned char>
  if (!(this->WriteMRCImage< itk::Image<itk::RGBPixel<unsigned char>,3> >(filePrefix, outputPath)))
    {
    std::cout << "[FAILED] writing (RGBPixel<unsighed char>)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (RGBPixel<unsigned char>)" << std::endl;
  if (!(this->ReadMRCImage< itk::Image<itk::RGBPixel<unsigned char>,3> >(filePrefix, outputPath, m_CallNumber)))
    {
    std::cout << "[FAILED] reading (RGBPixel<unsigned char>)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (RGBPixel<unsigned char>)" << std::endl;
  
  // complex<float>
  if (!(this->WriteMRCImage< itk::Image<std::complex<float>, 3 > >(filePrefix, outputPath)))
    {
    std::cout << "[FAILED] writing (complex<float>)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (complex<float>)" << std::endl;
  if (!(this->ReadMRCImage< itk::Image<std::complex<float>, 3> >(filePrefix, outputPath, m_CallNumber)))
    {
    std::cout << "[FAILED] reading (complex<float>)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (complex<float>)" << std::endl;
  
  
  //
  // test additional usable dimensions
  //
  
  // 1D
  if (!(this->WriteMRCImage< itk::Image<unsigned char, 1> >(filePrefix, outputPath)))
    {
    std::cout << "[FAILED] writing (1D)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (1D)" << std::endl;
  if (!(this->ReadMRCImage< itk::Image<unsigned char, 1> >(filePrefix, outputPath, m_CallNumber)))
    {
    std::cout << "[FAILED] reading (1D)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (1D)" << std::endl;
  
  // 2D
  if (!(this->WriteMRCImage< itk::Image<unsigned char, 2> >(filePrefix, outputPath)))
    {
    std::cout << "[FAILED] writing (2D)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] writing (2D)" << std::endl;
  if (!(this->ReadMRCImage< itk::Image<unsigned char, 2> >(filePrefix, outputPath, m_CallNumber)))
    {
    std::cout << "[FAILED] reading (2D)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] reading (2D)" << std::endl;
  
  
  //
  // expect exceptions with the following
  //
  if (this->WriteMRCImage< itk::Image<double> >(filePrefix, outputPath))
    {
    std::cout << "[FAILED] didn't throw exception (Image<double>)" 
              << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] threw exception (Image<double>)" << std::endl;
  
  if (this->WriteMRCImage< itk::Image<int> >(filePrefix, outputPath))
    {
    std::cout << "[FAILED] didn't throw exception (Image<int>)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] threw exception (Image<int>)" << std::endl;
  
  if (this->WriteMRCImage< itk::Image<unsigned long> >(filePrefix, outputPath))
    {
    std::cout << "[FAILED] didn't throw exception (Image<unsigned long>)" 
              << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] threw exception (Image<unsigned long>)" << std::endl;
  
  
  //
  // test unusable dimensions
  //
  if (this->WriteMRCImage< itk::Image<unsigned char, 4> >(filePrefix, outputPath))
    {
    std::cout << "[FAILED] incorrectly returned true (4D)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] threw exception (4D)" << std::endl;
  
  if (this->WriteMRCImage< itk::Image<unsigned char, 5> >(filePrefix, outputPath))
    {
    std::cout << "[FAILED] incorrectly returned true (5D)" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] threw exception (5D)" << std::endl;
  
  
  //
  // All tests succeeded
  //
  return EXIT_SUCCESS;
}

unsigned int MRCImageIOTester::m_CallNumber = 0;

} // end namespace MRCImageIOTest


int itkMRCImageIOTest(int argc, char* argv[])
{

  MRCImageIOTest::MRCImageIOTester test;
  return test.Test(argc, argv);
}
