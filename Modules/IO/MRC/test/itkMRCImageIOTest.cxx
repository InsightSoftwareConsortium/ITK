/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
  virtual int
  Test(int argc, char * argv[]);

  virtual ~MRCImageIOTester() = default;

  static bool
  Write(const std::string & filePrefix, std::string & outputPath);

  static bool
  Read(const std::string & filePrefix, std::string & outputPath, unsigned int index);
};

template <typename TImageType>
bool
MRCImageIOTester<TImageType>::Write(const std::string & filePrefix, std::string & outputPath)
{
  try
  {
    ++m_CallNumber;

    using ImageType = TImageType;
    using PixelType = typename ImageType::PixelType;

    // allocate an 10x10x10 image
    auto image = ImageType::New();
    auto m_ImageSize = ImageType::SizeType::Filled(10);
    image->SetRegions(m_ImageSize);
    image->Allocate();

    unsigned int                        cnt = 0;
    itk::ImageRegionIterator<ImageType> i(image, image->GetLargestPossibleRegion());
    i.GoToBegin();
    while (!i.IsAtEnd())
    {
      // fill the image switching between these pixels
      switch (cnt % 4)
      {
        case 0:
          i.Set(PixelType{});
          break;
        case 1:
          i.Set(itk::NumericTraits<PixelType>::OneValue());
          break;
        case 2:
          i.Set(itk::NumericTraits<PixelType>::min(PixelType()));
          break;
        case 3:
          i.Set(itk::NumericTraits<PixelType>::max(PixelType()));
      }
      ++cnt;
      ++i;
    }

    using ImageFileWriterType = itk::ImageFileWriter<ImageType>;
    auto writer = ImageFileWriterType::New();
    writer->SetInput(image);

    // force use of MRCImageIO
    using IOType = itk::MRCImageIO;
    auto mrcIO = IOType::New();
    writer->SetImageIO(mrcIO);

    std::ostringstream m_NameWithIndex;
    m_NameWithIndex << filePrefix << '_' << m_CallNumber << ".mrc";

    std::ostringstream m_OutputFileName;

#if defined(WIN32) // windows
    // if it ends in \\ just append the name
    if (outputPath.back() == '\\')
    {
      m_OutputFileName << outputPath << m_NameWithIndex.str();
    }
    else
    {
      m_OutputFileName << outputPath << '\\' << m_NameWithIndex.str();
    }

#else /// POSIX UNIX

    // if it ends in / just append the name
    if (outputPath.back() == '/')
    {
      m_OutputFileName << outputPath << m_NameWithIndex.str();
    }
    // otherwise, add / and the name
    else
    {
      m_OutputFileName << outputPath << '/' << m_NameWithIndex.str();
    }
#endif

    writer->SetFileName(m_OutputFileName.str());
    writer->Update();

    // test the CanWriteFile function after the fact (should always be true at
    // this point)
    if (!mrcIO->CanWriteFile(m_OutputFileName.str().c_str()))
    {
      return false;
    }

    return true;
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << e.GetDescription() << '\n';
    return false;
  }
}

template <typename TImageType>
bool
MRCImageIOTester<TImageType>::Read(const std::string & filePrefix, std::string & outputPath, unsigned int index)
{
  try
  {
    using ImageType = TImageType;
    using PixelType = typename ImageType::PixelType;

    using ImageFileReaderType = itk::ImageFileReader<ImageType>;
    auto reader = ImageFileReaderType::New();

    // force use of MRCImageIO
    using IOType = itk::MRCImageIO;
    auto mrcIO = IOType::New();
    reader->SetImageIO(mrcIO);

    // construct the image filename
    std::ostringstream m_NameWithIndex;
    m_NameWithIndex << filePrefix << '_' << index << ".mrc";

    std::ostringstream m_OutputFileName;

#if defined(WIN32) // windows
    // if it ends in \\ just append the name
    if (outputPath.back() == '\\')
    {
      m_OutputFileName << outputPath << m_NameWithIndex.str();
    }
    else
    {
      m_OutputFileName << outputPath << '\\' << m_NameWithIndex.str();
    }

#else /// POSIX UNIX

    // if it ends in / just append the name
    if (outputPath.back() == '/')
    {
      m_OutputFileName << outputPath << m_NameWithIndex.str();
    }
    // otherwise, add / and the name
    else
    {
      m_OutputFileName << outputPath << '/' << m_NameWithIndex.str();
    }
#endif
    reader->SetFileName(m_OutputFileName.str());

    // read the image
    const typename ImageType::Pointer image = reader->GetOutput();
    reader->Update();

    // test the CanReadFile function after the fact (should always be true at
    // this point)
    if (!mrcIO->CanReadFile(m_OutputFileName.str().c_str()))
    {
      return false;
    }

    // check the size
    const typename ImageType::RegionType region = image->GetLargestPossibleRegion();
    typename ImageType::SizeType         size = region.GetSize();
    bool                                 sizeGood = true;
    for (unsigned int i = 0; i < ImageType::GetImageDimension(); ++i)
    {
      if (size[i] != 10)
      {
        sizeGood = false;
        break;
      }
    }
    if (!sizeGood)
    {
      std::cout << "Error: Size didn't read properly" << '\n';
      return false;
    }

    // check each pixel
    bool                                pixelsGood = true;
    unsigned int                        cnt = 0;
    itk::ImageRegionIterator<ImageType> iter(image, region);
    iter.GoToBegin();
    while (!iter.IsAtEnd() && pixelsGood)
    {
      // check image switching between these pixels
      switch (cnt % 4)
      {
        case 0:
          if (itk::Math::NotExactlyEquals(iter.Get(), PixelType{}))
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
          if (itk::Math::NotExactlyEquals(iter.Get(), itk::NumericTraits<PixelType>::min(PixelType())))
          {
            pixelsGood = false;
          }
          break;
        case 3:
          if (itk::Math::NotExactlyEquals(iter.Get(), itk::NumericTraits<PixelType>::max(PixelType())))
          {
            pixelsGood = false;
          }
      }
      ++cnt;
      ++iter;
    }

    if (!pixelsGood)
    {
      std::cout << "Error: Pixels didn't read properly" << '\n';
      return false;
    }

    // reading successful, so return true
    return true;
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << e.GetDescription() << '\n';
    return false;
  }
}

// int MRCImageIOTester::Test(int argc, char* argv[] )
int
itkMRCImageIOTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Missing Parameters." << '\n';
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputPath" << '\n';
    return EXIT_FAILURE;
  }

  std::string       outputPath = argv[1];
  const std::string filePrefix = argv[0];

  //
  // test all usable pixeltypes
  //

  // unsigned char
  using ImageTypeUnsignedChar3 = itk::Image<unsigned char, 3>;
  if (!(MRCImageIOTester<ImageTypeUnsignedChar3>::Write(filePrefix, outputPath)))
  {
    std::cout << "[FAILED] writing (unsighed char)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] writing (unsighed char)" << '\n';
  if (!(MRCImageIOTester<ImageTypeUnsignedChar3>::Read(filePrefix, outputPath, m_CallNumber)))
  {
    std::cout << "[FAILED] reading (unsighed char)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] reading (unsighed char)" << '\n';

  // short
  using ImageTypeShort3 = itk::Image<short, 3>;
  if (!(MRCImageIOTester<ImageTypeShort3>::Write(filePrefix, outputPath)))
  {
    std::cout << "[FAILED] writing (short)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] writing (short)" << '\n';
  if (!(MRCImageIOTester<ImageTypeShort3>::Read(filePrefix, outputPath, m_CallNumber)))
  {
    std::cout << "[FAILED] reading (short)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] reading (short)" << '\n';

  // float
  using ImageTypeFloat3 = itk::Image<float, 3>;
  if (!(MRCImageIOTester<ImageTypeFloat3>::Write(filePrefix, outputPath)))
  {
    std::cout << "[FAILED] writing (float)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] writing (float)" << '\n';
  if (!(MRCImageIOTester<ImageTypeFloat3>::Read(filePrefix, outputPath, m_CallNumber)))
  {
    std::cout << "[FAILED] reading (float)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] reading (float)" << '\n';

  // unsigned short
  using ImageTypeUnsignedShort3 = itk::Image<unsigned short, 3>;
  if (!(MRCImageIOTester<ImageTypeUnsignedShort3>::Write(filePrefix, outputPath)))
  {
    std::cout << "[FAILED] writing (unsighed short)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] writing (unsighed short)" << '\n';
  if (!(MRCImageIOTester<ImageTypeUnsignedShort3>::Read(filePrefix, outputPath, m_CallNumber)))
  {
    std::cout << "[FAILED] reading (unsighed short)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] reading (unsighed short)" << '\n';

  // RGBPixel<unsigned char>
  using PixelTypeRGB = itk::RGBPixel<unsigned char>;
  using ImageTypeRGB3 = itk::Image<PixelTypeRGB, 3>;
  if (!(MRCImageIOTester<ImageTypeRGB3>::Write(filePrefix, outputPath)))
  {
    std::cout << "[FAILED] writing (RGBPixel<unsighed char>)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] writing (RGBPixel<unsigned char>)" << '\n';
  if (!(MRCImageIOTester<ImageTypeRGB3>::Read(filePrefix, outputPath, m_CallNumber)))
  {
    std::cout << "[FAILED] reading (RGBPixel<unsigned char>)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] reading (RGBPixel<unsigned char>)" << '\n';

  // complex<float>
  using PixelTypeComplexFloat = std::complex<float>;
  using ImageTypeComplexFloat3 = itk::Image<PixelTypeComplexFloat, 3>;
  if (!(MRCImageIOTester<ImageTypeComplexFloat3>::Write(filePrefix, outputPath)))
  {
    std::cout << "[FAILED] writing (complex<float>)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] writing (complex<float>)" << '\n';
  if (!(MRCImageIOTester<ImageTypeComplexFloat3>::Read(filePrefix, outputPath, m_CallNumber)))
  {
    std::cout << "[FAILED] reading (complex<float>)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] reading (complex<float>)" << '\n';

  //
  // test additional usable dimensions
  //

  // 1D
  using ImageTypeUnsignedChar1 = itk::Image<unsigned char, 1>;
  if (!(MRCImageIOTester<ImageTypeUnsignedChar1>::Write(filePrefix, outputPath)))
  {
    std::cout << "[FAILED] writing (1D)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] writing (1D)" << '\n';
  if (!(MRCImageIOTester<ImageTypeUnsignedChar1>::Read(filePrefix, outputPath, m_CallNumber)))
  {
    std::cout << "[FAILED] reading (1D)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] reading (1D)" << '\n';

  // 2D
  using ImageTypeUnsignedChar2 = itk::Image<unsigned char, 2>;
  if (!(MRCImageIOTester<ImageTypeUnsignedChar2>::Write(filePrefix, outputPath)))
  {
    std::cout << "[FAILED] writing (2D)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] writing (2D)" << '\n';
  if (!(MRCImageIOTester<ImageTypeUnsignedChar2>::Read(filePrefix, outputPath, m_CallNumber)))
  {
    std::cout << "[FAILED] reading (2D)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] reading (2D)" << '\n';

  //
  // expect exceptions with the following
  //
  using ImageTypeDouble = itk::Image<double>;
  if (MRCImageIOTester<ImageTypeDouble>::Write(filePrefix, outputPath))
  {
    std::cout << "[FAILED] didn't throw exception (Image<double>)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] threw exception (Image<double>)" << '\n';

  using ImageTypeInt = itk::Image<int>;
  if (MRCImageIOTester<ImageTypeInt>::Write(filePrefix, outputPath))
  {
    std::cout << "[FAILED] didn't throw exception (Image<int>)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] threw exception (Image<int>)" << '\n';

  using ImageTypeUnsignedLong = itk::Image<unsigned long>;
  if (MRCImageIOTester<ImageTypeUnsignedLong>::Write(filePrefix, outputPath))
  {
    std::cout << "[FAILED] didn't throw exception (Image<unsigned long>)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] threw exception (Image<unsigned long>)" << '\n';

  //
  // test unusable dimensions
  //
  using ImageTypeUnsignedChar4 = itk::Image<unsigned char, 4>;
  if (MRCImageIOTester<ImageTypeUnsignedChar4>::Write(filePrefix, outputPath))
  {
    std::cout << "[FAILED] incorrectly returned true (4D)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] threw exception (4D)" << '\n';

  using ImageTypeUnsignedChar5 = itk::Image<unsigned char, 5>;
  if (MRCImageIOTester<ImageTypeUnsignedChar5>::Write(filePrefix, outputPath))
  {
    std::cout << "[FAILED] incorrectly returned true (5D)" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] threw exception (5D)" << '\n';


  // Exercise other methods to improve coverage
  using IOType = itk::MRCImageIO;
  auto mrcIO = IOType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(mrcIO, MRCImageIO, StreamingImageIOBase);


  // Not used; empty method body; called for coverage purposes
  mrcIO->WriteImageInformation();

  std::cout << "Test finished." << '\n';
  return EXIT_SUCCESS;
}
