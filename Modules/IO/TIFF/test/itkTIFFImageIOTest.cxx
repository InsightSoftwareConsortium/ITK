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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTIFFImageIO.h"
#include "itkTestingMacros.h"
#include <fstream>

namespace
{

template <typename TImage>
bool
TestMultipleReads(const std::string & fname, TImage *)
{
  using ImageType = TImage;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();

  itk::TIFFImageIO::Pointer io = itk::TIFFImageIO::New();
  reader->SetFileName(fname.c_str());
  reader->SetImageIO(io);

  try
  {
    reader->GetOutput()->SetRequestedRegionToLargestPossibleRegion();
    reader->GetOutput()->UpdateOutputInformation();
    reader->GetOutput()->PropagateRequestedRegion();
    reader->GetOutput()->UpdateOutputData();
    reader->GetOutput()->ReleaseData();
    reader->GetOutput()->UpdateOutputData();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "exception in file reader for bug  " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
  }

  return true;
}

// Specific ImageIO test

template <typename TImage>
int
itkTIFFImageIOTestHelper(int, char * argv[])
{
  using ImageType = TImage;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  itk::TIFFImageIO::Pointer io = itk::TIFFImageIO::New();
  reader->SetFileName(argv[1]);
  reader->SetImageIO(io);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  // Test 2 reads with only one ReadImageInformation
  TestMultipleReads<ImageType>(argv[1], nullptr);


  typename ImageType::Pointer image = reader->GetOutput();

  image->Print(std::cout);

  typename ImageType::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region << std::endl;

  // Generate test image
  writer->SetInput(reader->GetOutput());
  writer->SetFileName(argv[2]);
  writer->SetImageIO(io);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
} // namespace

int
itkTIFFImageIOTest(int argc, char * argv[])
{

  unsigned int dimension = 2;
  unsigned int pixelType = 1;

  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " Input Output [dimensionality (default: 2)]"
              << "[pixeltype: 1:RBG<char>(default); 2:RBG<ushort>; 3:short; 4:float; 5:ushort]" << std::endl;
    return EXIT_FAILURE;
  }
  else if (argc == 4)
  {
    dimension = std::stoi(argv[3]);
  }
  else if (argc == 5)
  {
    dimension = std::stoi(argv[3]);
    pixelType = std::stoi(argv[4]);
  }

  if (dimension == 2 && pixelType == 1)
  {
    using PixelType = itk::RGBPixel<unsigned char>;
    return itkTIFFImageIOTestHelper<itk::Image<PixelType, 2>>(argc, argv);
  }
  else if (dimension == 2 && pixelType == 2)
  {
    using PixelType = itk::RGBPixel<unsigned short>;
    return itkTIFFImageIOTestHelper<itk::Image<PixelType, 2>>(argc, argv);
  }
  else if (dimension == 2 && pixelType == 3)
  {
    using PixelType = itk::RGBPixel<short>;
    return itkTIFFImageIOTestHelper<itk::Image<PixelType, 2>>(argc, argv);
  }
  else if (dimension == 2 && pixelType == 5)
  {
    return itkTIFFImageIOTestHelper<itk::Image<unsigned short, 2>>(argc, argv);
  }
  else if (dimension == 3 && pixelType == 1)
  {
    return itkTIFFImageIOTestHelper<itk::Image<unsigned char, 3>>(argc, argv);
  }
  else if (dimension == 3 && pixelType == 5)
  {
    return itkTIFFImageIOTestHelper<itk::Image<unsigned short, 3>>(argc, argv);
  }
  else if (dimension == 3 && pixelType == 3)
  {
    return itkTIFFImageIOTestHelper<itk::Image<short, 3>>(argc, argv);
  }
  else if (dimension == 3 && pixelType == 4)
  {
    return itkTIFFImageIOTestHelper<itk::Image<float, 3>>(argc, argv);
  }
  else if (dimension == 4 && pixelType == 5)
  {
    return itkTIFFImageIOTestHelper<itk::Image<unsigned short, 4>>(argc, argv);
  }
  else if (dimension == 4 && pixelType == 3)
  {
    itk::Image<short, 4>::Pointer dummy;
    return itkTIFFImageIOTestHelper<itk::Image<short, 4>>(argc, argv);
  }
  else if (dimension == 4 && pixelType == 4)
  {
    return itkTIFFImageIOTestHelper<itk::Image<float, 4>>(argc, argv);
  }
  else
  {
    std::cerr << "Test failed!" << itkNameOfTestExecutableMacro(argv) << std::endl;
    std::cerr << " Unsupported dimensionality or pixelType provided." << std::endl;
    std::cerr << " Supported dimensionality: [2-4]; (default: 2)" << std::endl;
    std::cerr << " Supported pixelType: [1:uchar(default); 2:ushort; 3:short; 4:float]" << std::endl;
    return EXIT_FAILURE;
  }
}
