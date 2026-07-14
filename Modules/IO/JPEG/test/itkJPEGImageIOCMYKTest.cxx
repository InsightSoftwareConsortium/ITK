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

#include "itkJPEGImageIO.h"
#include "itkImageFileReader.h"
#include "itkRGBPixel.h"
#include "itkTestingMacros.h"

int
itkJPEGImageIOCMYKTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension{ 2 };
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, Dimension>;

  {
    const itk::JPEGImageIO::Pointer io = itk::JPEGImageIO::New();

    const itk::ImageFileReader<ImageType>::Pointer reader = itk::ImageFileReader<ImageType>::New();

    reader->SetFileName(argv[1]);

    reader->SetImageIO(io);

    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

    ITK_TEST_EXPECT_TRUE(io->GetPixelType() == itk::CommonEnums::IOPixel::RGB);
  }

  {
    const itk::JPEGImageIO::Pointer io = itk::JPEGImageIO::New();

    const itk::ImageFileReader<ImageType>::Pointer reader = itk::ImageFileReader<ImageType>::New();

    auto cmykToRGB = false;
    ITK_TEST_SET_GET_BOOLEAN(io, CMYKtoRGB, cmykToRGB);

    reader->SetFileName(argv[1]);

    reader->SetImageIO(io);

    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

    ITK_TEST_EXPECT_TRUE(io->GetPixelType() == itk::CommonEnums::IOPixel::VECTOR);
  }

  {
    // Pin actual color values, not just pixel type: a CMYK/YCCK conversion
    // defect can invert colors while every type check above still passes.
    // Expected values (row 100, columns 0-3) cross-checked against Pillow's
    // independent CMYK/YCCK decoder. Tolerance allows for lossy-JPEG DCT
    // rounding differences across libjpeg builds.
    using RGBImageType = itk::Image<itk::RGBPixel<unsigned char>, Dimension>;
    const itk::ImageFileReader<RGBImageType>::Pointer reader = itk::ImageFileReader<RGBImageType>::New();
    reader->SetFileName(argv[1]);
    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

    const RGBImageType::Pointer image = reader->GetOutput();
    const unsigned char         expected[4][3]{ { 55, 96, 16 }, { 53, 90, 16 }, { 55, 92, 15 }, { 56, 94, 15 } };
    constexpr int               tolerance = 5;
    for (int x = 0; x < 4; ++x)
    {
      const RGBImageType::IndexType      idx{ { x, 100 } };
      const itk::RGBPixel<unsigned char> pixel = image->GetPixel(idx);
      for (unsigned int c = 0; c < 3; ++c)
      {
        const int diff = static_cast<int>(pixel[c]) - static_cast<int>(expected[x][c]);
        ITK_TEST_EXPECT_TRUE(diff >= -tolerance && diff <= tolerance);
      }
    }
  }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
