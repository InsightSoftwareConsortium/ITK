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

#include <iostream>
#include <algorithm>
#include "itkPNGImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"


// Specific ImageIO test

namespace
{
int
CompareExtensions(itk::ImageIOBase::ArrayOfExtensionsType & a1, itk::ImageIOBase::ArrayOfExtensionsType & a2)
{
  std::sort(a1.begin(), a1.end());
  std::sort(a2.begin(), a2.end());
  if (a1 == a2)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}
} // end anonymous namespace

int
itkPNGImageIOTest2(int argc, char * argv[])
{
  // Test the reading of an image as grayscale image and writing of grayscale image
  if (argc < 5)
  {
    std::cerr << "Missing parameters." << '\n';
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input"
              << " output"
              << " useCompression"
              << " compressionLevel"
              << " expandRGBPalette" << '\n';
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = unsigned char;

  // We are converting read data into grayscale pixel image
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;


  // Read the input image
  const itk::PNGImageIO::Pointer io = itk::PNGImageIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(io, PNGImageIO, ImageIOBase);

  // Exercise exception cases
  const size_t sizeOfActualIORegion =
    io->GetIORegion().GetNumberOfPixels() * (io->GetComponentSize() * io->GetNumberOfComponents());
  auto * loadBuffer = new char[sizeOfActualIORegion];

  ITK_TRY_EXPECT_EXCEPTION(io->Read(loadBuffer));


  auto useCompression = static_cast<bool>(argv[3]);
  ITK_TEST_SET_GET_BOOLEAN(io, UseCompression, useCompression);

  const int compressionLevel = std::stoi(argv[4]);
  io->SetCompressionLevel(compressionLevel);
  ITK_TEST_SET_GET_VALUE(compressionLevel, io->GetCompressionLevel());

  auto expandRGBPalette = static_cast<bool>(std::stoi(argv[5]));
  ITK_TEST_SET_GET_BOOLEAN(io, ExpandRGBPalette, expandRGBPalette);

  if (io->CanReadFile(""))
  {
    std::cerr << "Test failed!" << '\n';
    std::cout << "No filename specified." << '\n';
    std::cout << "CanReadFile: "
              << "Expected false but got true" << '\n';
  }

  if (io->CanStreamRead())
  {
    std::cout << "itk::PNGImageIO can stream read" << '\n';
  }
  else
  {
    std::cout << "itk::PNGImageIO cannot stream read" << '\n';
  }

  // Check supported file extensions
  // Expecting ".png" and ".PNG"
  itk::ImageIOBase::ArrayOfExtensionsType expectedExtensions;
  expectedExtensions.push_back(".png");
  expectedExtensions.push_back(".PNG");

  // Read extensions
  itk::ImageIOBase::ArrayOfExtensionsType readExtensions = io->GetSupportedReadExtensions();
  if (CompareExtensions(readExtensions, expectedExtensions))
  {
    std::cout << "Test failed!" << '\n';
    std::cerr << "Unexpected list of supported read extension." << '\n';
    return EXIT_FAILURE;
  }
  // Write extensions
  itk::ImageIOBase::ArrayOfExtensionsType writeExtensions = io->GetSupportedWriteExtensions();
  if (CompareExtensions(writeExtensions, expectedExtensions))
  {
    std::cout << "Test failed!" << '\n';
    std::cerr << "Unexpected list of supported write extension." << '\n';
    return EXIT_FAILURE;
  }

  if (!io->SupportsDimension(Dimension))
  {
    std::cerr << "Test failed!" << '\n';
    std::cerr << "itk::PNGImageIO does not support dimension: " << Dimension << '\n';
    return EXIT_FAILURE;
  }

  if (!io->CanReadFile(argv[1]))
  {
    std::cerr << "Test failed!" << '\n';
    std::cout << "itk::PNGImageIO cannot read file " << io->GetFileName() << '\n';
    return EXIT_FAILURE;
  }

  // Actually reading an RGBA image
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->SetImageIO(io);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  if (io->GetExpandRGBPalette())
  {
    std::cout << "If palette image, expanding to RGB. " << '\n';
  }
  else
  {
    std::cout << "If palette image, trying to read as scalar. " << '\n';
  }

  if (!io->GetExpandRGBPalette() && io->GetIsReadAsScalarPlusPalette())
  {
    std::cout << "Image read as Scalar." << '\n';
    itk::PNGImageIO::PaletteType palette = io->GetColorPalette();
    std::cout << "PaletteType: " << '\n';
    for (unsigned int i = 0; i < palette.size(); ++i)
    {
      std::cout << '[' << i << "]:" << palette[i] << '\n';
    }
  }
  else
  {
    std::cout << "Image read as grayscale." << '\n';
  }


  // Exercise other methods
  const itk::ImageIOBase::SizeType pixelStride = io->GetPixelStride();
  std::cout << "PixelStride: " << itk::NumericTraits<itk::ImageIOBase::SizeType>::PrintType(pixelStride) << '\n';


  const ImageType::Pointer inputImage = reader->GetOutput();

  // Write the grayscale output image
  auto writer = WriterType::New();
  writer->SetInput(inputImage);
  writer->SetImageIO(io);
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // Release memory
  delete[] loadBuffer;

  std::cout << "Test finished" << '\n';
  return EXIT_SUCCESS;
}
