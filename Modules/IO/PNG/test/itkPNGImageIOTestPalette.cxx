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
#include "itkMetaDataDictionary.h"
#include "itkTestingMacros.h"


// Specific ImageIO test

int
itkPNGImageIOTestPalette(int argc, char * argv[])
{
  if (argc != 5)
  {
    std::cerr << "Missing parameters." << '\n';
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input"
              << " output"
              << " expandRGBPalette"
              << " isPaletteImage" << '\n';
    return EXIT_FAILURE;
  }

  const auto expandRGBPalette = static_cast<bool>(std::stoi(argv[3]));
  const auto isPaletteImage = static_cast<bool>(std::stoi(argv[4]));
  std::cout << "Expanding palette: " << ((expandRGBPalette) ? "True" : "False") << '\n';
  std::cout << "Is palette: " << ((isPaletteImage) ? "True" : "False") << '\n';

  constexpr unsigned long Dimension = 2;
  using ScalarPixelType = unsigned char;

  using ScalarImageType = itk::Image<ScalarPixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ScalarImageType>;
  using WriterType = itk::ImageFileWriter<ScalarImageType>;
  using IOType = itk::PNGImageIO;

  auto io = IOType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(io, PNGImageIO, ImageIOBase);

  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  ITK_TEST_SET_GET_BOOLEAN(io, ExpandRGBPalette, expandRGBPalette);

  // Exercise exception cases
  const size_t sizeOfActualIORegion =
    io->GetIORegion().GetNumberOfPixels() * (io->GetComponentSize() * io->GetNumberOfComponents());
  auto * loadBuffer = new char[sizeOfActualIORegion];

  ITK_TRY_EXPECT_EXCEPTION(io->Read(loadBuffer));

  io->SetFileName(argv[1]);
  reader->SetFileName(argv[1]);
  reader->SetImageIO(io);

  if (io->CanReadFile(""))
  {
    std::cerr << "Test failed!" << '\n';
    std::cout << "No filename specified." << '\n';
    std::cout << "CanReadFile: "
              << "Expected false but got true" << '\n';
    return EXIT_FAILURE;
  }

  if (!io->SupportsDimension(Dimension))
  {
    std::cerr << "Test failed!" << '\n';
    std::cerr << "itk::PNGImageIO does not support dimension: " << Dimension << '\n';
    return EXIT_FAILURE;
  }

  if (io->CanStreamRead())
  {
    std::cout << "itk::PNGImageIO can stream read" << '\n';
  }
  else
  {
    std::cout << "itk::PNGImageIO cannot stream read" << '\n';
  }

  if (!io->CanReadFile(argv[1]))
  {
    std::cerr << "Test failed!" << '\n';
    std::cout << "itk::PNGImageIO cannot read file " << argv[1] << '\n';
    return EXIT_FAILURE;
  }

  // Try reading
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  io->Print(std::cout);

  // Try Palette reading and scalar image reading
  IOType::PaletteType palette_read = io->GetColorPalette();
  if (!io->GetExpandRGBPalette() && isPaletteImage)
  {
    std::cout << "Expect to read palette image as scalar+palette." << '\n';
    if (io->GetIsReadAsScalarPlusPalette())
    {
      std::cout << "Image read as scalar+palette." << '\n';
    }
    else
    {
      std::cerr << "Test failed!" << '\n';
      std::cerr << "Cannot read data of this palette image as scalar." << '\n';
      return EXIT_FAILURE;
    }
  }
  else if (io->GetExpandRGBPalette())
  {
    if (isPaletteImage)
    {
      std::cout << "Expect to expand palette image." << '\n';
    }
    else
    {
      std::cout << "Expect to read non palette image with expanding palette requested." << '\n';
    }
    if (io->GetIsReadAsScalarPlusPalette())
    {
      std::cerr << '\n' << "Test failed!" << '\n';
      std::cerr << '\n' << "Palette image read as scalar plus palette, but expanding is requested." << '\n';
      return EXIT_FAILURE;
    }
    if (!palette_read.empty())
    {
      std::cerr << '\n' << "Test failed!" << '\n';
      std::cerr << '\n' << "Non empty palette but expanding palette to rgb requested." << '\n';
      return EXIT_FAILURE;
    }
    std::cout << "Image not read as scalar and palette is empty." << '\n';
  }
  else if (!isPaletteImage)
  {
    std::cout << "Expect to read non palette image with not expanding palette requested." << '\n';
    if (io->GetIsReadAsScalarPlusPalette())
    {
      std::cerr << '\n' << "Test failed!" << '\n';
      std::cerr << '\n' << "Image indicated as non palette read as scalar plus palette." << '\n';
      return EXIT_FAILURE;
    }
    if (!palette_read.empty())
    {
      std::cerr << '\n' << "Test failed!" << '\n';
      std::cerr << '\n' << "Non empty palette but image indicated as RGB." << '\n';
      return EXIT_FAILURE;
    }
    std::cout << "Image not read as scalar and palette is empty." << '\n';
  }
  else
  {
    // case not possible here
  }

  const ScalarImageType::Pointer inputImage = reader->GetOutput();

  // test writing palette
  const bool writePalette = !expandRGBPalette && isPaletteImage;
  std::cerr << "Trying to write the image as " << ((writePalette) ? "palette" : "expanded palette") << '\n';
  ITK_TEST_SET_GET_BOOLEAN(io, WritePalette, writePalette);

  io->Print(std::cout);

  writer->SetInput(inputImage);
  writer->SetImageIO(io);
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  // try reading written image
  std::cerr << "Reading the written image, expecting " << ((writePalette) ? "palette" : "RGB") << '\n';
  reader->SetImageIO(io);
  reader->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  std::cerr << "Image read as " << ((io->GetIsReadAsScalarPlusPalette()) ? "scalar + palette" : "RGB") << '\n';

  if (io->GetIsReadAsScalarPlusPalette() != writePalette)
  {
    std::cerr << "Test failed!" << '\n';
    std::cerr << "Written image is expected to be a palette image, but could not be read such as." << '\n';
    return EXIT_FAILURE;
  }
  if (io->GetIsReadAsScalarPlusPalette())
  {
    IOType::PaletteType palette_written = io->GetColorPalette();

    if (palette_written.size() != palette_read.size())
    {
      std::cerr << "Test failed!" << '\n';
      std::cerr << "Read and written palette don't have the same size ( " << palette_read.size() << " vs "
                << palette_written.size() << ')' << '\n';
      return EXIT_FAILURE;
    }
    {
      bool   palette_equal = true;
      size_t i = 0;
      for (; i < palette_written.size(); ++i)
      {
        if (palette_written[i] != palette_read[i])
        {
          palette_equal = false;
          break;
        }
      }
      if (!palette_equal)
      {
        std::cerr << "Test failed!" << '\n';
        std::cerr << "Palette not written as it was read at position [" << i << "]." << '\n';
        return EXIT_FAILURE;
      }
    }
    std::cout << "Read and written palette are equal" << '\n';
  }

  // Exercise other methods
  const itk::ImageIOBase::SizeType pixelStride = io->GetPixelStride();
  std::cout << "PixelStride: " << itk::NumericTraits<itk::ImageIOBase::SizeType>::PrintType(pixelStride) << '\n';

  // ToDo
  // When the palette has made into the Metadata Dictionary (as opposed to the ImageIO):
  // Use the MetaDataDictionary
  // io->SetMetaDataDictionary();


  // Release memory
  delete[] loadBuffer;

  std::cout << "Test finished" << '\n';
  return EXIT_SUCCESS;
}
