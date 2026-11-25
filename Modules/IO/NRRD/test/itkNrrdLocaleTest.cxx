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

#include <locale.h>
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNrrdImageIO.h"
#include "itkTestingMacros.h"
#include "itkTestNumericLocale.h"

// Test that NRRD reader handles locale-dependent parsing correctly.
// This test verifies the fix for: https://github.com/InsightSoftwareConsortium/ITK/issues/5683

int
itkNrrdLocaleTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " OutputDirectory" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  // Create a test image with fractional spacing
  auto image = ImageType::New();

  auto size = itk::MakeFilled<ImageType::SizeType>(16);

  ImageType::IndexType start{};

  ImageType::RegionType region(start, size);
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(42.0f);

  // Set spacing with fractional values that would be misparsed
  // in locales using comma as decimal separator (without the fix)
  ImageType::SpacingType spacing;
  spacing[0] = 3.5;      // Without fix: would be truncated to 3 in de_DE locale
  spacing[1] = 0.878906; // Without fix: would become 0.0 in de_DE locale, causing zero-spacing error
  spacing[2] = 2.2;      // Without fix: would be truncated to 2 in de_DE locale

  image->SetSpacing(spacing);

  // Set origin with fractional values
  ImageType::PointType origin;
  origin[0] = 1.5;
  origin[1] = 2.75;
  origin[2] = 0.5;
  image->SetOrigin(origin);

  // Write the image with C locale (normal operation)
  std::string filename = std::string(argv[1]) + "/locale_test.nrrd";

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(filename);
  writer->SetInput(image);
  writer->SetImageIO(itk::NrrdImageIO::New());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "Original spacing: " << spacing << std::endl;
  std::cout << "Original origin: " << origin << std::endl;

  // Test 1: Read with C locale (should work)
  {
    setlocale(LC_NUMERIC, "C");

    using ReaderType = itk::ImageFileReader<ImageType>;
    auto reader = ReaderType::New();
    reader->SetFileName(filename);
    reader->SetImageIO(itk::NrrdImageIO::New());

    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

    ImageType::Pointer     readImage = reader->GetOutput();
    ImageType::SpacingType readSpacing = readImage->GetSpacing();
    ImageType::PointType   readOrigin = readImage->GetOrigin();

    std::cout << "C locale - Read spacing: " << readSpacing << std::endl;
    std::cout << "C locale - Read origin: " << readOrigin << std::endl;

    // Verify spacing
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      if (itk::Math::Absolute(readSpacing[i] - spacing[i]) > 1e-6)
      {
        std::cerr << "Spacing mismatch in C locale at index " << i << std::endl;
        std::cerr << "Expected: " << spacing[i] << ", Got: " << readSpacing[i] << std::endl;
        return EXIT_FAILURE;
      }
    }

    // Verify origin
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      if (itk::Math::Absolute(readOrigin[i] - origin[i]) > 1e-6)
      {
        std::cerr << "Origin mismatch in C locale at index " << i << std::endl;
        std::cerr << "Expected: " << origin[i] << ", Got: " << readOrigin[i] << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  // Test 2: read under a decimal-comma locale, the only path exercising the parse.
  unsigned int localesTested = 0;
  for (const char * const commaLocale : itk::test::commaDecimalLocales)
  {
    if (setlocale(LC_NUMERIC, commaLocale) == nullptr)
    {
      continue;
    }
    ++localesTested;
    std::cout << "Testing with " << commaLocale << " locale..." << std::endl;

    using ReaderType = itk::ImageFileReader<ImageType>;
    auto reader = ReaderType::New();
    reader->SetFileName(filename);
    reader->SetImageIO(itk::NrrdImageIO::New());

    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

    ImageType::Pointer     readImage = reader->GetOutput();
    ImageType::SpacingType readSpacing = readImage->GetSpacing();
    ImageType::PointType   readOrigin = readImage->GetOrigin();

    std::cout << commaLocale << " - Read spacing: " << readSpacing << std::endl;
    std::cout << commaLocale << " - Read origin: " << readOrigin << std::endl;

    // Verify spacing - this is the critical test
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      if (itk::Math::Absolute(readSpacing[i] - spacing[i]) > 1e-6)
      {
        std::cerr << "Spacing mismatch in " << commaLocale << " locale at index " << i << std::endl;
        std::cerr << "Expected: " << spacing[i] << ", Got: " << readSpacing[i] << std::endl;
        std::cerr << "This indicates locale-dependent parsing is still occurring!" << std::endl;
        return EXIT_FAILURE;
      }
    }

    // Verify origin
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      if (itk::Math::Absolute(readOrigin[i] - origin[i]) > 1e-6)
      {
        std::cerr << "Origin mismatch in " << commaLocale << " locale at index " << i << std::endl;
        std::cerr << "Expected: " << origin[i] << ", Got: " << readOrigin[i] << std::endl;
        std::cerr << "This indicates locale-dependent parsing is still occurring!" << std::endl;
        return EXIT_FAILURE;
      }
    }
  }
  setlocale(LC_NUMERIC, "C");

  // Test 3: WRITE under a decimal-comma locale, read back under "C". A
  // locale-dependent writer would emit "0,878906" and corrupt the file.
  for (const char * const commaLocale : itk::test::commaDecimalLocales)
  {
    if (setlocale(LC_NUMERIC, commaLocale) == nullptr)
    {
      continue;
    }
    const std::string commaFilename = std::string(argv[1]) + "/locale_test_written_under_comma.nrrd";
    auto              commaWriter = WriterType::New();
    commaWriter->SetFileName(commaFilename);
    commaWriter->SetInput(image);
    commaWriter->SetImageIO(itk::NrrdImageIO::New());
    ITK_TRY_EXPECT_NO_EXCEPTION(commaWriter->Update());
    setlocale(LC_NUMERIC, "C");

    using ReaderType = itk::ImageFileReader<ImageType>;
    auto reader = ReaderType::New();
    reader->SetFileName(commaFilename);
    reader->SetImageIO(itk::NrrdImageIO::New());
    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

    const ImageType::SpacingType readSpacing = reader->GetOutput()->GetSpacing();
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      if (itk::Math::Absolute(readSpacing[i] - spacing[i]) > 1e-6)
      {
        std::cerr << "Spacing mismatch after writing under " << commaLocale << " at index " << i << std::endl;
        std::cerr << "Expected: " << spacing[i] << ", Got: " << readSpacing[i] << std::endl;
        std::cerr << "This indicates locale-dependent FORMATTING during write!" << std::endl;
        return EXIT_FAILURE;
      }
    }
    break; // one comma locale suffices for the write path
  }
  setlocale(LC_NUMERIC, "C");

  if (localesTested == 0)
  {
    if (itk::test::CommaDecimalLocaleIsExpected())
    {
      std::cerr << itk::test::NoCommaDecimalLocaleMessage() << std::endl;
      return EXIT_FAILURE;
    }
    std::cout << "WARNING: " << itk::test::NoCommaDecimalLocaleMessage() << std::endl;
  }

  std::cout << "Test finished successfully." << std::endl;
  return EXIT_SUCCESS;
}
