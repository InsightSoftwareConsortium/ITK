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
#include "itksys/SystemTools.hxx"

#include "itkGTest.h"

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{

constexpr unsigned int Dimension = 3;
using PixelType = float;
using ImageType = itk::Image<PixelType, Dimension>;

class NrrdLocaleTest : public ::testing::Test
{
public:
  void
  SetUp() override
  {
    itksys::SystemTools::ChangeDirectory(TOSTRING(ITK_TEST_OUTPUT_DIR));
  }

  ImageType::Pointer
  CreateTestImage()
  {
    auto image = ImageType::New();

    ImageType::SizeType size;
    size.Fill(16);

    ImageType::IndexType start;
    start.Fill(0);

    ImageType::RegionType region(start, size);
    image->SetRegions(region);
    image->Allocate();
    image->FillBuffer(42.0f);

    // Set spacing with fractional values that would be misparsed
    // in locales using comma as decimal separator
    ImageType::SpacingType spacing;
    spacing[0] = 3.5;      // Would be parsed correctly even with truncation
    spacing[1] = 0.878906; // Would become 0.0 in de_DE locale, causing error
    spacing[2] = 2.2;      // Would be parsed incorrectly

    image->SetSpacing(spacing);

    // Set origin with fractional values
    ImageType::PointType origin;
    origin[0] = 1.5;
    origin[1] = 2.75;
    origin[2] = 0.5;
    image->SetOrigin(origin);

    return image;
  }
};

TEST_F(NrrdLocaleTest, ReadWriteWithCLocale)
{
  auto       image = CreateTestImage();
  const auto expectedSpacing = image->GetSpacing();
  const auto expectedOrigin = image->GetOrigin();

  const std::string filename = "locale_test.nrrd";

  // Write the image with C locale
  setlocale(LC_NUMERIC, "C");

  auto writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetFileName(filename);
  writer->SetInput(image);
  writer->SetImageIO(itk::NrrdImageIO::New());
  EXPECT_NO_THROW(writer->Update());

  // Read with C locale
  auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetFileName(filename);
  reader->SetImageIO(itk::NrrdImageIO::New());
  EXPECT_NO_THROW(reader->Update());

  const auto readImage = reader->GetOutput();
  const auto readSpacing = readImage->GetSpacing();
  const auto readOrigin = readImage->GetOrigin();

  // Verify spacing
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    EXPECT_NEAR(readSpacing[i], expectedSpacing[i], 1e-6) << "Spacing mismatch in C locale at index " << i;
  }

  // Verify origin
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    EXPECT_NEAR(readOrigin[i], expectedOrigin[i], 1e-6) << "Origin mismatch in C locale at index " << i;
  }
}

TEST_F(NrrdLocaleTest, ReadWithGermanLocale)
{
  auto       image = CreateTestImage();
  const auto expectedSpacing = image->GetSpacing();
  const auto expectedOrigin = image->GetOrigin();

  const std::string filename = "locale_test_de.nrrd";

  // Write the image with C locale
  setlocale(LC_NUMERIC, "C");

  auto writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetFileName(filename);
  writer->SetInput(image);
  writer->SetImageIO(itk::NrrdImageIO::New());
  EXPECT_NO_THROW(writer->Update());

  // Try to set German locale; skip test if not available
  if (setlocale(LC_NUMERIC, "de_DE.UTF-8") == nullptr)
  {
    GTEST_SKIP() << "de_DE.UTF-8 locale not available, skipping locale-specific test";
  }

  // Read with de_DE locale - this is the critical test
  auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetFileName(filename);
  reader->SetImageIO(itk::NrrdImageIO::New());
  EXPECT_NO_THROW(reader->Update());

  const auto readImage = reader->GetOutput();
  const auto readSpacing = readImage->GetSpacing();
  const auto readOrigin = readImage->GetOrigin();

  // Verify spacing - this would fail without locale-independent parsing
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    EXPECT_NEAR(readSpacing[i], expectedSpacing[i], 1e-6)
      << "Spacing mismatch in de_DE locale at index " << i
      << ". This indicates locale-dependent parsing is still occurring!";
  }

  // Verify origin
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    EXPECT_NEAR(readOrigin[i], expectedOrigin[i], 1e-6)
      << "Origin mismatch in de_DE locale at index " << i
      << ". This indicates locale-dependent parsing is still occurring!";
  }

  // Restore C locale
  setlocale(LC_NUMERIC, "C");
}

} // namespace
