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
#include "gtest/gtest.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkJPEGImageIO.h"

#include <string>

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{
using ScalarImageType = itk::Image<unsigned char, 2>;

std::string
OutputPath(const std::string & name)
{
  return TOSTRING(ITK_TEST_OUTPUT_DIR) + "/" + name;
}
} // namespace

TEST(JPEGImageIO, SpacingSurvivesCentimeterRoundTrip)
{
  auto                                image = ScalarImageType::New();
  constexpr ScalarImageType::SizeType size{ { 8, 8 } };
  image->SetRegions(ScalarImageType::RegionType(size));
  image->AllocateInitialized();

  ScalarImageType::SpacingType spacing;
  spacing[0] = 10.0 / 3.0;
  spacing[1] = 10.0 / 3.0;
  image->SetSpacing(spacing);

  const std::string path = OutputPath("jpeg_b76_cm_spacing.jpg");

  auto writer = itk::ImageFileWriter<ScalarImageType>::New();
  writer->SetImageIO(itk::JPEGImageIO::New());
  writer->SetFileName(path);
  writer->SetInput(image);
  ASSERT_NO_THROW(writer->Update());

  auto reader = itk::ImageFileReader<ScalarImageType>::New();
  reader->SetImageIO(itk::JPEGImageIO::New());
  reader->SetFileName(path);
  ASSERT_NO_THROW(reader->Update());

  const auto readSpacing = reader->GetOutput()->GetSpacing();
  EXPECT_NEAR(readSpacing[0], spacing[0], 0.1);
  EXPECT_NEAR(readSpacing[1], spacing[1], 0.1);
}
