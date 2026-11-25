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
#include "itkGiplImageIO.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVector.h"

#include <cstdint>
#include <filesystem>
#include <fstream>
#include <sstream>

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{
using ScalarImageType = itk::Image<unsigned char, 2>;

ScalarImageType::Pointer
MakeScalarImage(unsigned int side)
{
  auto                            image = ScalarImageType::New();
  const ScalarImageType::SizeType size{ { side, side } };
  image->SetRegions(ScalarImageType::RegionType(size));
  image->Allocate();
  image->FillBuffer(42);
  return image;
}

std::string
OutputPath(const std::string & name)
{
  return TOSTRING(ITK_TEST_OUTPUT_DIR) + "/" + name;
}

template <typename TImage>
void
WriteWithGiplIO(TImage * image, const std::string & path)
{
  auto writer = itk::ImageFileWriter<TImage>::New();
  writer->SetImageIO(itk::GiplImageIO::New());
  writer->SetInput(image);
  writer->SetFileName(path);
  ASSERT_NO_THROW(writer->Update());
}

template <typename TImage>
void
ExpectWriteThrows(TImage * image, const std::string & path)
{
  auto writer = itk::ImageFileWriter<TImage>::New();
  writer->SetImageIO(itk::GiplImageIO::New());
  writer->SetInput(image);
  writer->SetFileName(path);
  EXPECT_THROW(writer->Update(), itk::ExceptionObject);
}

template <typename TImage>
typename TImage::Pointer
ReadWithGiplIO(const std::string & path)
{
  auto reader = itk::ImageFileReader<TImage>::New();
  reader->SetImageIO(itk::GiplImageIO::New());
  reader->SetFileName(path);
  EXPECT_NO_THROW(reader->Update());
  return reader->GetOutput();
}

void
ExpectTruncatedReadThrows(const std::string & path, const std::uintmax_t truncatedSize)
{
  std::filesystem::resize_file(path, truncatedSize);

  auto reader = itk::ImageFileReader<ScalarImageType>::New();
  reader->SetImageIO(itk::GiplImageIO::New());
  reader->SetFileName(path);
  EXPECT_THROW(reader->Update(), itk::ExceptionObject);
}
} // namespace

TEST(GiplImageIO, WriteOfUnsupportedComponentTypePreservesExistingFile)
{
  const std::string path = OutputPath("gipl_b53_existing.gipl");
  const std::string originalContents = "pre-existing data that must survive a failed Gipl write";
  {
    std::ofstream out(path, std::ios::binary);
    out << originalContents;
  }

  using UnsupportedImageType = itk::Image<unsigned long, 2>;
  auto                                 image = UnsupportedImageType::New();
  const UnsupportedImageType::SizeType size{ { 2, 2 } };
  image->SetRegions(UnsupportedImageType::RegionType(size));
  image->Allocate();
  image->FillBuffer(7);

  ExpectWriteThrows(image.GetPointer(), path);

  std::ifstream      in(path, std::ios::binary);
  std::ostringstream contents;
  contents << in.rdbuf();
  EXPECT_EQ(contents.str(), originalContents);
}

TEST(GiplImageIO, ReadOfTruncatedUncompressedFileThrows)
{
  const std::string path = OutputPath("gipl_b54_truncated.gipl");
  WriteWithGiplIO(MakeScalarImage(8).GetPointer(), path);

  const auto fullSize = std::filesystem::file_size(path);
  ASSERT_GT(fullSize, 20u);
  ExpectTruncatedReadThrows(path, fullSize - 10);
}

TEST(GiplImageIO, ReadOfTruncatedCompressedFileThrows)
{
  const std::string path = OutputPath("gipl_b55_truncated.gipl.gz");
  WriteWithGiplIO(MakeScalarImage(8).GetPointer(), path);

  const auto fullSize = std::filesystem::file_size(path);
  ASSERT_GT(fullSize, 20u);
  ExpectTruncatedReadThrows(path, fullSize / 2);
}

TEST(GiplImageIO, ReadImageInformationOfHeaderTruncatedUncompressedFileThrows)
{
  const std::string path = OutputPath("gipl_b86_header_truncated.gipl");
  WriteWithGiplIO(MakeScalarImage(8).GetPointer(), path);
  std::filesystem::resize_file(path, 100);

  const itk::GiplImageIO::Pointer io = itk::GiplImageIO::New();
  io->SetFileName(path);
  EXPECT_THROW(io->ReadImageInformation(), itk::ExceptionObject);
}

TEST(GiplImageIO, ReadImageInformationOfHeaderTruncatedCompressedFileThrows)
{
  const std::string path = OutputPath("gipl_b86_header_truncated.gipl.gz");
  WriteWithGiplIO(MakeScalarImage(8).GetPointer(), path);
  std::filesystem::resize_file(path, 20);

  const itk::GiplImageIO::Pointer io = itk::GiplImageIO::New();
  io->SetFileName(path);
  EXPECT_THROW(io->ReadImageInformation(), itk::ExceptionObject);
}

TEST(GiplImageIO, WriteOfMultiComponentImageThrows)
{
  using VectorPixelType = itk::Vector<float, 3>;
  using VectorImageType = itk::Image<VectorPixelType, 2>;

  auto                            image = VectorImageType::New();
  const VectorImageType::SizeType size{ { 4, 4 } };
  image->SetRegions(VectorImageType::RegionType(size));
  image->Allocate();
  image->FillBuffer(VectorPixelType({ 1.0f, 2.0f, 3.0f }));

  ExpectWriteThrows(image.GetPointer(), OutputPath("gipl_b56_vector.gipl"));
}

TEST(GiplImageIO, RoundTripsIntAndUnsignedIntComponentTypes)
{
  using IntImageType = itk::Image<int, 2>;
  using UIntImageType = itk::Image<unsigned int, 2>;
  const IntImageType::SizeType size{ { 3, 3 } };

  auto intImage = IntImageType::New();
  intImage->SetRegions(IntImageType::RegionType(size));
  intImage->Allocate();
  intImage->FillBuffer(-70000);
  intImage->GetBufferPointer()[1] = 123456;

  auto uintImage = UIntImageType::New();
  uintImage->SetRegions(UIntImageType::RegionType(size));
  uintImage->Allocate();
  uintImage->FillBuffer(3000000000u);
  uintImage->GetBufferPointer()[1] = 654321u;

  const std::string intPath = OutputPath("gipl_enh_int.gipl");
  const std::string uintPath = OutputPath("gipl_enh_uint.gipl");

  WriteWithGiplIO(intImage.GetPointer(), intPath);
  WriteWithGiplIO(uintImage.GetPointer(), uintPath);

  const auto intRead = ReadWithGiplIO<IntImageType>(intPath);
  const auto uintRead = ReadWithGiplIO<UIntImageType>(uintPath);

  const auto numberOfPixels = intImage->GetPixelContainer()->Size();
  for (unsigned int i = 0; i < numberOfPixels; ++i)
  {
    EXPECT_EQ(intImage->GetBufferPointer()[i], intRead->GetBufferPointer()[i]);
    EXPECT_EQ(uintImage->GetBufferPointer()[i], uintRead->GetBufferPointer()[i]);
  }
}
