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
#include "itkMetaImageIO.h"
#include "itkGTest.h"

#include <cstdio>


TEST(ImageFileReader, ConvertedLegacyTest)
{
  using ImageNDType = itk::Image<short, 2>;
  using ReaderType = itk::ImageFileReader<ImageNDType>;

  // Try an empty read
  auto reader = ReaderType::New();
  EXPECT_THROW(reader->Update(), itk::ExceptionObject);


  // Now try a read with an image that doesn't exist
  reader->SetFileName("this_file_should_not_exist");
  EXPECT_THROW(reader->Update(), itk::ExceptionObject);


  // Let's try to read a file where no ImageIO can read it.
  // Use the test binary itself, which is guaranteed to exist on disk.
  reader->SetFileName(::testing::internal::GetArgvs()[0]);
  EXPECT_THROW(reader->Update(), itk::ExceptionObject);
}


TEST(ImageFileReader, RetainsSpatialDirectionWhenReadingFirstVolume)
{
  using Image4DType = itk::Image<unsigned char, 4>;
  using Image3DType = itk::Image<unsigned char, 3>;

  auto                  image = Image4DType::New();
  Image4DType::SizeType size;
  size.Fill(2);
  Image4DType::RegionType region;
  region.SetSize(size);
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(7);

  Image4DType::DirectionType direction;
  direction.SetIdentity();
  direction[0][0] = 0.0;
  direction[0][1] = -1.0;
  direction[1][0] = 1.0;
  direction[1][1] = 0.0;
  direction[0][3] = 1e-8;
  direction[3][0] = -1e-8;
  image->SetDirection(direction);

  Image4DType::PointType origin;
  origin[0] = 10.0;
  origin[1] = 20.0;
  origin[2] = 30.0;
  origin[3] = 0.0;
  image->SetOrigin(origin);

  Image4DType::SpacingType spacing;
  spacing[0] = 2.0;
  spacing[1] = 3.0;
  spacing[2] = 4.0;
  spacing[3] = 1.0;
  image->SetSpacing(spacing);

  const std::string path = std::string(::testing::TempDir()) + "/itkImageFileReaderDirection4D.mha";
  auto              writer = itk::ImageFileWriter<Image4DType>::New();
  writer->SetImageIO(itk::MetaImageIO::New());
  writer->SetFileName(path);
  writer->SetInput(image);
  writer->Update();

  auto reader = itk::ImageFileReader<Image3DType>::New();
  reader->SetImageIO(itk::MetaImageIO::New());
  reader->SetFileName(path);
  reader->Update();

  const Image3DType::IndexType index{ { 1, 1, 1 } };
  const auto                   point = reader->GetOutput()->TransformIndexToPhysicalPoint<double>(index);
  EXPECT_DOUBLE_EQ(7.0, point[0]);
  EXPECT_DOUBLE_EQ(22.0, point[1]);
  EXPECT_DOUBLE_EQ(34.0, point[2]);
  EXPECT_EQ(7, reader->GetOutput()->GetPixel(index));

  std::remove(path.c_str());
}


TEST(ImageFileReader, UsesIdentityWhenReducedDirectionIsSingular)
{
  using Image3DType = itk::Image<unsigned char, 3>;
  using Image2DType = itk::Image<unsigned char, 2>;

  auto                  image = Image3DType::New();
  Image3DType::SizeType size;
  size.Fill(2);
  Image3DType::RegionType region;
  region.SetSize(size);
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(9);

  Image3DType::DirectionType direction;
  direction.SetIdentity();
  direction[0][0] = 0.0;
  direction[0][2] = 1.0;
  direction[2][0] = 1.0;
  direction[2][2] = 0.0;
  image->SetDirection(direction);

  const std::string path = std::string(::testing::TempDir()) + "/itkImageFileReaderSingularDirection3D.mha";
  auto              writer = itk::ImageFileWriter<Image3DType>::New();
  writer->SetImageIO(itk::MetaImageIO::New());
  writer->SetFileName(path);
  writer->SetInput(image);
  writer->Update();

  auto reader = itk::ImageFileReader<Image2DType>::New();
  reader->SetImageIO(itk::MetaImageIO::New());
  reader->SetFileName(path);
  reader->Update();

  Image2DType::DirectionType identity;
  identity.SetIdentity();
  EXPECT_EQ(identity, reader->GetOutput()->GetDirection());
  const Image2DType::IndexType index{ { 1, 1 } };
  EXPECT_EQ(9, reader->GetOutput()->GetPixel(index));

  std::remove(path.c_str());
}


TEST(ImageFileReader, UsesIdentityWhenDiscardedAxisMixesWithKeptAxes)
{
  using Image3DType = itk::Image<unsigned char, 3>;
  using Image2DType = itk::Image<unsigned char, 2>;

  auto                  image = Image3DType::New();
  Image3DType::SizeType size;
  size.Fill(2);
  Image3DType::RegionType region;
  region.SetSize(size);
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(11);

  Image3DType::DirectionType direction;
  direction.SetIdentity();
  direction[1][1] = 0.8;
  direction[1][2] = -0.6;
  direction[2][1] = 0.6;
  direction[2][2] = 0.8;
  image->SetDirection(direction);

  const std::string path = std::string(::testing::TempDir()) + "/itkImageFileReaderObliqueDirection3D.mha";
  auto              writer = itk::ImageFileWriter<Image3DType>::New();
  writer->SetImageIO(itk::MetaImageIO::New());
  writer->SetFileName(path);
  writer->SetInput(image);
  writer->Update();

  auto reader = itk::ImageFileReader<Image2DType>::New();
  reader->SetImageIO(itk::MetaImageIO::New());
  reader->SetFileName(path);
  reader->Update();

  Image2DType::DirectionType identity;
  identity.SetIdentity();
  EXPECT_EQ(identity, reader->GetOutput()->GetDirection());
  const Image2DType::IndexType index{ { 1, 1 } };
  EXPECT_EQ(11, reader->GetOutput()->GetPixel(index));

  std::remove(path.c_str());
}
