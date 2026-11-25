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
#include "itkExtractImageFilter.h"
#include "itkMetaImageIO.h"
#include "itksys/SystemTools.hxx"
#include "itkGTest.h"

#include <algorithm>
#include <random>
#include <stdexcept>
#include <tuple>


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


namespace
{
using CollapseStrategy = itk::ExtractImageFilterEnums::DirectionCollapseStrategy;
using Image3D = itk::Image<unsigned char, 3>;
using Image2D = itk::Image<unsigned char, 2>;

template <typename TImage>
struct TemporaryMetaImage
{
  explicit TemporaryMetaImage(const TImage * image)
    : directory(CreateDirectory())
    , path(directory + "/image.mha")
  {
    const auto writer = itk::ImageFileWriter<TImage>::New();
    writer->SetImageIO(itk::MetaImageIO::New());
    writer->SetFileName(path);
    writer->SetInput(image);
    writer->Update();
  }

  ~TemporaryMetaImage() { itksys::SystemTools::RemoveADirectory(directory); }

  std::string directory;
  std::string path;

private:
  static std::string
  CreateDirectory()
  {
    // A random suffix keeps concurrent runs of the same test, even from different builds, apart.
    const auto * test = ::testing::UnitTest::GetInstance()->current_test_info();
    std::string  name = std::string("itkImageFileReaderCollapse-") + test->test_suite_name() + '-' + test->name();
    std::replace(name.begin(), name.end(), '/', '_');
    std::random_device device;
    for (unsigned int attempt = 0; attempt < 16; ++attempt)
    {
      const std::string directory =
        itksys::SystemTools::CollapseFullPath(name + '-' + std::to_string(device()), ::testing::TempDir());
      if (!itksys::SystemTools::FileExists(directory) && itksys::SystemTools::MakeDirectory(directory).IsSuccess())
      {
        return directory;
      }
    }
    throw std::runtime_error("Could not create a temporary image directory.");
  }
};

template <typename TOutputImage, typename TInputImage>
typename itk::ImageFileReader<TOutputImage>::Pointer
MakeReader(const TemporaryMetaImage<TInputImage> & file)
{
  const auto reader = itk::ImageFileReader<TOutputImage>::New();
  reader->SetImageIO(itk::MetaImageIO::New());
  reader->SetFileName(file.path);
  return reader;
}

Image3D::Pointer
MakeCollapseInput(unsigned int caseIndex)
{
  const auto image = Image3D::New();
  auto       size = Image3D::SizeType::Filled(2);
  if (caseIndex == 6)
  {
    size[2] = 1;
  }
  image->SetRegions(size);
  image->AllocateInitialized();
  image->FillBuffer(13);
  image->SetOrigin(Image3D::PointType{ { 10.0, 20.0, 30.0 } });
  image->SetSpacing(Image3D::SpacingType{ { 2.0, 3.0, 4.0 } });
  Image3D::DirectionType direction;
  direction.SetIdentity();
  switch (caseIndex)
  {
    case 0:
    case 6:
      direction[0][0] = direction[1][1] = 0.0;
      direction[0][1] = -1.0;
      direction[1][0] = 1.0;
      break;
    case 1:
      direction[1][1] = direction[2][2] = 0.8;
      direction[1][2] = -0.6;
      direction[2][1] = 0.6;
      break;
    case 2:
      direction[0][0] = direction[2][2] = 0.0;
      direction[0][2] = direction[2][0] = 1.0;
      break;
    case 3:
      direction[0][0] = 0.8;
      break;
    case 4:
      direction[0][0] = -1.0;
      break;
    case 5:
      direction[0][0] = direction[2][2] = 1e-12;
      direction[0][2] = -1.0;
      direction[2][0] = 1.0;
      break;
  }
  image->SetDirection(direction);
  return image;
}

class ImageFileReaderCollapseTest : public ::testing::TestWithParam<std::tuple<CollapseStrategy, unsigned int>>
{};
} // namespace

TEST_P(ImageFileReaderCollapseTest, MatchesExtractImageFilter)
{
  const auto [strategy, caseIndex] = GetParam();
  const auto                        image = MakeCollapseInput(caseIndex);
  const TemporaryMetaImage<Image3D> file(image);
  const auto                        reader = MakeReader<Image2D>(file);
  reader->SetDirectionCollapseToStrategy(strategy);
  const auto extract = itk::ExtractImageFilter<Image3D, Image2D>::New();
  auto       region = image->GetLargestPossibleRegion();
  auto       size = region.GetSize();
  size[2] = 0;
  region.SetSize(size);
  extract->SetExtractionRegion(region);
  extract->SetDirectionCollapseToStrategy(strategy);
  extract->SetInput(image);

  if (caseIndex == 2 && strategy == CollapseStrategy::DIRECTIONCOLLAPSETOSUBMATRIX)
  {
    EXPECT_THROW(reader->Update(), itk::ExceptionObject);
    EXPECT_THROW(extract->Update(), itk::ExceptionObject);
    return;
  }
  ASSERT_NO_THROW(reader->Update());
  ASSERT_NO_THROW(extract->Update());
  EXPECT_EQ(extract->GetOutput()->GetDirection(), reader->GetOutput()->GetDirection());
  EXPECT_EQ(extract->GetOutput()->GetOrigin(), reader->GetOutput()->GetOrigin());
  EXPECT_EQ(extract->GetOutput()->GetSpacing(), reader->GetOutput()->GetSpacing());
  EXPECT_EQ(extract->GetOutput()->GetLargestPossibleRegion(), reader->GetOutput()->GetLargestPossibleRegion());
  const Image2D::IndexType index{ { 1, 1 } };
  EXPECT_EQ(extract->GetOutput()->GetPixel(index), reader->GetOutput()->GetPixel(index));
  EXPECT_EQ(extract->GetOutput()->TransformIndexToPhysicalPoint<double>(index),
            reader->GetOutput()->TransformIndexToPhysicalPoint<double>(index));
}

INSTANTIATE_TEST_SUITE_P(DirectionStrategies,
                         ImageFileReaderCollapseTest,
                         ::testing::Combine(::testing::Values(CollapseStrategy::DIRECTIONCOLLAPSETOIDENTITY,
                                                              CollapseStrategy::DIRECTIONCOLLAPSETOSUBMATRIX,
                                                              CollapseStrategy::DIRECTIONCOLLAPSETOGUESS),
                                            ::testing::Range(0u, 7u)));

TEST(ImageFileReader, CollapseStrategyDefaultAndPipelineUpdates)
{
  const auto                        image = MakeCollapseInput(0);
  const TemporaryMetaImage<Image3D> file(image);
  const auto                        reader = MakeReader<Image2D>(file);
  EXPECT_EQ(CollapseStrategy::DIRECTIONCOLLAPSETOIDENTITY, reader->GetDirectionCollapseToStrategy());
  reader->Update();
  Image2D::DirectionType identity;
  identity.SetIdentity();
  EXPECT_EQ(identity, reader->GetOutput()->GetDirection());
  const auto previousTime = reader->GetMTime();
  reader->SetDirectionCollapseToIdentity();
  EXPECT_EQ(previousTime, reader->GetMTime());
  reader->SetDirectionCollapseToSubmatrix();
  EXPECT_GT(reader->GetMTime(), previousTime);
  reader->Update();
  EXPECT_DOUBLE_EQ(-1.0, reader->GetOutput()->GetDirection()[0][1]);
  reader->SetDirectionCollapseToGuess();
  EXPECT_EQ(CollapseStrategy::DIRECTIONCOLLAPSETOGUESS, reader->GetDirectionCollapseToStrategy());
  reader->Update();
  EXPECT_DOUBLE_EQ(-1.0, reader->GetOutput()->GetDirection()[0][1]);
  EXPECT_THROW(reader->SetDirectionCollapseToStrategy(CollapseStrategy::DIRECTIONCOLLAPSETOUNKOWN),
               itk::ExceptionObject);
  EXPECT_THROW(reader->SetDirectionCollapseToStrategy(static_cast<CollapseStrategy>(255)), itk::ExceptionObject);
  EXPECT_EQ(CollapseStrategy::DIRECTIONCOLLAPSETOGUESS, reader->GetDirectionCollapseToStrategy());
}

TEST(ImageFileReader, RetainsSpatialDirectionWhenReadingFirstVolume)
{
  using Image4D = itk::Image<unsigned char, 4>;
  const auto image = Image4D::New();
  image->SetRegions(Image4D::SizeType::Filled(2));
  image->AllocateInitialized();
  image->FillBuffer(7);
  Image4D::DirectionType direction;
  direction.SetIdentity();
  direction[0][0] = direction[1][1] = 0.0;
  direction[0][1] = -1.0;
  direction[1][0] = 1.0;
  direction[0][3] = 1e-8;
  direction[3][0] = -1e-8;
  image->SetDirection(direction);
  image->SetOrigin(Image4D::PointType{ { 10.0, 20.0, 30.0, 0.0 } });
  image->SetSpacing(Image4D::SpacingType{ { 2.0, 3.0, 4.0, 1.0 } });
  const TemporaryMetaImage<Image4D> file(image);
  const auto                        reader = MakeReader<Image3D>(file);
  reader->SetDirectionCollapseToSubmatrix();
  reader->Update();
  const Image3D::IndexType index{ { 1, 1, 1 } };
  const auto               point = reader->GetOutput()->TransformIndexToPhysicalPoint<double>(index);
  EXPECT_DOUBLE_EQ(7.0, point[0]);
  EXPECT_DOUBLE_EQ(22.0, point[1]);
  EXPECT_DOUBLE_EQ(34.0, point[2]);
  EXPECT_EQ(7, reader->GetOutput()->GetPixel(index));
}

TEST(ImageFileReader, CollapseStrategyDoesNotChangeFullDimensionalReads)
{
  const auto                        image = MakeCollapseInput(1);
  const TemporaryMetaImage<Image3D> file(image);
  const auto                        reader = MakeReader<Image3D>(file);
  reader->SetDirectionCollapseToIdentity();
  reader->Update();
  EXPECT_EQ(image->GetDirection(), reader->GetOutput()->GetDirection());
}
