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

#include "itkImageIOBase.h"
#include "itkImageIOFactory.h"
#include "itkMetaImageIO.h"
#include "itkGTest.h"
#include "itkTestDriverIncludeRequiredFactories.h"

namespace
{
struct ImageIOExtensionFactoryTest : public ::testing::Test
{
  void
  SetUp() override
  {
    RegisterRequiredFactories();
  }
};
} // namespace

TEST_F(ImageIOExtensionFactoryTest, HasSupportedReadExtensionDefaultIgnoresCase)
{
  const auto io = itk::MetaImageIO::New();
  EXPECT_TRUE(io->HasSupportedReadExtension("image.mha"));
  EXPECT_TRUE(io->HasSupportedReadExtension("image.MHA"));
  EXPECT_TRUE(io->HasSupportedReadExtension("image.mhd"));
}

TEST_F(ImageIOExtensionFactoryTest, HasSupportedReadExtensionCaseSensitiveOptOut)
{
  const auto io = itk::MetaImageIO::New();
  EXPECT_FALSE(io->HasSupportedReadExtension("image.MHA", false));
}

TEST_F(ImageIOExtensionFactoryTest, HasSupportedReadExtensionRejectsUnknown)
{
  const auto io = itk::MetaImageIO::New();
  EXPECT_FALSE(io->HasSupportedReadExtension("image.png"));
  EXPECT_FALSE(io->HasSupportedReadExtension("no_extension"));
}

TEST_F(ImageIOExtensionFactoryTest, HasSupportedWriteExtensionDefaultIgnoresCase)
{
  const auto io = itk::MetaImageIO::New();
  EXPECT_TRUE(io->HasSupportedWriteExtension("out.mha"));
  EXPECT_TRUE(io->HasSupportedWriteExtension("out.MHA"));
}

TEST_F(ImageIOExtensionFactoryTest, HasSupportedWriteExtensionCaseSensitiveOptOut)
{
  const auto io = itk::MetaImageIO::New();
  EXPECT_FALSE(io->HasSupportedWriteExtension("out.MHA", false));
}

TEST_F(ImageIOExtensionFactoryTest, HasSupportedWriteExtensionRejectsUnknown)
{
  const auto io = itk::MetaImageIO::New();
  EXPECT_FALSE(io->HasSupportedWriteExtension("out.png"));
}

TEST_F(ImageIOExtensionFactoryTest, CreateImageIOReturnsNullForMissingReadFile)
{
  const auto missingFileIO =
    itk::ImageIOFactory::CreateImageIO("/this/path/does/not/exist.mha", itk::IOFileModeEnum::ReadMode);
  EXPECT_EQ(missingFileIO, nullptr);
}

TEST_F(ImageIOExtensionFactoryTest, CreateImageIOResolvesMetaForWriteByExtension)
{
  const auto writeIO =
    itk::ImageIOFactory::CreateImageIO("/this/path/does/not/exist.mha", itk::IOFileModeEnum::WriteMode);
  ASSERT_NE(writeIO, nullptr);
  EXPECT_STREQ(writeIO->GetNameOfClass(), "MetaImageIO");
}
