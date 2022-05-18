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
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkImage.h"

#include "itkGTest.h"
#include "itksys/SystemTools.hxx"
#include "itkTestDriverIncludeRequiredFactories.h"

#define STRING(s) #s

namespace
{

struct ITKWriteImageFunctionTest : public ::testing::Test
{
  void
  SetUp() override
  {
    RegisterRequiredFactories();
    itksys::SystemTools::ChangeDirectory(STRING(ITK_TEST_OUTPUT_DIR_STR));
  }
  using ImageType = itk::Image<float, 2>;
  using RegionType = ImageType::RegionType;
  using SizeType = ImageType::SizeType;

  ImageType::Pointer
  MakeImage()
  {
    auto image = ImageType::New();

    RegionType region(SizeType{ { 3, 2 } });

    image->SetRegions(region);

    image->Allocate(true);
    return image;
  }
};


} // namespace

TEST_F(ITKWriteImageFunctionTest, ImageTypes)
{
  const std::string fileName = "test1.mha";

  ImageType::Pointer image_ptr = MakeImage();
  itk::WriteImage(image_ptr, fileName);
  itk::WriteImage(std::move(image_ptr), fileName);

  ImageType::ConstPointer image_cptr = MakeImage();
  itk::WriteImage(image_cptr, fileName);
  itk::WriteImage(std::move(image_cptr), fileName);

  const ImageType::ConstPointer image_ccptr = MakeImage();
  itk::WriteImage(image_ccptr, fileName);
  itk::WriteImage(std::move(image_ccptr), fileName);

  image_ptr = MakeImage();
  ImageType * image_rptr = image_ptr.GetPointer();
  itk::WriteImage(image_rptr, fileName);

  const ImageType * image_crptr = image_ptr.GetPointer();
  itk::WriteImage(image_crptr, fileName);

  // Test that what is written is also read back.
  const ImageType::ConstPointer readImage = itk::ReadImage<ImageType>(fileName);
  ASSERT_NE(readImage, nullptr);
  ASSERT_NE(image_ccptr, nullptr);
  EXPECT_EQ(*readImage, *image_ccptr);
}
