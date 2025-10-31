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

#include "itkImageSeriesWriter.h"
#include "itkImageSeriesReader.h"
#include "itkImage.h"
#include "itkTIFFImageIO.h"

#include "itkGTest.h"
#include "itksys/SystemTools.hxx"
#include "itkTestDriverIncludeRequiredFactories.h"
#include <string>


#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{

struct ITKIOTIFF : public ::testing::Test
{
  void
  SetUp() override
  {
    RegisterRequiredFactories();

    itksys::SystemTools::MakeDirectory(m_TempDir);
  }

  void
  TearDown() override
  {
    // Remove the temporary directory and its contents
    itksys::SystemTools::RemoveADirectory(m_TempDir);
  }

  std::string m_TempDir = TOSTRING(ITK_TEST_OUTPUT_DIR) + "/ImageSeriesReader.ReverseOrder";


  template <typename PixelType>
  typename itk::Image<PixelType, 3>::Pointer
  MakeImage(unsigned int z_size)
  {
    using ImageType = itk::Image<PixelType, 3>;

    typename ImageType::Pointer    img = ImageType::New();
    typename ImageType::SizeType   size = { { 32, 32, z_size } };
    typename ImageType::RegionType region;
    region.SetSize(size);
    img->SetRegions(region);
    img->AllocateInitialized();

    for (unsigned int z = 0; z < z_size; ++z)
    {
      for (unsigned int y = 0; y < 32; ++y)
        for (unsigned int x = 0; x < 32; ++x)
        {
          typename ImageType::IndexType idx = { { x, y, z } };
          img->SetPixel(idx, static_cast<PixelType>(1 + z * 5));
        }
    }
    return img;
  }
};


} // namespace

TEST_F(ITKIOTIFF, ReverseOrder_with_ImageIO)
{


  // Create a series of 3 TIFF files with different values using native itk::Image
  const unsigned int z_size = 3;
  using PixelType = unsigned char;
  constexpr unsigned int Dimension{ 3 };
  using ImageType = itk::Image<PixelType, Dimension>;


  auto img = MakeImage<PixelType>(z_size);

  std::vector<std::string> filePaths;
  for (unsigned int z = 0; z < z_size; ++z)
  {
    std::ostringstream oss;
    oss << m_TempDir << "/test_reader_kwargs_" << z << ".tiff";
    filePaths.push_back(oss.str());
  }
  {
    // Create an ImageSeriesWriter to write the series of images
    auto writer = itk::ImageSeriesWriter<
      ImageType,
      typename ImageType::RebindImageType<ImageType::PixelType, ImageType::ImageDimension - 1>>::New();
    writer->SetFileNames(filePaths);
    writer->SetInput(img);
    writer->Update();
  }

  // Create ImageIO for TIFF
  auto imageIO = itk::TIFFImageIO::New();

  // Test reading with ReverseOrder=True
  auto reader = itk::ImageSeriesReader<ImageType>::New();
  reader->SetImageIO(imageIO);
  reader->SetFileNames(filePaths);
  reader->SetReverseOrder(true);
  reader->Update();
  ImageType::Pointer reversed_img = reader->GetOutput();


  // Print the value at 0,0 of each slice for the two images
  for (unsigned int z = 0; z < z_size; ++z)
  {

    ImageType::IndexType idx{ { 31, 31, z } };
    auto                 actual = reversed_img->GetPixel(idx);

    idx[2] = z_size - 1 - z;
    auto expected = img->GetPixel(idx);

    EXPECT_EQ(static_cast<int>(expected), static_cast<int>(actual)) << "Slice " << z << " value mismatch";
  }
}
