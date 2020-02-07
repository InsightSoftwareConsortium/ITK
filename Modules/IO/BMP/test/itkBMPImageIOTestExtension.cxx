/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include <itkImage.h>
#include <itkImageFileReader.h>
#include <itkImageRegion.h>
#include <itkRGBPixel.h>
#include <itkTestingMacros.h>
#include <fstream>
#include <iostream>
#include <string>

namespace
{
bool
WriteSmallBmp(const std::string & filename)
{
  const char    bmp_raw[] = { 66, 77, 30, 0, 0, 0, 0, 0, 0, 0,  26, 0, 0, 0,  12,
                           0,  0,  0,  1, 0, 1, 0, 1, 0, 24, 0,  0, 0, -1, 0 };
  std::ofstream bmp_output(filename, std::ios::binary);
  if (!bmp_output)
  {
    std::cerr << "Cannot open file for writing (" << filename << ")\n";
    return false;
  }
  bmp_output.write(bmp_raw, sizeof(bmp_raw));
  bmp_output.close();
  if (!bmp_output)
  {
    std::cerr << "Cannot write to file (" << filename << ")\n";
    return false;
  }
  return true;
}
} // namespace

int
itkBMPImageIOTestExtension(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " temporary_output_folder\n";
    return EXIT_FAILURE;
  }

  // filename shouldn't have bmp extension
  const std::string filename(std::string(argv[1]) + "/itkBMPImageIOTestExtension");

  ITK_TEST_EXPECT_TRUE(WriteSmallBmp(filename));

  using ComponentType = unsigned char;
  using PixelType = itk::RGBPixel<ComponentType>;
  auto reader = itk::ImageFileReader<itk::Image<PixelType, 2>>::New();
  reader->SetFileName(filename);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  const itk::ImageRegion<2> expected_region{ itk::Size<2>{ { 1, 1 } } };
  const ComponentType       expected_rgb[] = { 255, 0, 0 };
  const PixelType           expected_pixel{ expected_rgb };
  // check that data has been actually read
  auto image = reader->GetOutput();
  ITK_TEST_EXPECT_EQUAL(image->GetLargestPossibleRegion(), expected_region);
  ITK_TEST_EXPECT_EQUAL(image->GetPixel({ { 0, 0 } }), expected_pixel);
  std::cout << "Test finished\n";
  return EXIT_SUCCESS;
}
