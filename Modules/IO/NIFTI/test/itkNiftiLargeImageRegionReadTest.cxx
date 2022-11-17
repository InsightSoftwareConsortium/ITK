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
#include "itkTestingMacros.h"


int
itkNiftiLargeImageRegionReadTest(int argc, char * argv[])
{

  if (argc != 2)
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputFileName" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string fname{ argv[1] };

  constexpr int Dimension = 3;
  using PixelType = unsigned short;

  using ImageType = itk::Image<PixelType, Dimension>;

  // Create a large image
  ImageType::SizeType   size = { { 1034, 1034, 1020 } };
  ImageType::RegionType region;
  region.SetSize(size);

  {
    auto image = ImageType::New();
    image->SetRegions(region);
    image->Allocate(true);
    itk::WriteImage(image, fname);
  }

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();

  reader->SetFileName(fname);

  // Ensure that a sufficiently large subregion, requiring allocating a pixel buffer larger than INT_MAX bytes, can be
  // read, as nifti_clib did not support reading such large subregions previously.
  region.SetSize({ { 1029, 1029, 1017 } });
  reader->GetOutput()->SetRequestedRegion(region);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  const auto output = reader->GetOutput();
  ITK_TEST_EXPECT_TRUE(output != nullptr);
  ITK_TEST_EXPECT_TRUE(output->GetBufferedRegion() == region);


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
