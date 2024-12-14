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

#include "itkComposeImageFilter.h"
#include "itkVectorImage.h"
#include "itkTestingMacros.h"

int
itkComposeBigVectorImageFilterTest(int, char *[])
{

  // Test case for SimpleITK issue #2220
  // Reported data corruption with 4G compose on Windows system


  const unsigned int size = 400;
  const unsigned int nchannels = 100;
  using ImageType = itk::Image<unsigned char, 3>;
  using VectorImageType = itk::VectorImage<unsigned char, 3>;
  std::vector<ImageType::Pointer> images;

  for (unsigned int i = 0; i < nchannels; ++i)
  {
    auto                  image = ImageType::New();
    ImageType::RegionType region;
    ImageType::SizeType   imageSize = { { size, size, size } };
    region.SetSize(imageSize);
    image->SetRegions(region);
    image->Allocate();
    image->FillBuffer(i % 250);

    images.push_back(image);
  }

  using ComposeFilterType = itk::ComposeImageFilter<ImageType>;
  ComposeFilterType::Pointer composeFilter = ComposeFilterType::New();
  for (unsigned int i = 0; i < nchannels; ++i)
  {
    composeFilter->SetInput(i, images[i]);
  }

  composeFilter->Update();
  VectorImageType::Pointer img = composeFilter->GetOutput();
  std::cout << "Compose filter executed." << std::endl;

  for (unsigned int i = 0; i < nchannels; ++i)
  {
    itk::IndexValueType                  z = i;
    std::map<unsigned int, unsigned int> values;

    for (itk::IndexValueType y = 0; y < size; ++y)
    {
      for (itk::IndexValueType x = 0; x < size; ++x)
      {
        itk::Index<3> idx = { { x, y, z } };

        auto pixel = img->GetPixel(idx);
        if (pixel[i] != i % 250)
        {
          ++values[pixel[i]];
        }
      }
    }

    if (!values.empty())
    {
      std::cout << "unexpected values: ";
      for (auto v : values)
      {
        std::cout << v.first << "(" << v.second << "), ";
      }
      std::cout << std::endl;
    }
    ITK_TEST_EXPECT_TRUE(values.empty());
  }
  return EXIT_SUCCESS;
}
