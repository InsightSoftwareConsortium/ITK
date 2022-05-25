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

#include "itkNiftiImageIOTest.h"
#include "itkTestingMacros.h"

// Test to read small voxel NIFTI file which was triggering numerical instability
// in IsAffine loading code
int
itkNiftiImageIOTest13(int argc, char * argv[])
{
  if (argc != 2)
  {
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  ImageType::Pointer image;

  ITK_TRY_EXPECT_NO_EXCEPTION(image = itk::ReadImage<ImageType>(argv[1]));

  if (image == nullptr)
  {
    std::cerr << "Error: The read image is null!\n";
    return EXIT_FAILURE;
  }

  if (image->GetBufferPointer() == nullptr)
  {
    std::cerr << "Error: the buffer of the read image is null!\n";
    return EXIT_FAILURE;
  }

  if (image->GetBufferedRegion().GetNumberOfPixels() == 0)
  {
    std::cerr << "Error: The read image has no pixels!\n";
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
