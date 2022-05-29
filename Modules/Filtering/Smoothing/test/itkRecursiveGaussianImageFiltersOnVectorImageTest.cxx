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
#include "itkRecursiveGaussianImageFilter.h"
#include "itkVectorImage.h"
#include "itkTestingMacros.h"

int
itkRecursiveGaussianImageFiltersOnVectorImageTest(int, char *[])
{
  // In this test, we will create a 9x9 image of vectors with pixels (4,4)
  // and (1,6) set to 'vector1'. We will filter it using
  // RecursiveGaussianImageFilter and compare a few filtered pixels.
  //
  constexpr unsigned int Dimension = 2;
  constexpr double       sigma = 1;
  constexpr double       tolerance = 0.001;

  constexpr unsigned int NumberOfComponents = 4;
  using PixelComponentType = double;
  using ImageType = itk::VectorImage<PixelComponentType, Dimension>;
  using PixelType = ImageType::PixelType;
  using ConstIteratorType = itk::ImageLinearConstIteratorWithIndex<ImageType>;

  // Create ON and OFF vectors.
  PixelType vector0;
  PixelType vector1;

  vector0.SetSize(NumberOfComponents);
  vector1.SetSize(NumberOfComponents);

  vector0.Fill(0.0);
  vector1.Fill(1.0);

  // Create the 9x9 input image
  ImageType::SizeType size;
  size.Fill(9);
  ImageType::IndexType index;
  index.Fill(0);
  ImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);
  auto inputImage = ImageType::New();
  inputImage->SetRegions(region);
  inputImage->SetNumberOfComponentsPerPixel(NumberOfComponents);
  inputImage->Allocate();
  inputImage->FillBuffer(vector0);

  std::cout << "Apply RecursiveGaussianImageFilter with a 9x9 image, pixels (4,4) "
            << "and (1,6) set to ON." << std::endl;

  /* Set pixel (4,4) with the value 1
   * and pixel (1,6) with the value 2
   */
  index[0] = 4;
  index[1] = 4;
  inputImage->SetPixel(index, vector1);
  index[0] = 1;
  index[1] = 6;
  inputImage->SetPixel(index, vector1);

  // Gaussian filter this image now. Each component of the tensor
  // is filtered independently.
  //
  using FilterType = itk::RecursiveGaussianImageFilter<ImageType, ImageType>;
  auto filterX = FilterType::New();
  auto filterY = FilterType::New();
  filterX->SetDirection(0); // 0 --> X direction
  filterY->SetDirection(1); // 1 --> Y direction
  filterX->SetOrder(itk::GaussianOrderEnum::ZeroOrder);
  filterY->SetOrder(itk::GaussianOrderEnum::ZeroOrder);
  filterX->SetNormalizeAcrossScale(false);
  filterY->SetNormalizeAcrossScale(false);
  filterX->SetInput(inputImage);
  filterY->SetInput(filterX->GetOutput());
  filterX->SetSigma(sigma);
  filterY->SetSigma(sigma);

  ITK_TRY_EXPECT_NO_EXCEPTION(filterY->Update());


  // Test a few pixels of the  fitlered image
  //
  ImageType::Pointer filteredImage = filterY->GetOutput();
  ConstIteratorType  cit(filteredImage, filteredImage->GetRequestedRegion());
  cit.SetDirection(0);

  index[0] = 4;
  index[1] = 4;
  cit.SetIndex(index);
  if (itk::Math::abs(cit.Get()[0] - 0.160313) > tolerance)
  {
    std::cout << "[FAILED] Tensor(0,0) at index (4,4) must be 0.1603 but is " << cit.Get()[0] << std::endl;
    return EXIT_FAILURE;
  }

  index[0] = 6;
  index[1] = 6;
  cit.SetIndex(index);
  if (itk::Math::abs(cit.Get()[3] - 0.0026944) > tolerance)
  {
    std::cout << "[FAILED] Tensor(3,3) at index (6,6) must be 0.0026944 but is " << cit.Get()[3] << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
