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

#include "itkPowImageFilter.h"
#include "itkTestingMacros.h"


int
itkPowImageFilterTest(int, char *[])
{

  using ImageType = itk::Image<float, 1>;
  using myImageType1 = itk::Image<short, 2>;
  using myImageType2 = itk::Image<int, 3>;
  using myImageType3 = itk::Image<float, 3>;


  using FilterType = itk::PowImageFilter<ImageType>;

  // The following is to ensure that the filter can be instantiated
  // with these types without warning
  itk::PowImageFilter<myImageType1>::New();
  itk::PowImageFilter<myImageType2>::New();
  itk::PowImageFilter<myImageType3>::New();
  itk::PowImageFilter<myImageType2, myImageType3>::New();


  using SizeType = itk::Size<1>;
  using IndexType = itk::Index<1>;

  ImageType::Pointer inputImageA = ImageType::New();
  ImageType::Pointer inputImageB = ImageType::New();

  SizeType size;
  size[0] = 2;

  ImageType::RegionType region(size);

  // Initialize Image A
  inputImageA->SetRegions(region);
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetRegions(region);
  inputImageB->Allocate();

  // set some initial pixel values
  IndexType idx;
  for (unsigned int i = 0; i < size[0]; ++i)
  {
    idx[0] = i;
    inputImageA->SetPixel(idx, i + 1);
    inputImageB->SetPixel(idx, 1.0);
  }

  // Create a PowFilter
  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, PowImageFilter, BinaryGeneratorImageFilter);

  // Check == and != operators
  // FilterType::FunctorType func2;

  // Connect the input images
  filter->SetInput1(inputImageA);
  filter->SetInput2(inputImageB);

  // Get the Smart Pointer to the Filter Output
  ImageType::Pointer outputImage = filter->GetOutput();

  // Execute the filter
  filter->Update();

  IndexType idx0;
  idx0[0] = 0;
  IndexType idx1;
  idx1[0] = 1;

  // Values should be 1.0^1.0 and 2.0^1.0
  ITK_TEST_EXPECT_EQUAL(outputImage->GetPixel(idx0), 1.0);
  ITK_TEST_EXPECT_EQUAL(outputImage->GetPixel(idx1), 2.0);

  filter->SetInput1(inputImageA);
  filter->SetConstant2(2.0);
  filter->Update();

  // Values should be 1.0^2.0 and 2.0^2.0
  ITK_TEST_EXPECT_EQUAL(outputImage->GetPixel(idx0), 1.0);
  ITK_TEST_EXPECT_EQUAL(outputImage->GetPixel(idx1), 4.0);

  filter->SetConstant1(2.0);
  filter->SetInput2(inputImageA);
  filter->Update();

  // Values should be 2.0^1.0 and 2.0^2.0
  ITK_TEST_EXPECT_EQUAL(outputImage->GetPixel(idx0), 2.0);
  ITK_TEST_EXPECT_EQUAL(outputImage->GetPixel(idx1), 4.0);

  {
    using complexFloatFilterType =
      itk::PowImageFilter<itk::Image<float>, itk::Image<std::complex<float>>, itk::Image<std::complex<float>>>;
    complexFloatFilterType::Pointer tFilter = complexFloatFilterType::New();
    ITK_TEST_EXPECT_TRUE(!tFilter.IsNull());
  }

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
