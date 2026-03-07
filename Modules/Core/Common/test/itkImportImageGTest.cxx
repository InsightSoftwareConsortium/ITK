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

#include "itkImageRegionIterator.h"
#include "itkShrinkImageFilter.h"
#include "itkImportImageFilter.h"
#include "itkGTest.h"
#include "itkMath.h"

#include <iostream>


TEST(ImportImageFilter, ImportAndShrink)
{
  // Create a C-array to hold an image
  auto * rawImage = new short[8 * 12];
  for (unsigned int i = 0; i < 8 * 12; ++i)
  {
    rawImage[i] = static_cast<short>(i);
  }

  constexpr unsigned int Dimension{ 2 };
  using PixelType = short;

  using ImportImageFilter = itk::ImportImageFilter<PixelType, Dimension>;
  using ShortImage = itk::Image<PixelType, Dimension>;

  // Test basic object methods
  auto basicImport = ImportImageFilter::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(basicImport, ImportImageFilter, ImageSource);

  ShortImage::Pointer                             image;
  constexpr itk::ImageRegion<Dimension>::SizeType size = { { 8, 12 } };
  itk::ImageRegion<Dimension>                     region = { size };

  {
    const ImportImageFilter::Pointer import = ImportImageFilter::New();

    constexpr itk::SpacePrecisionType data[2]{ 1.0, 1.0 };
    import->SetSpacing(data);

    constexpr float data2[2]{ 1.0, 1.0 };
    import->SetSpacing(data2);

    const itk::SpacePrecisionType * spacingValue = import->GetSpacing().GetDataPointer();
    std::cout << "import->GetSpacing(): " << spacingValue << std::endl;

    constexpr double data3[2]{ 1.0, 1.0 };
    import->SetOrigin(data3);

    constexpr float data4[2]{ 1.0, 1.0 };
    import->SetOrigin(data4);

    const itk::SpacePrecisionType * originValue = import->GetOrigin().GetDataPointer();
    std::cout << "import->GetOrigin(): " << originValue << std::endl;

    import->SetRegion(region);
    import->SetImportPointer(rawImage, 8 * 12, true);
    import->Update();
    image = import->GetOutput();
  }

  const itk::ShrinkImageFilter<ImportImageFilter::OutputImageType, ShortImage>::Pointer shrink =
    itk::ShrinkImageFilter<ImportImageFilter::OutputImageType, ShortImage>::New();

  shrink->SetInput(image);
  shrink->SetShrinkFactors(2);
  EXPECT_NO_THROW(shrink->Update());

  const ShortImage::RegionType         requestedRegion = shrink->GetOutput()->GetRequestedRegion();
  itk::ImageRegionIterator<ShortImage> iterator2(shrink->GetOutput(), requestedRegion);

  for (; !iterator2.IsAtEnd(); ++iterator2)
  {
    std::cout << "Pixel " << iterator2.ComputeIndex() << " = " << iterator2.Get() << std::endl;
    const short expectedValue = itk::Math::RoundHalfIntegerUp<short>(static_cast<float>(
      (shrink->GetShrinkFactors()[0] * iterator2.ComputeIndex()[0] + shrink->GetShrinkFactors()[0] / 2) +
      (region.GetSize()[0] *
       ((shrink->GetShrinkFactors()[1] / 2) + (shrink->GetShrinkFactors()[0] * iterator2.ComputeIndex()[1])))));
    EXPECT_EQ(iterator2.Get(), expectedValue) << "Pixel mismatch at " << iterator2.ComputeIndex();
  }
}
