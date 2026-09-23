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

#include "itkCastImageFilter.h"
#include "itkVectorImage.h"
#include "itkVector.h"
#include "itkImageRegionConstIterator.h"

#include "itkGTest.h"

namespace
{

// Accesses pixels through a proxy, not a reference, like otb::Image (itk.org/issue/6898).
template <typename TPixel, unsigned int VImageDimension>
class ImageSubclass : public itk::Image<TPixel, VImageDimension>
{
public:
  using Self = ImageSubclass;
  using Superclass = itk::Image<TPixel, VImageDimension>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);

protected:
  ImageSubclass() = default;
  ~ImageSubclass() override = default;
};

} // namespace

// This casts a VectorImage<float, 2> to an image-subclass Image<Vector<double, 2>, 2>
TEST(CastImageFilter, CastToImageSubclassWithoutDirectPixelAccess)
{
  using OutputImageType = ImageSubclass<itk::Vector<double, 2>, 2>;
  using FloatVectorImageType = itk::VectorImage<float, 2>;

  // Create a 1x3 image of 2D vectors
  auto image = FloatVectorImageType::New();

  constexpr itk::Size<2>    size{ { 1, 3 } };
  const itk::ImageRegion<2> region{ size };
  image->SetNumberOfComponentsPerPixel(2);
  image->SetRegions(region);
  image->Allocate();
  itk::VariableLengthVector<float> vec;
  vec.SetSize(2);
  // All pixels will be the vector (1.3, 5.3)
  vec[0] = 1.3;
  vec[1] = 5.3;
  image->FillBuffer(vec);

  using CastImageFilterType = itk::CastImageFilter<FloatVectorImageType, OutputImageType>;
  auto castImageFilter = CastImageFilterType::New();
  castImageFilter->SetInput(image);
  castImageFilter->Update();

  itk::ImageRegionConstIterator<OutputImageType> castedImageIterator(
    castImageFilter->GetOutput(), castImageFilter->GetOutput()->GetLargestPossibleRegion());
  itk::ImageRegionConstIterator<FloatVectorImageType> originalImageIterator(image, image->GetLargestPossibleRegion());

  while (!originalImageIterator.IsAtEnd())
  {
    EXPECT_EQ(castedImageIterator.Get()[0], static_cast<double>(originalImageIterator.Get()[0]));
    EXPECT_EQ(castedImageIterator.Get()[1], static_cast<double>(originalImageIterator.Get()[1]));
    ++originalImageIterator;
    ++castedImageIterator;
  }
}
