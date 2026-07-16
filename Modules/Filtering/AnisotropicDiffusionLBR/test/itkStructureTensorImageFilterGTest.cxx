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

#include "itkStructureTensorImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkGTest.h"

namespace
{

using ImageType = itk::Image<float, 2>;
using FilterType = itk::StructureTensorImageFilter<ImageType>;

constexpr itk::SizeValueType  imageSize = 64;
constexpr itk::IndexValueType edge = 32;

ImageType::Pointer
MakeStepImage(unsigned int stepAxis)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType{ ImageType::IndexType{}, ImageType::SizeType::Filled(imageSize) });
  image->Allocate();

  itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion());
  for (; !it.IsAtEnd(); ++it)
  {
    it.Set(it.GetIndex()[stepAxis] < edge ? 0.0F : 100.0F);
  }
  return image;
}

FilterType::TensorType::ComponentType
TraceNearEdge(unsigned int stepAxis, FilterType::ScalarType featureScale)
{
  auto filter = FilterType::New();
  filter->SetInput(MakeStepImage(stepAxis));
  filter->SetNoiseScale(0.5);
  filter->SetFeatureScale(featureScale);
  filter->Update();

  auto index = ImageType::IndexType::Filled(edge);
  index[stepAxis] -= 12;
  return filter->GetOutput()->GetPixel(index).GetTrace();
}

} // namespace

TEST(StructureTensorImageFilter, FeatureScaleSmoothsAlongAllAxes)
{
  // An edge varying along one axis responds to feature-scale smoothing on that
  // axis only; a per-axis regression would leave its delta at zero.
  for (unsigned int stepAxis = 0; stepAxis < ImageType::ImageDimension; ++stepAxis)
  {
    const auto smallScaleTrace = TraceNearEdge(stepAxis, 1.0);
    const auto largeScaleTrace = TraceNearEdge(stepAxis, 8.0);
    EXPECT_GT(largeScaleTrace - smallScaleTrace, 10.0) << "stepAxis=" << stepAxis;
  }
}

// A constant image has zero gradients, so the tensor-trace maximum is zero; the
// adimensionizing rescale must not divide by it and poison the tensors with NaN
// (issue #6575, B23).
TEST(StructureTensorImageFilter, ConstantImageYieldsFiniteZeroTensors)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType{ ImageType::IndexType{}, ImageType::SizeType::Filled(16) });
  image->Allocate();
  image->FillBuffer(0.0F);

  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->SetRescaleForUnitMaximumTrace(true);
  filter->Update();

  using TensorImageType = FilterType::TensorImageType;
  itk::ImageRegionConstIterator<TensorImageType> it(filter->GetOutput(), filter->GetOutput()->GetBufferedRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    const auto & tensor = it.Get();
    for (unsigned int c = 0; c < tensor.Size(); ++c)
    {
      ASSERT_TRUE(std::isfinite(tensor[c]));
      ASSERT_NEAR(tensor[c], 0.0F, 1e-20);
    }
  }
}
