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

#include "itkContourExtractor2DImageFilter.h"

#include "itkGTest.h"

namespace
{

// Label equals NumericTraits<float>::min(), colliding with the pre-fix m_UnusedLabel seed.
template <typename ImageType>
typename ImageType::Pointer
MakeCornerBlockLabelImage()
{
  auto                         image = ImageType::New();
  typename ImageType::SizeType size;
  size.Fill(4);
  const typename ImageType::RegionType region{ typename ImageType::IndexType{ { 0, 0 } }, size };
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(0.0f);

  const float collidingLabel = itk::NumericTraits<float>::min();
  image->SetPixel({ { 0, 0 } }, collidingLabel);
  image->SetPixel({ { 1, 0 } }, collidingLabel);
  image->SetPixel({ { 0, 1 } }, collidingLabel);
  image->SetPixel({ { 1, 1 } }, collidingLabel);

  return image;
}

} // namespace

TEST(ContourExtractor2DImageFilter, LabelContoursHandlesLabelEqualToNumericTraitsMin)
{
  using ImageType = itk::Image<float, 2>;
  using FilterType = itk::ContourExtractor2DImageFilter<ImageType>;

  auto image = MakeCornerBlockLabelImage<ImageType>();

  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->LabelContoursOn();
  filter->Update();

  // The background contour traces the full image perimeter; the block's is the shorter one.
  FilterType::OutputPathType::Pointer blockContour;
  for (unsigned int i = 0; i < filter->GetNumberOfIndexedOutputs(); ++i)
  {
    auto output = filter->GetOutput(i);
    if (blockContour.IsNull() || output->GetVertexList()->Size() < blockContour->GetVertexList()->Size())
    {
      blockContour = output;
    }
  }
  ASSERT_TRUE(blockContour.IsNotNull());

  // A label touching the image border must still produce a closed contour, not an open arc.
  const auto vertices = blockContour->GetVertexList();
  ASSERT_GE(vertices->Size(), 2u);
  EXPECT_EQ(vertices->ElementAt(0), vertices->ElementAt(vertices->Size() - 1));
}
