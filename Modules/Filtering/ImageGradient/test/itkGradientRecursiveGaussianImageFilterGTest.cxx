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

#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkImageBufferRange.h"
#include "itkNthElementImageAdaptor.h"
#include "itkGTest.h"

#include <algorithm>
#include <random>

namespace
{
constexpr unsigned int Dimension = 2;
using ScalarImageType = itk::Image<float, Dimension>;
using VectorImageType = itk::Image<itk::Vector<float, Dimension>, Dimension>;
using AdaptorType = itk::NthElementImageAdaptor<VectorImageType, float>;
using GradientImageType = itk::Image<itk::CovariantVector<double, Dimension>, Dimension>;

constexpr itk::SizeValueType imageSize = 16;
constexpr unsigned int       selectedElement = 1;

// An image whose two elements are different random integers.
VectorImageType::Pointer
MakeVectorImage()
{
  auto image = VectorImageType::New();
  image->SetRegions(VectorImageType::SizeType::Filled(imageSize));
  image->Allocate();
  std::mt19937 generator(42);
  for (auto & pixel : itk::ImageBufferRange<VectorImageType>(*image))
  {
    VectorImageType::PixelType value;
    value[0] = static_cast<float>(generator() % 100);
    value[1] = static_cast<float>(generator() % 100);
    pixel = value;
  }
  return image;
}

template <typename TInputImage>
GradientImageType::Pointer
ComputeGradient(const TInputImage * input)
{
  auto filter = itk::GradientRecursiveGaussianImageFilter<TInputImage, GradientImageType>::New();
  filter->SetInput(input);
  filter->SetSigma(1.5);
  filter->Update();
  return filter->GetOutput();
}

// The gradient of the selected element, computed from a scalar copy of it.
GradientImageType::Pointer
ComputeReferenceGradient(const VectorImageType * vectorImage)
{
  auto scalarImage = ScalarImageType::New();
  scalarImage->SetRegions(vectorImage->GetBufferedRegion());
  scalarImage->Allocate();
  const itk::ImageBufferRange<const VectorImageType> vectorRange(*vectorImage);
  const itk::ImageBufferRange<ScalarImageType>       scalarRange(*scalarImage);
  auto                                               scalarIt = scalarRange.begin();
  for (const VectorImageType::PixelType & value : vectorRange)
  {
    *scalarIt = value[selectedElement];
    ++scalarIt;
  }
  return ComputeGradient(scalarImage.GetPointer());
}

void
ExpectEqualImages(const GradientImageType & actual, const GradientImageType & expected)
{
  const itk::ImageBufferRange<const GradientImageType> actualRange(actual);
  const itk::ImageBufferRange<const GradientImageType> expectedRange(expected);
  ASSERT_EQ(actualRange.size(), expectedRange.size());
  // Reports the first difference only.
  const auto mismatch = std::mismatch(actualRange.begin(), actualRange.end(), expectedRange.begin());
  EXPECT_TRUE(mismatch.first == actualRange.end())
    << "first difference at pixel " << (mismatch.first - actualRange.begin()) << ": "
    << static_cast<GradientImageType::PixelType>(*mismatch.first)
    << " != " << static_cast<GradientImageType::PixelType>(*mismatch.second);
}

class GlobalReleaseDataFlagGuard
{
public:
  GlobalReleaseDataFlagGuard()
    : m_Previous(itk::DataObject::GetGlobalReleaseDataFlag())
  {
    itk::DataObject::SetGlobalReleaseDataFlag(true);
  }
  ~GlobalReleaseDataFlagGuard() { itk::DataObject::SetGlobalReleaseDataFlag(m_Previous); }
  ITK_DISALLOW_COPY_AND_MOVE(GlobalReleaseDataFlagGuard);

private:
  bool m_Previous;
};
} // namespace

// The gradient of an NthElementImageAdaptor input is that of the selected element.
TEST(GradientRecursiveGaussianImageFilter, NthElementImageAdaptorInput)
{
  const auto vectorImage = MakeVectorImage();
  const auto expected = ComputeReferenceGradient(vectorImage);

  auto adaptor = AdaptorType::New();
  adaptor->SetImage(vectorImage);
  adaptor->SelectNthElement(selectedElement);
  ExpectEqualImages(*ComputeGradient(adaptor.GetPointer()), *expected);
}

// The same, with the global release data flag on, where each pass gets a graft of the adaptor.
TEST(GradientRecursiveGaussianImageFilter, NthElementImageAdaptorInputWithGlobalReleaseDataFlag)
{
  const auto vectorImage = MakeVectorImage();
  const auto expected = ComputeReferenceGradient(vectorImage);

  auto adaptor = AdaptorType::New();
  adaptor->SetImage(vectorImage);
  adaptor->SelectNthElement(selectedElement);
  GradientImageType::Pointer actual;
  {
    const GlobalReleaseDataFlagGuard releaseData;
    actual = ComputeGradient(adaptor.GetPointer());
  }
  ExpectEqualImages(*actual, *expected);
}
