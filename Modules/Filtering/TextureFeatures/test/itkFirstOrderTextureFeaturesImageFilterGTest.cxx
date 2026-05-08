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
#include "itkFirstOrderTextureFeaturesImageFilter.h"
#include "itkFlatStructuringElement.h"
#include "itkAdditiveGaussianNoiseImageFilter.h"
#include "itkSimpleFilterWatcher.h"

#include "gtest/gtest.h"

namespace
{

template <typename T>
void
print_feature(const T & p, std::ostream & out = std::cout)
{
  out << "mean: " << p[0] << std::endl;
  out << "minimum: " << p[1] << std::endl;
  out << "maximum: " << p[2] << std::endl;
  out << "variance: " << p[3] << std::endl;
  out << "standard deviation: " << p[4] << std::endl;
  out << "skewness: " << p[5] << std::endl;
  out << "kurtosis: " << p[6] << std::endl;
  out << "entropy: " << p[7] << std::endl;
}

} // namespace

TEST(TextureFeatures, FirstOrder_Test1)
{
  constexpr unsigned int ImageDimension = 2;
  using ImageType = itk::Image<float, ImageDimension>;
  using OImageType = itk::Image<itk::FixedArray<float, 8>, ImageDimension>;
  using KernelType = itk::FlatStructuringElement<ImageDimension>;


  unsigned int                r = 50u;
  unsigned int                d = r * 2 + 1;
  ImageType::SizeType         imageSize = { { d, d } };
  ImageType::SpacingValueType imageSpacing[] = { 1.0f, 1.0f };

  ImageType::Pointer image = ImageType::New();

  image->SetRegions(ImageType::RegionType(imageSize));
  image->SetSpacing(imageSpacing);
  image->Allocate();
  image->FillBuffer(0);


  using ImageNoiseType = itk::AdditiveGaussianNoiseImageFilter<ImageType>;
  ImageNoiseType::Pointer noiseFilter = ImageNoiseType::New();
  noiseFilter->SetSeed(124);
  noiseFilter->SetMean(0.0);
  noiseFilter->SetStandardDeviation(.1);
  noiseFilter->SetInput(image);

  using TextureFilterType = itk::FirstOrderTextureFeaturesImageFilter<ImageType, OImageType, KernelType>;

  KernelType::SizeType radius;
  radius.Fill(r);
  KernelType                 kernel = KernelType::Box(radius);
  TextureFilterType::Pointer filter = TextureFilterType::New();
  filter->SetKernel(kernel);
  filter->SetInput(noiseFilter->GetOutput());

  itk::SimpleFilterWatcher watcher(filter, "filter");


  ImageType::SizeType   requestSize = { { 10, 10 } };
  ImageType::IndexType  requestIndex = { { 45, 45 } };
  ImageType::RegionType request(requestIndex, requestSize);
  filter->GetOutput()->SetRequestedRegion(request);
  filter->Update();

  OImageType::ConstPointer output = filter->GetOutput();

  {
    ImageType::IndexType          idx = { { r, r } };
    const OImageType::PixelType & p = output->GetPixel(idx);

    print_feature(p);

    // The following is for a Gaussian sample with std dev of .1
    // The expected value was analytically computed, while the
    // tolerances were estimated based on 10,000 different sample set
    // distributions in numpy.
    EXPECT_NEAR(0.0, p[0], 0.002) << "mean";
    EXPECT_GT(-.3, p[1]) << "min";
    EXPECT_LT(.3, p[2]) << "max";
    EXPECT_NEAR(0.01, p[3], .001) << "variance";
    EXPECT_NEAR(0.1, p[4], .01) << "standard deviation";
    EXPECT_NEAR(0, p[5], .1) << "skewness";
    EXPECT_NEAR(0, p[6], .2) << "kurtosis";
    EXPECT_NEAR(13.3, p[7], .1) << "entropy";
  }
}


TEST(TextureFeatures, FirstOrder_Test2)
{
  constexpr unsigned int ImageDimension = 2;
  using ImageType = itk::Image<float, ImageDimension>;
  using OImageType = itk::Image<itk::FixedArray<float, 8>, ImageDimension>;
  using KernelType = itk::FlatStructuringElement<ImageDimension>;


  unsigned int                r = 50u;
  unsigned int                d = r * 2 + 1;
  ImageType::SizeType         imageSize = { { d, d } };
  ImageType::SpacingValueType imageSpacing[] = { 1.0f, 1.0f };

  ImageType::Pointer image = ImageType::New();

  image->SetRegions(ImageType::RegionType(imageSize));
  image->SetSpacing(imageSpacing);
  image->Allocate();
  image->FillBuffer(0);


  using ImageNoiseType = itk::AdditiveGaussianNoiseImageFilter<ImageType>;
  ImageNoiseType::Pointer noiseFilter = ImageNoiseType::New();
  noiseFilter->SetSeed(124);
  noiseFilter->SetMean(100.0);
  noiseFilter->SetStandardDeviation(1);
  noiseFilter->SetInput(image);

  using TextureFilterType = itk::FirstOrderTextureFeaturesImageFilter<ImageType, OImageType, KernelType>;

  KernelType::SizeType radius;
  radius.Fill(r);
  KernelType                 kernel = KernelType::Box(radius);
  TextureFilterType::Pointer filter = TextureFilterType::New();
  filter->SetKernel(kernel);
  filter->SetInput(noiseFilter->GetOutput());

  itk::SimpleFilterWatcher watcher(filter, "filter");


  ImageType::SizeType   requestSize = { { 10, 10 } };
  ImageType::IndexType  requestIndex = { { 45, 45 } };
  ImageType::RegionType request(requestIndex, requestSize);
  filter->GetOutput()->SetRequestedRegion(request);
  filter->Update();

  OImageType::ConstPointer output = filter->GetOutput();

  {
    ImageType::IndexType          idx = { { r, r } };
    const OImageType::PixelType & p = output->GetPixel(idx);

    print_feature(p);

    // The following is for a Gaussian sample with std dev of .1
    // The expected value was analytically computed, while the
    // tolerances were estimated based on 10,000 different sample set
    // distributions in numpy.
    EXPECT_NEAR(100, p[0], 0.02) << "mean";
    EXPECT_GT(97, p[1]) << "min";
    EXPECT_LT(103, p[2]) << "max";
    EXPECT_NEAR(1, p[3], .1) << "variance";
    EXPECT_NEAR(1, p[4], .1) << "standard deviation";
    EXPECT_NEAR(0, p[5], .1) << "skewness";
    EXPECT_NEAR(0, p[6], .2) << "kurtosis";
    EXPECT_NEAR(13.3, p[7], .2) << "entropy";
  }
}
