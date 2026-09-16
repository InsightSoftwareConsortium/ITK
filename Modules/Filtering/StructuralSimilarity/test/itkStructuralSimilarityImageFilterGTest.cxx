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

#include "itkGTest.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkPNGImageIOFactory.h"
#include "itkJPEGImageIOFactory.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkStructuralSimilarityImageFilter.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <random>

#define SSIM_STRINGIFY(s) #s
#define TOSTRING(s) std::string(SSIM_STRINGIFY(s))

namespace
{
//
// Expected values fall into three classes:
//
//   1. Mathematical identities (identical images, constant inputs, symmetry).
//
//   2. Closed-form formulas evaluated analytically over constant inputs.
//      The luminance term reduces to (2*mu_x*mu_y + C1)/(mu_x^2 + mu_y^2 + C1)
//      because variances and covariance vanish.
//
//   3. Regression values of this filter for synthetic and real image pairs.
//

// ---- Common types ---------------------------------------------------------

using PixelType = float;
constexpr unsigned int Dimension2D = 2;
using ImageType = itk::Image<PixelType, Dimension2D>;
using FilterType = itk::StructuralSimilarityImageFilter<ImageType>;

// ---- Image factories ------------------------------------------------------

ImageType::Pointer
MakeConstantImage(PixelType value, unsigned int size = 64)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(size));
  image->Allocate();
  image->FillBuffer(value);
  return image;
}

ImageType::Pointer
MakeGradientImage(unsigned int size = 64, PixelType scale = 255.0)
{
  auto                        image = ImageType::New();
  const auto                  imageSize = ImageType::SizeType::Filled(size);
  const ImageType::RegionType region(imageSize);
  image->SetRegions(region);
  image->Allocate();
  // (x + y) normalized to [0, scale]
  const PixelType norm = scale / static_cast<PixelType>(2 * (size - 1));
  for (itk::ImageRegionIteratorWithIndex it(image, region); !it.IsAtEnd(); ++it)
  {
    const auto idx = it.GetIndex();
    it.Set(norm * static_cast<PixelType>(idx[0] + idx[1]));
  }
  return image;
}

ImageType::Pointer
MakeRandomImage(unsigned int size = 64, unsigned int seed = 42, PixelType lo = 0.0, PixelType hi = 255.0)
{
  auto                        image = ImageType::New();
  const auto                  imageSize = ImageType::SizeType::Filled(size);
  const ImageType::RegionType region(imageSize);
  image->SetRegions(region);
  image->Allocate();
  std::mt19937                              gen(seed);
  std::uniform_real_distribution<PixelType> dist(lo, hi);
  for (itk::ImageRegionIterator it(image, region); !it.IsAtEnd(); ++it)
  {
    it.Set(dist(gen));
  }
  return image;
}

ImageType::Pointer
ScaledCopy(const ImageType * input, PixelType scale, PixelType bias = 0.0)
{
  auto       image = ImageType::New();
  const auto region = input->GetLargestPossibleRegion();
  image->SetRegions(region);
  image->Allocate();
  itk::ImageRegionConstIterator<ImageType> in(input, region);
  itk::ImageRegionIterator<ImageType>      out(image, region);
  for (; !in.IsAtEnd(); ++in, ++out)
  {
    out.Set(scale * in.Get() + bias);
  }
  return image;
}

ImageType::Pointer
NoisyCopy(const ImageType * input, PixelType noiseSigma, unsigned int seed)
{
  auto       image = ImageType::New();
  const auto region = input->GetLargestPossibleRegion();
  image->SetRegions(region);
  image->Allocate();
  std::mt19937                             gen(seed);
  std::normal_distribution<PixelType>      noise(0.0, noiseSigma);
  itk::ImageRegionConstIterator<ImageType> in(input, region);
  itk::ImageRegionIterator<ImageType>      out(image, region);
  for (; !in.IsAtEnd(); ++in, ++out)
  {
    out.Set(in.Get() + noise(gen));
  }
  return image;
}

double
ComputeFiltered(const ImageType * a, const ImageType * b)
{
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetScaleWeights(FilterType::ScaleWeightsType(1, 1.0));
  filter->Update();
  return filter->GetMeanSSIM();
}

// Reads an image as gray levels rescaled to [0, 1].
ImageType::Pointer
ReadAsNormalizedGray(const std::string & fileName)
{
  itk::PNGImageIOFactory::RegisterOneFactory();
  itk::JPEGImageIOFactory::RegisterOneFactory();

  auto rescaler = itk::RescaleIntensityImageFilter<ImageType>::New();
  rescaler->SetInput(itk::ReadImage<ImageType>(fileName));
  rescaler->SetOutputMinimum(0.0);
  rescaler->SetOutputMaximum(1.0);
  rescaler->Update();
  return rescaler->GetOutput();
}

// Default (5-scale) MS-SSIM of two images rescaled to [0, 1].
double
ComputeDefaultMSSSIM(const std::string & fileName1, const std::string & fileName2)
{
  auto image1 = ReadAsNormalizedGray(fileName1);
  auto image2 = ReadAsNormalizedGray(fileName2);
  EXPECT_EQ(image1->GetLargestPossibleRegion(), image2->GetLargestPossibleRegion());

  auto filter = FilterType::New();
  filter->SetInput1(image1);
  filter->SetInput2(image2);
  filter->Update();
  return filter->GetMeanSSIM();
}

} // namespace


// ===========================================================================
// Object-level macros and basic API
// ===========================================================================

TEST(StructuralSimilarityImageFilter, BasicObjectMethods)
{
  auto filter = FilterType::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(filter, StructuralSimilarityImageFilter, ImageToImageFilter);
}

TEST(StructuralSimilarityImageFilter, DefaultParameters)
{
  auto filter = FilterType::New();
  EXPECT_DOUBLE_EQ(filter->GetGaussianSigma(), 1.5);
  EXPECT_DOUBLE_EQ(filter->GetK1(), 0.01);
  EXPECT_DOUBLE_EQ(filter->GetK2(), 0.03);
  EXPECT_DOUBLE_EQ(filter->GetLuminanceExponent(), 1.0);
  EXPECT_DOUBLE_EQ(filter->GetContrastExponent(), 1.0);
  EXPECT_DOUBLE_EQ(filter->GetStructureExponent(), 1.0);
  EXPECT_EQ(filter->GetScaleWeights(), FilterType::WangEtAl2003ScaleWeights());
  // For PixelType=double the default dynamic range is 1.0.
  EXPECT_DOUBLE_EQ(filter->GetDynamicRange(), 1.0);
}

TEST(StructuralSimilarityImageFilter, WangEtAl2003ScaleWeights)
{
  const auto weights = FilterType::WangEtAl2003ScaleWeights();
  ASSERT_EQ(weights.GetSize(), 5u);
  EXPECT_DOUBLE_EQ(weights[0], 0.0448);
  EXPECT_DOUBLE_EQ(weights[1], 0.2856);
  EXPECT_DOUBLE_EQ(weights[2], 0.3001);
  EXPECT_DOUBLE_EQ(weights[3], 0.2363);
  EXPECT_DOUBLE_EQ(weights[4], 0.1333);
}

TEST(StructuralSimilarityImageFilter, GaussianScaleWeights)
{
  const auto odd = FilterType::GaussianScaleWeights(5, 1.0);
  ASSERT_EQ(odd.GetSize(), 5u);
  EXPECT_NEAR(odd[0], 0.05448868454964294, 1e-15);
  EXPECT_NEAR(odd[1], 0.24420134200323332, 1e-15);
  EXPECT_NEAR(odd[2], 0.4026199468942474, 1e-15);
  EXPECT_NEAR(odd[3], 0.24420134200323332, 1e-15);
  EXPECT_NEAR(odd[4], 0.05448868454964294, 1e-15);

  for (const unsigned int size : { 4u, 5u })
  {
    const auto weights = FilterType::GaussianScaleWeights(size, 1.0);
    double     sum = 0.0;
    for (unsigned int k = 0; k < size; ++k)
    {
      EXPECT_DOUBLE_EQ(weights[k], weights[size - 1 - k]);
      sum += weights[k];
    }
    EXPECT_NEAR(sum, 1.0, 1e-15);
  }

  EXPECT_THROW(FilterType::GaussianScaleWeights(5, 0.0), itk::ExceptionObject);
  EXPECT_THROW(FilterType::GaussianScaleWeights(5, -1.0), itk::ExceptionObject);
  EXPECT_THROW(FilterType::GaussianScaleWeights(5, std::numeric_limits<double>::quiet_NaN()), itk::ExceptionObject);
}

TEST(StructuralSimilarityImageFilter, SetGetParameters)
{
  auto filter = FilterType::New();
  filter->SetGaussianSigma(2.0);
  EXPECT_DOUBLE_EQ(filter->GetGaussianSigma(), 2.0);
  filter->SetK1(0.02);
  EXPECT_DOUBLE_EQ(filter->GetK1(), 0.02);
  filter->SetK2(0.04);
  EXPECT_DOUBLE_EQ(filter->GetK2(), 0.04);
  filter->SetDynamicRange(255.0);
  EXPECT_DOUBLE_EQ(filter->GetDynamicRange(), 255.0);
  filter->SetLuminanceExponent(2.0);
  EXPECT_DOUBLE_EQ(filter->GetLuminanceExponent(), 2.0);
  filter->SetContrastExponent(0.5);
  EXPECT_DOUBLE_EQ(filter->GetContrastExponent(), 0.5);
  filter->SetStructureExponent(0.7);
  EXPECT_DOUBLE_EQ(filter->GetStructureExponent(), 0.7);

  FilterType::ScaleWeightsType weights(3);
  weights[0] = 0.4;
  weights[1] = 0.4;
  weights[2] = 0.2;
  filter->SetScaleWeights(weights);
  EXPECT_EQ(filter->GetScaleWeights().GetSize(), 3u);
  EXPECT_DOUBLE_EQ(filter->GetScaleWeights()[0], 0.4);
  EXPECT_DOUBLE_EQ(filter->GetScaleWeights()[2], 0.2);
}


// ===========================================================================
// Mathematical identities (kernel-implementation independent)
// ===========================================================================

TEST(StructuralSimilarityImageFilter, IdenticalConstantImagesYieldOne)
{
  auto image = MakeConstantImage(100.0, 64);
  auto filter = FilterType::New();
  filter->SetInput1(image);
  filter->SetInput2(image);
  filter->SetDynamicRange(255.0);
  filter->Update();
  EXPECT_NEAR(filter->GetMeanSSIM(), 1.0, 1e-9);
}

TEST(StructuralSimilarityImageFilter, IdenticalRandomImagesYieldOne)
{
  auto image = MakeRandomImage(64, 42);
  auto filter = FilterType::New();
  filter->SetInput1(image);
  filter->SetInput2(image);
  filter->SetDynamicRange(255.0);
  filter->Update();
  EXPECT_NEAR(filter->GetMeanSSIM(), 1.0, 1e-9);
}

TEST(StructuralSimilarityImageFilter, IdenticalGradientImagesYieldOne)
{
  auto image = MakeGradientImage(64);
  auto filter = FilterType::New();
  filter->SetInput1(image);
  filter->SetInput2(image);
  filter->SetDynamicRange(255.0);
  filter->Update();
  EXPECT_NEAR(filter->GetMeanSSIM(), 1.0, 1e-9);
}

TEST(StructuralSimilarityImageFilter, SymmetryProperty)
{
  // SSIM is symmetric: SSIM(a,b) == SSIM(b,a) up to floating-point round-off.
  auto a = MakeRandomImage(48, 11);
  auto b = MakeRandomImage(48, 22);
  auto filter1 = FilterType::New();
  filter1->SetInput1(a);
  filter1->SetInput2(b);
  filter1->SetDynamicRange(255.0);
  filter1->Update();
  const double s_ab = filter1->GetMeanSSIM();

  auto filter2 = FilterType::New();
  filter2->SetInput1(b);
  filter2->SetInput2(a);
  filter2->SetDynamicRange(255.0);
  filter2->Update();
  const double s_ba = filter2->GetMeanSSIM();

  EXPECT_NEAR(s_ab, s_ba, 1e-12);
}


// ===========================================================================
// Closed-form analytic checks for constant inputs
// ===========================================================================

// SSIM(constant_a, constant_b) reduces to the luminance term only because
// variances and covariance vanish:
//
//   SSIM = (2*a*b + C1) / (a^2 + b^2 + C1)
//
// (the contrast and structure terms each become 1).
static double
AnalyticConstantSSIM(double a, double b, double K1 = 0.01, double L = 255.0)
{
  const double C1 = (K1 * L) * (K1 * L);
  return (2 * a * b + C1) / (a * a + b * b + C1);
}

TEST(StructuralSimilarityImageFilter, TwoDifferentConstantImages_AnalyticMatch)
{
  // (100, 150) -> approx 0.92310
  auto a = MakeConstantImage(100.0);
  auto b = MakeConstantImage(150.0);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetDynamicRange(255.0);
  filter->SetScaleWeights(FilterType::ScaleWeightsType(1, 1.0));
  filter->Update();

  const double expected = AnalyticConstantSSIM(100.0, 150.0);
  EXPECT_NEAR(filter->GetMeanSSIM(), expected, 1e-9);
  EXPECT_NEAR(expected, 0.9230923, 1e-6); // sanity-check the analytic helper
}

TEST(StructuralSimilarityImageFilter, MaximallyDifferentConstants_AnalyticMatch)
{
  // (0, 255) with K1=0.01, L=255 gives the textbook 0.0000999900
  auto a = MakeConstantImage(0.0);
  auto b = MakeConstantImage(255.0);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetDynamicRange(255.0);
  filter->SetScaleWeights(FilterType::ScaleWeightsType(1, 1.0));
  filter->Update();

  const double expected = AnalyticConstantSSIM(0.0, 255.0);
  EXPECT_NEAR(filter->GetMeanSSIM(), expected, 1e-9);
  EXPECT_NEAR(expected, 0.0000999900, 1e-9);
}

TEST(StructuralSimilarityImageFilter, ConstantImages_PerPixelMapValueIsAnalytic)
{
  // Verify the per-pixel SSIM map is the analytic constant value everywhere.
  auto a = MakeConstantImage(80.0, 32);
  auto b = MakeConstantImage(120.0, 32);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetDynamicRange(255.0);
  filter->Update();

  const double expected = AnalyticConstantSSIM(80.0, 120.0);

  using OutputImageType = FilterType::OutputImageType;
  itk::ImageRegionConstIterator<OutputImageType> it(filter->GetOutput(), filter->GetOutput()->GetBufferedRegion());
  unsigned int                                   sampledCount = 0;
  for (; !it.IsAtEnd(); ++it)
  {
    EXPECT_NEAR(static_cast<double>(it.Get()), expected, 1e-6);
    ++sampledCount;
  }
  EXPECT_EQ(sampledCount, 32u * 32u);
}


// ===========================================================================
// Output geometry / data-flow correctness
// ===========================================================================

TEST(StructuralSimilarityImageFilter, OutputMapHasSameSizeAsInput)
{
  constexpr unsigned int sz = 24;
  auto                   a = MakeConstantImage(100.0, sz);
  auto                   b = MakeConstantImage(100.0, sz);
  auto                   filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->Update();
  EXPECT_EQ(filter->GetOutput()->GetLargestPossibleRegion().GetSize()[0], sz);
  EXPECT_EQ(filter->GetOutput()->GetLargestPossibleRegion().GetSize()[1], sz);
}

TEST(StructuralSimilarityImageFilter, MismatchedInputSizes_Throws)
{
  auto a = MakeConstantImage(100.0, 32);
  auto b = MakeConstantImage(100.0, 48);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
}

TEST(StructuralSimilarityImageFilter, MissingInputs_Throws)
{
  auto a = MakeConstantImage(100.0, 32);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
}

TEST(StructuralSimilarityImageFilter, NonPositiveSigma_Throws)
{
  auto a = MakeConstantImage(100.0, 32);
  auto b = MakeConstantImage(100.0, 32);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetGaussianSigma(0.0);
  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
}

TEST(StructuralSimilarityImageFilter, NonPositiveDynamicRange_Throws)
{
  auto a = MakeConstantImage(100.0, 32);
  auto b = MakeConstantImage(100.0, 32);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetDynamicRange(0.0);
  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
}

TEST(StructuralSimilarityImageFilter, MultiScaleConstantImage)
{
  auto a = MakeConstantImage(100.0, 32);
  auto b = MakeConstantImage(100.0, 32);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  FilterType::ScaleWeightsType weights(5);
  weights.Fill(0.2);
  filter->SetScaleWeights(weights);
  filter->Update();
  EXPECT_NEAR(filter->GetMeanSSIM(), 1.0, 10e-9);
}

TEST(StructuralSimilarityImageFilter, EmptyScaleWeights_Throws)
{
  auto a = MakeConstantImage(100.0, 32);
  auto b = MakeConstantImage(100.0, 32);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  FilterType::ScaleWeightsType empty;
  filter->SetScaleWeights(empty);
  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
}

TEST(StructuralSimilarityImageFilter, InvalidScaleWeights_Throw)
{
  auto a = MakeConstantImage(100.0, 32);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(a);
  for (const double invalid :
       { -0.1, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::infinity() })
  {
    auto weights = FilterType::WangEtAl2003ScaleWeights();
    weights[2] = invalid;
    filter->SetScaleWeights(weights);
    EXPECT_THROW(filter->Update(), itk::ExceptionObject);
  }
}

TEST(StructuralSimilarityImageFilter, ImageTooSmallForNumberOfScales_Throws)
{
  // 5 scales need at least 2^4 = 16 pixels per dimension.
  auto a = MakeConstantImage(100.0, 15);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(a);
  EXPECT_THROW(filter->Update(), itk::ExceptionObject);

  filter->SetScaleWeights(FilterType::ScaleWeightsType(4, 0.25));
  EXPECT_NO_THROW(filter->Update());
}

TEST(StructuralSimilarityImageFilter, OddStartIndexNeedsLargerImage)
{
  // Starting at index 1, each BinShrinkImageFilter halving drops the first
  // pixel: 30 -> 14 -> 6 -> 2 -> 0, but 31 -> 15 -> 7 -> 3 -> 1.
  for (const unsigned int size : { 30u, 31u })
  {
    auto image = MakeConstantImage(100.0, size);
    image->SetRegions(ImageType::RegionType({ { 1, 1 } }, ImageType::SizeType::Filled(size)));
    auto filter = FilterType::New();
    filter->SetInput1(image);
    filter->SetInput2(image);
    if (size == 30u)
    {
      EXPECT_THROW(filter->Update(), itk::ExceptionObject);
    }
    else
    {
      EXPECT_NO_THROW(filter->Update());
      EXPECT_NEAR(filter->GetMeanSSIM(), 1.0, 1e-12);
    }
  }
}

TEST(StructuralSimilarityImageFilter, MeanSSIMCombinesPerScaleValues)
{
  auto a = MakeGradientImage(64);
  auto b = NoisyCopy(a, 20.0, 7);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetDynamicRange(255.0);
  filter->Update();

  const auto & weights = filter->GetScaleWeights();
  const auto & ssim = filter->GetSSIMPerScale();
  const auto & cs = filter->GetContrastStructurePerScale();
  ASSERT_EQ(ssim.GetSize(), weights.GetSize());
  ASSERT_EQ(cs.GetSize(), weights.GetSize());

  const unsigned int last = weights.GetSize() - 1;
  double             expected = std::pow(std::max(ssim[last], 1e-6), weights[last]);
  for (unsigned int j = 0; j < last; ++j)
  {
    EXPECT_LT(cs[j], 1.0);
    expected *= std::pow(std::max(cs[j], 1e-6), weights[j]);
  }
  EXPECT_NEAR(filter->GetMeanSSIM(), expected, 1e-12);
}

TEST(StructuralSimilarityImageFilter, EvenStartIndex_SameResult)
{
  auto a = MakeGradientImage(50);
  auto b = NoisyCopy(a, 20.0, 9);

  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetDynamicRange(255.0);
  filter->Update();
  const double expected = filter->GetMeanSSIM();

  const auto region = a->GetLargestPossibleRegion();
  auto       shiftedRegion = region;
  // BinShrinkImageFilter aligns bins to multiples of the shrink factor; 16 = 2^4 covers 5 scales.
  shiftedRegion.SetIndex({ { 16, -32 } });
  for (auto & image : { a, b })
  {
    image->SetRegions(shiftedRegion);
  }

  auto shiftedFilter = FilterType::New();
  shiftedFilter->SetInput1(a);
  shiftedFilter->SetInput2(b);
  shiftedFilter->SetDynamicRange(255.0);
  shiftedFilter->Update();
  EXPECT_NEAR(shiftedFilter->GetMeanSSIM(), expected, 1e-12);
  EXPECT_EQ(shiftedFilter->GetOutput()->GetLargestPossibleRegion(), shiftedRegion);
}


// ===========================================================================
// Range / monotonicity / qualitative properties
// ===========================================================================

TEST(StructuralSimilarityImageFilter, RandomPair_SSIMInValidRange)
{
  auto a = MakeRandomImage(64, 1);
  auto b = MakeRandomImage(64, 2);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetDynamicRange(255.0);
  filter->Update();
  const double s = filter->GetMeanSSIM();
  EXPECT_GE(s, -1.0);
  EXPECT_LE(s, 1.0);
}

TEST(StructuralSimilarityImageFilter, MoreNoiseLowersSSIM)
{
  auto base = MakeRandomImage(64, 5);
  auto a_low = NoisyCopy(base, 2.0, 17);
  auto a_mid = NoisyCopy(base, 8.0, 17);
  auto a_hi = NoisyCopy(base, 24.0, 17);

  const double s_low = ComputeFiltered(base, a_low);
  const double s_mid = ComputeFiltered(base, a_mid);
  const double s_hi = ComputeFiltered(base, a_hi);

  EXPECT_GT(s_low, s_mid);
  EXPECT_GT(s_mid, s_hi);
  EXPECT_LT(s_low, 1.0);
}

TEST(StructuralSimilarityImageFilter, NegatedImage_StronglyAntiCorrelated)
{
  auto base = MakeRandomImage(64, 99);
  auto neg = ScaledCopy(base, -1.0, 255.0); // 255 - x

  auto filter = FilterType::New();
  filter->SetInput1(base);
  filter->SetInput2(neg);
  filter->SetDynamicRange(255.0);
  filter->Update();
  EXPECT_NEAR(filter->GetMeanSSIM(), 0.0, 1e-3);

  // Single-scale SSIM is not floored, so anti-correlation shows as a negative score.
  filter->SetScaleWeights(FilterType::ScaleWeightsType(1, 1.0));
  filter->Update();
  EXPECT_LT(filter->GetMeanSSIM(), -0.5);
  EXPECT_DOUBLE_EQ(filter->GetMeanSSIM(), filter->GetSSIMPerScale()[0]);
}


// ===========================================================================
// Single-scale regression values on synthetic images
// ===========================================================================

TEST(StructuralSimilarityImageFilter, GradientShiftedByConstant)
{
  auto a = MakeGradientImage(64);
  auto b = ScaledCopy(a, 1.0, 30.0);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetDynamicRange(255.0);
  filter->SetScaleWeights(FilterType::ScaleWeightsType(1, 1.0));
  filter->Update();
  EXPECT_NEAR(filter->GetMeanSSIM(), 0.95788000569951843, 1e-9);
}

TEST(StructuralSimilarityImageFilter, GradientHalfContrast)
{
  auto a = MakeGradientImage(64);
  auto b = ScaledCopy(a, 0.5, 0.0);
  auto filter = FilterType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetDynamicRange(255.0);
  filter->SetScaleWeights(FilterType::ScaleWeightsType(1, 1.0));
  filter->Update();
  EXPECT_NEAR(filter->GetMeanSSIM(), 0.75807280867093147, 1e-9);
}


// ===========================================================================
// Code-path equivalence: simplified vs general formula
// ===========================================================================

TEST(StructuralSimilarityImageFilter, SimplifiedAndGeneralFormulaAgreeWhenExponentsAreOne)
{
  // With alpha=beta=gamma=1 the filter takes the simplified fast path.  We
  // can force the general (std::pow) path by setting an exponent to a value
  // *almost equal* to 1, which is treated as different and disables the
  // FloatAlmostEqual fast-path check.  In practice setting all exponents to
  // exactly 1.0 (default) and another filter with values that differ by
  // 1e-12 should give numerically equivalent answers.
  auto a = MakeRandomImage(48, 33);
  auto b = NoisyCopy(a, 5.0, 44);

  auto fastFilter = FilterType::New();
  fastFilter->SetInput1(a);
  fastFilter->SetInput2(b);
  fastFilter->SetDynamicRange(255.0);
  fastFilter->Update();
  const double fastResult = fastFilter->GetMeanSSIM();

  auto generalFilter = FilterType::New();
  generalFilter->SetInput1(a);
  generalFilter->SetInput2(b);
  generalFilter->SetDynamicRange(255.0);
  // Tiny perturbation forces the general code path.
  generalFilter->SetLuminanceExponent(1.0 + 1e-9);
  generalFilter->SetContrastExponent(1.0);
  generalFilter->SetStructureExponent(1.0);
  generalFilter->Update();
  const double generalResult = generalFilter->GetMeanSSIM();

  EXPECT_NEAR(fastResult, generalResult, 1e-6);
}


// ===========================================================================
// Multi-dimensional support (3D and 4D)
// ===========================================================================

TEST(StructuralSimilarityImageFilter, ThreeDimensional_IdenticalRandomYieldsOne)
{
  using Image3DType = itk::Image<double, 3>;
  using Filter3DType = itk::StructuralSimilarityImageFilter<Image3DType>;

  auto                  image = Image3DType::New();
  Image3DType::SizeType size;
  size.Fill(16);
  image->SetRegions(Image3DType::RegionType(size));
  image->Allocate();

  std::mt19937                           gen(11);
  std::uniform_real_distribution<double> dist(0.0, 255.0);
  itk::ImageRegionIterator<Image3DType>  it(image, image->GetLargestPossibleRegion());
  for (; !it.IsAtEnd(); ++it)
  {
    it.Set(dist(gen));
  }

  auto filter = Filter3DType::New();
  filter->SetInput1(image);
  filter->SetInput2(image);
  filter->SetDynamicRange(255.0);
  filter->Update();
  EXPECT_NEAR(filter->GetMeanSSIM(), 1.0, 1e-9);
}

TEST(StructuralSimilarityImageFilter, ThreeDimensional_ConstantInputs_AnalyticMatch)
{
  using Image3DType = itk::Image<double, 3>;
  using Filter3DType = itk::StructuralSimilarityImageFilter<Image3DType>;

  auto                  a = Image3DType::New();
  auto                  b = Image3DType::New();
  Image3DType::SizeType size;
  size.Fill(16);
  a->SetRegions(Image3DType::RegionType(size));
  a->Allocate();
  a->FillBuffer(80.0);
  b->SetRegions(Image3DType::RegionType(size));
  b->Allocate();
  b->FillBuffer(120.0);

  auto filter = Filter3DType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetDynamicRange(255.0);
  filter->SetScaleWeights(Filter3DType::ScaleWeightsType(1, 1.0));
  filter->Update();
  EXPECT_NEAR(filter->GetMeanSSIM(), AnalyticConstantSSIM(80.0, 120.0), 1e-9);
}

TEST(StructuralSimilarityImageFilter, FourDimensional_ConstantInputs_AnalyticMatch)
{
  using Image4DType = itk::Image<double, 4>;
  using Filter4DType = itk::StructuralSimilarityImageFilter<Image4DType>;

  auto                  a = Image4DType::New();
  auto                  b = Image4DType::New();
  Image4DType::SizeType size;
  size.Fill(8);
  a->SetRegions(Image4DType::RegionType(size));
  a->Allocate();
  a->FillBuffer(60.0);
  b->SetRegions(Image4DType::RegionType(size));
  b->Allocate();
  b->FillBuffer(80.0);

  auto filter = Filter4DType::New();
  filter->SetInput1(a);
  filter->SetInput2(b);
  filter->SetDynamicRange(255.0);
  filter->SetScaleWeights(Filter4DType::ScaleWeightsType(1, 1.0));
  filter->Update();
  EXPECT_NEAR(filter->GetMeanSSIM(), AnalyticConstantSSIM(60.0, 80.0), 1e-9);
}


// ===========================================================================
// Pixel-type variants (integer types use NumericTraits-based default L)
// ===========================================================================

TEST(StructuralSimilarityImageFilter, UnsignedCharPixelType_DefaultDynamicRangeIs255)
{
  using UCharImage = itk::Image<unsigned char, 2>;
  using UCharFilter = itk::StructuralSimilarityImageFilter<UCharImage>;
  auto filter = UCharFilter::New();
  EXPECT_DOUBLE_EQ(filter->GetDynamicRange(), 255.0);
}

TEST(StructuralSimilarityImageFilter, UnsignedShortPixelType_DefaultDynamicRangeIs65535)
{
  using UShortImage = itk::Image<unsigned short, 2>;
  using UShortFilter = itk::StructuralSimilarityImageFilter<UShortImage>;
  auto filter = UShortFilter::New();
  EXPECT_DOUBLE_EQ(filter->GetDynamicRange(), 65535.0);
}

TEST(StructuralSimilarityImageFilter, FloatPixelType_DefaultDynamicRangeIsOne)
{
  using FloatImage = itk::Image<float, 2>;
  using FloatFilter = itk::StructuralSimilarityImageFilter<FloatImage>;
  auto filter = FloatFilter::New();
  EXPECT_DOUBLE_EQ(filter->GetDynamicRange(), 1.0);
}

TEST(StructuralSimilarityImageFilter, UnsignedCharPixelType_IdenticalYieldsOne)
{
  using UCharImage = itk::Image<unsigned char, 2>;
  using UCharFilter = itk::StructuralSimilarityImageFilter<UCharImage>;

  auto                 image = UCharImage::New();
  UCharImage::SizeType size;
  size.Fill(32);
  image->SetRegions(UCharImage::RegionType(size));
  image->Allocate();
  image->FillBuffer(static_cast<unsigned char>(128));

  auto filter = UCharFilter::New();
  filter->SetInput1(image);
  filter->SetInput2(image);
  filter->Update();
  EXPECT_NEAR(filter->GetMeanSSIM(), 1.0, 1e-9);
}


// ===========================================================================
// Real image regression values (default 5-scale MS-SSIM)
// ===========================================================================

constexpr double realImageTolerance = 1e-8;

TEST(StructuralSimilarityImageFilter, RealImages_Cthead1PngVsJpg)
{
  EXPECT_NEAR(ComputeDefaultMSSSIM(TOSTRING(CTHEAD1_PNG_INPUT), TOSTRING(CTHEAD1_JPG_INPUT)),
              0.99933197130242668,
              realImageTolerance);
}

TEST(StructuralSimilarityImageFilter, RealImages_Cthead1PngVsConnectedComponents)
{
  EXPECT_NEAR(ComputeDefaultMSSSIM(TOSTRING(CTHEAD1_PNG_INPUT), TOSTRING(CTHEAD1_PNG_CC)),
              0.37404742803963448,
              realImageTolerance);
}

TEST(StructuralSimilarityImageFilter, RealImages_Cthead1PngVsItself_YieldsOne)
{
  EXPECT_NEAR(ComputeDefaultMSSSIM(TOSTRING(CTHEAD1_PNG_INPUT), TOSTRING(CTHEAD1_PNG_INPUT)), 1.0, 1e-12);
}

TEST(StructuralSimilarityImageFilter, RealImages_Cthead1PngVsMorphologicalClosing)
{
  EXPECT_NEAR(ComputeDefaultMSSSIM(TOSTRING(CTHEAD1_PNG_INPUT), TOSTRING(CTHEAD1_CLOSING_PNG)),
              0.99243489264191953,
              realImageTolerance);
}

TEST(StructuralSimilarityImageFilter, RealImages_Cthead1PngVsInverted)
{
  // Anti-correlated: three of the five per-scale means are floored at 1e-6.
  EXPECT_NEAR(
    ComputeDefaultMSSSIM(TOSTRING(CTHEAD1_PNG_INPUT), TOSTRING(CTHEAD1_INVERTED_PNG)), 3.2193928432230837e-05, 1e-12);
}

TEST(StructuralSimilarityImageFilter, RealImages_CakeEasyVsHard)
{
  EXPECT_NEAR(
    ComputeDefaultMSSSIM(TOSTRING(CAKE_EASY_PNG), TOSTRING(CAKE_HARD_PNG)), 0.6598890690486825, realImageTolerance);
}

TEST(StructuralSimilarityImageFilter, RealImages_SmoothCircleVsSquare)
{
  EXPECT_NEAR(ComputeDefaultMSSSIM(TOSTRING(SMOOTH_CIRCLE_PNG), TOSTRING(SMOOTH_SQUARE_PNG)),
              0.79535271856765444,
              realImageTolerance);
}

TEST(StructuralSimilarityImageFilter, RealImages_Staple1VsStaple2)
{
  EXPECT_NEAR(
    ComputeDefaultMSSSIM(TOSTRING(STAPLE1_PNG), TOSTRING(STAPLE2_PNG)), 0.78577317632273957, realImageTolerance);
}

TEST(StructuralSimilarityImageFilter, RealImages_Number1VsNumber2InText)
{
  EXPECT_NEAR(
    ComputeDefaultMSSSIM(TOSTRING(NUMBER1_PNG), TOSTRING(NUMBER2_PNG)), 0.48636890005946809, realImageTolerance);
}
