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
#ifndef itkStructuralSimilarityImageFilter_hxx
#define itkStructuralSimilarityImageFilter_hxx

#include "itkBinShrinkImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkMath.h"
#include "itkMultiplyImageFilter.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <mutex>

namespace itk
{
template <typename TInputImage, typename TOutputImage>
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::StructuralSimilarityImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
}

template <typename TInputImage, typename TOutputImage>
auto
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::WangEtAl2003ScaleWeights() -> ScaleWeightsType
{
  constexpr std::array<RealType, 5> weights{ 0.0448, 0.2856, 0.3001, 0.2363, 0.1333 };
  return ScaleWeightsType(weights.data(), weights.size());
}

template <typename TInputImage, typename TOutputImage>
auto
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::GaussianScaleWeights(unsigned int size, double sigma)
  -> ScaleWeightsType
{
  if (!(sigma > 0.0) || !std::isfinite(sigma))
  {
    itkGenericExceptionMacro("GaussianScaleWeights sigma must be finite and strictly positive (got " << sigma << ").");
  }
  ScaleWeightsType weights(size);
  RealType         sum{};
  const double     center = 0.5 * (static_cast<double>(size) - 1.0);
  for (unsigned int i = 0; i < size; ++i)
  {
    const double offset = static_cast<double>(i) - center;
    weights[i] = static_cast<RealType>(std::exp(-(offset * offset) / (2.0 * sigma * sigma)));
    sum += weights[i];
  }
  if (sum > RealType{})
  {
    weights /= sum;
  }
  return weights;
}

template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::SetInput2(const InputImageType * image)
{
  this->SetNthInput(1, const_cast<InputImageType *>(image));
}

template <typename TInputImage, typename TOutputImage>
auto
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::GetInput2() const -> const InputImageType *
{
  return itkDynamicCastInDebugMode<const InputImageType *>(this->ProcessObject::GetInput(1));
}

template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::SetScaleWeights(const ScaleWeightsType & weights)
{
  if (m_ScaleWeights != weights)
  {
    m_ScaleWeights = weights;
    this->Modified();
  }
}

template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject * data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::VerifyPreconditions() const
{
  Superclass::VerifyPreconditions();

  if (m_ScaleWeights.GetSize() == 0)
  {
    itkExceptionMacro("ScaleWeights array must contain at least one element.");
  }
  for (unsigned int scale = 0; scale < m_ScaleWeights.GetSize(); ++scale)
  {
    if (!std::isfinite(m_ScaleWeights[scale]) || m_ScaleWeights[scale] < RealType{})
    {
      itkExceptionMacro("ScaleWeights must be finite and non-negative (got " << m_ScaleWeights[scale] << " at scale "
                                                                             << scale << ").");
    }
  }
  if (m_GaussianSigma <= 0.0)
  {
    itkExceptionMacro("GaussianSigma must be strictly positive (got " << m_GaussianSigma << ").");
  }
  if (m_DynamicRange <= 0.0)
  {
    itkExceptionMacro("DynamicRange must be strictly positive (got " << m_DynamicRange << ").");
  }

  const InputImageType * input1 = this->GetInput1();
  const InputImageType * input2 = this->GetInput2();
  if (input1 == nullptr || input2 == nullptr)
  {
    itkExceptionMacro("StructuralSimilarityImageFilter requires both inputs to be set.");
  }
  if (input1->GetLargestPossibleRegion() != input2->GetLargestPossibleRegion())
  {
    itkExceptionMacro("StructuralSimilarityImageFilter requires the two inputs to have identical regions.");
  }

  // Mirrors BinShrinkImageFilter::GenerateOutputInformation(), whose output
  // region depends on the start index as well as on the size, to avoid a crash later.
  const auto & region = input1->GetLargestPossibleRegion();
  for (unsigned int d = 0; d < ImageDimension; ++d)
  {
    IndexValueType coarsestIndex = region.GetIndex(d);
    SizeValueType  coarsestSize = region.GetSize(d);
    for (unsigned int scale = 1; scale < m_ScaleWeights.GetSize() && coarsestSize > 0; ++scale)
    {
      const auto           shrunkIndex = Math::Ceil<IndexValueType>(coarsestIndex / 2.0);
      const IndexValueType remaining = static_cast<IndexValueType>(coarsestSize) + coarsestIndex - 2 * shrunkIndex;
      coarsestSize = static_cast<SizeValueType>(remaining / 2);
      coarsestIndex = shrunkIndex;
    }
    if (coarsestSize == 0)
    {
      itkExceptionMacro("Image region " << region << " along dimension " << d << " is too small for "
                                        << m_ScaleWeights.GetSize() << " scales.");
    }
  }
}

template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  using RealImageType = Image<RealType, ImageDimension>;
  using RealImagePointer = typename RealImageType::Pointer;
  using RealImageRegionType = typename RealImageType::RegionType;
  using CastFilterType = CastImageFilter<InputImageType, RealImageType>;
  using MultiplyFilterType = MultiplyImageFilter<RealImageType, RealImageType, RealImageType>;
  using SmoothingFilterType = DiscreteGaussianImageFilter<RealImageType, RealImageType>;
  using ShrinkFilterType = BinShrinkImageFilter<RealImageType, RealImageType>;

  const unsigned int numberOfScales = m_ScaleWeights.GetSize();

  const auto smooth = [](const RealImageType * image, double sigma) {
    auto smoother = SmoothingFilterType::New();
    smoother->SetInput(image);
    smoother->SetVariance(sigma * sigma);
    smoother->Update();
    RealImagePointer smoothed = smoother->GetOutput();
    smoothed->DisconnectPipeline();
    return smoothed;
  };
  // Grafting the inputs keeps the internal Update() calls from propagating upstream.
  const auto toRealImage = [](const InputImageType * input) {
    auto grafted = InputImageType::New();
    grafted->Graft(input);
    auto cast = CastFilterType::New();
    cast->SetInput(grafted);
    cast->Update();
    RealImagePointer image = cast->GetOutput();
    image->DisconnectPipeline();
    return image;
  };
  const auto multiply = [](const RealImageType * a, const RealImageType * b) {
    auto filter = MultiplyFilterType::New();
    filter->SetInput1(a);
    filter->SetInput2(b);
    filter->Update();
    RealImagePointer product = filter->GetOutput();
    product->DisconnectPipeline();
    return product;
  };
  const auto shrink = [](const RealImageType * image) {
    auto filter = ShrinkFilterType::New();
    filter->SetInput(image);
    filter->SetShrinkFactors(2);
    filter->Update();
    RealImagePointer shrunk = filter->GetOutput();
    shrunk->DisconnectPipeline();
    return shrunk;
  };

  RealImagePointer x = toRealImage(this->GetInput1());
  RealImagePointer y = toRealImage(this->GetInput2());

  OutputImageType * output = this->GetOutput();
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  const RealType C1 = Math::sqr(static_cast<RealType>(m_K1 * m_DynamicRange));
  const RealType C2 = Math::sqr(static_cast<RealType>(m_K2 * m_DynamicRange));
  const RealType C3 = C2 / RealType{ 2 };

  const bool unitExponents = Math::FloatAlmostEqual(m_LuminanceExponent, 1.0) &&
                             Math::FloatAlmostEqual(m_ContrastExponent, 1.0) &&
                             Math::FloatAlmostEqual(m_StructureExponent, 1.0);
  const auto alpha = static_cast<RealType>(m_LuminanceExponent);
  const auto beta = static_cast<RealType>(m_ContrastExponent);
  const auto gamma = static_cast<RealType>(m_StructureExponent);

  m_SSIMPerScale.SetSize(numberOfScales);
  m_ContrastStructurePerScale.SetSize(numberOfScales);

  double sigma = m_GaussianSigma;
  for (unsigned int scale = 0; scale < numberOfScales; ++scale, sigma *= 2.0)
  {
    const RealImagePointer mu_x = smooth(x, sigma);
    const RealImagePointer mu_y = smooth(y, sigma);
    const RealImagePointer mu_xx = smooth(multiply(x, x), sigma);
    const RealImagePointer mu_yy = smooth(multiply(y, y), sigma);
    const RealImagePointer mu_xy = smooth(multiply(x, y), sigma);

    const RealImageRegionType region = x->GetBufferedRegion();
    const bool                writeOutput = (scale == 0);

    std::mutex accumulatorMutex;
    RealType   ssimSum{};
    RealType   csSum{};

    this->GetMultiThreader()->template ParallelizeImageRegion<ImageDimension>(
      region,
      [&](const RealImageRegionType & subRegion) {
        ImageRegionConstIterator<RealImageType> muXIt(mu_x, subRegion);
        ImageRegionConstIterator<RealImageType> muYIt(mu_y, subRegion);
        ImageRegionConstIterator<RealImageType> muXXIt(mu_xx, subRegion);
        ImageRegionConstIterator<RealImageType> muYYIt(mu_yy, subRegion);
        ImageRegionConstIterator<RealImageType> muXYIt(mu_xy, subRegion);

        ImageRegionIterator<OutputImageType> outIt;
        if (writeOutput)
        {
          outIt = ImageRegionIterator<OutputImageType>(output, subRegion);
        }

        RealType localSSIMSum{};
        RealType localCSSum{};
        for (; !muXIt.IsAtEnd(); ++muXIt, ++muYIt, ++muXXIt, ++muYYIt, ++muXYIt)
        {
          const RealType mean_x = muXIt.Get();
          const RealType mean_y = muYIt.Get();
          // Round-off can make the variance of a flat region slightly negative.
          const RealType var_x = std::max(muXXIt.Get() - mean_x * mean_x, RealType{});
          const RealType var_y = std::max(muYYIt.Get() - mean_y * mean_y, RealType{});
          const RealType cov_xy = muXYIt.Get() - mean_x * mean_y;

          const RealType l = (RealType{ 2 } * mean_x * mean_y + C1) / (mean_x * mean_x + mean_y * mean_y + C1);
          RealType       cs;
          RealType       ssim;
          if (unitExponents)
          {
            cs = (RealType{ 2 } * cov_xy + C2) / (var_x + var_y + C2);
            ssim = l * cs;
          }
          else
          {
            const RealType sigma_x = std::sqrt(var_x);
            const RealType sigma_y = std::sqrt(var_y);
            const RealType c = (RealType{ 2 } * sigma_x * sigma_y + C2) / (var_x + var_y + C2);
            const RealType s = (cov_xy + C3) / (sigma_x * sigma_y + C3);
            cs = std::pow(c, beta) * std::pow(s, gamma);
            ssim = std::pow(l, alpha) * cs;
          }

          if (writeOutput)
          {
            outIt.Set(static_cast<OutputPixelType>(ssim));
            ++outIt;
          }
          localSSIMSum += ssim;
          localCSSum += cs;
        }

        const std::lock_guard<std::mutex> lock(accumulatorMutex);
        ssimSum += localSSIMSum;
        csSum += localCSSum;
      },
      nullptr);

    const auto numberOfPixels = static_cast<RealType>(region.GetNumberOfPixels());
    m_SSIMPerScale[scale] = ssimSum / numberOfPixels;
    m_ContrastStructurePerScale[scale] = csSum / numberOfPixels;

    if (scale + 1 < numberOfScales)
    {
      x = shrink(x);
      y = shrink(y);
    }
    this->UpdateProgress(static_cast<float>(scale + 1) / static_cast<float>(numberOfScales));
  }

  if (numberOfScales == 1)
  {
    // Plain SSIM, which may legitimately be negative; there is no product to protect.
    m_MeanSSIM = static_cast<double>(m_SSIMPerScale[0]);
    return;
  }

  // Keeps the product finite and non-zero when a per-scale mean is not positive.
  constexpr RealType minimumScaleValue{ 1e-6 };
  const unsigned int coarsest = numberOfScales - 1;

  RealType msssim = std::pow(std::max(m_SSIMPerScale[coarsest], minimumScaleValue), m_ScaleWeights[coarsest]);
  for (unsigned int scale = 0; scale < coarsest; ++scale)
  {
    msssim *= std::pow(std::max(m_ContrastStructurePerScale[scale], minimumScaleValue), m_ScaleWeights[scale]);
  }
  m_MeanSSIM = static_cast<double>(msssim);
}

template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "GaussianSigma: " << m_GaussianSigma << std::endl;
  os << indent << "K1: " << m_K1 << std::endl;
  os << indent << "K2: " << m_K2 << std::endl;
  os << indent << "DynamicRange: " << m_DynamicRange << std::endl;
  os << indent << "LuminanceExponent: " << m_LuminanceExponent << std::endl;
  os << indent << "ContrastExponent: " << m_ContrastExponent << std::endl;
  os << indent << "StructureExponent: " << m_StructureExponent << std::endl;
  os << indent << "ScaleWeights: " << m_ScaleWeights << std::endl;
  os << indent << "MeanSSIM: " << m_MeanSSIM << std::endl;
  os << indent << "SSIMPerScale: " << m_SSIMPerScale << std::endl;
  os << indent << "ContrastStructurePerScale: " << m_ContrastStructurePerScale << std::endl;
}
} // end namespace itk
#endif
