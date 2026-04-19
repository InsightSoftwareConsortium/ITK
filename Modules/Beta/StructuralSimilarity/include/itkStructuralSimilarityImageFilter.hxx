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

#include "itkCastImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkMultiplyImageFilter.h"
#include "itkProgressReporter.h"
#include "itkTotalProgressReporter.h"

#include <algorithm>
#include <atomic>
#include <cmath>

namespace itk
{
template <typename TInputImage, typename TOutputImage>
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::StructuralSimilarityImageFilter()
{
  this->SetNumberOfRequiredInputs(2);

  // Default ScaleWeights = [1.0] => single-scale SSIM.
  m_ScaleWeights.SetSize(1);
  m_ScaleWeights[0] = static_cast<RealType>(1.0);
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
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // The mean SSIM is computed over the full output, so the filter needs the
  // entire largest possible region of both inputs.  This mirrors
  // SimilarityIndexImageFilter and avoids subtle errors when the filter is
  // pipelined behind a streaming filter.
  Superclass::GenerateInputRequestedRegion();

  for (unsigned int i = 0; i < 2; ++i)
  {
    auto * input = const_cast<InputImageType *>(this->GetInput(i));
    if (input)
    {
      input->SetRequestedRegionToLargestPossibleRegion();
    }
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
  if (m_ScaleWeights.GetSize() > 1)
  {
    itkExceptionMacro("Multi-scale SSIM (MS-SSIM) with ScaleWeights size > 1 is not yet implemented.  "
                      "Set a ScaleWeights array of length 1 to compute single-scale SSIM.");
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
}

template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  // ---- Build the internal Gaussian-statistics pipeline -----------------
  //
  // Following the composite-filter pattern (see CompositeFilterExample.cxx),
  // we reconnect the mini-pipeline inputs from the external pipeline every
  // time GenerateData() is called.  This ensures the internal sub-filters
  // pick up any new inputs when the external pipeline re-executes.
  using RealImageType = Image<RealType, ImageDimension>;
  using CastFilterType = CastImageFilter<InputImageType, RealImageType>;
  using MultiplyFilterType = MultiplyImageFilter<RealImageType, RealImageType, RealImageType>;
  using GaussianFilterType = DiscreteGaussianImageFilter<RealImageType, RealImageType>;

  // Graft external inputs to disconnect the mini-pipeline from the
  // upstream pipeline, preventing the internal Update() calls from
  // propagating back upstream.
  auto graftedInput1 = InputImageType::New();
  graftedInput1->Graft(this->GetInput1());

  auto graftedInput2 = InputImageType::New();
  graftedInput2->Graft(this->GetInput2());

  auto cast1 = CastFilterType::New();
  cast1->SetInput(graftedInput1);

  auto cast2 = CastFilterType::New();
  cast2->SetInput(graftedInput2);

  auto x_times_x = MultiplyFilterType::New();
  x_times_x->SetInput1(cast1->GetOutput());
  x_times_x->SetInput2(cast1->GetOutput());

  auto y_times_y = MultiplyFilterType::New();
  y_times_y->SetInput1(cast2->GetOutput());
  y_times_y->SetInput2(cast2->GetOutput());

  auto x_times_y = MultiplyFilterType::New();
  x_times_y->SetInput1(cast1->GetOutput());
  x_times_y->SetInput2(cast2->GetOutput());

  // Configure a Gaussian filter with the requested sigma and kernel-width
  // ceiling.  We disable image-spacing-aware sigmas: SSIM is defined in
  // pixel coordinates and the canonical reference (Wang 2004) uses an
  // 11x11 unit-spacing window.
  auto makeGaussian = [this](const auto & inputPort) {
    auto g = GaussianFilterType::New();
    g->SetInput(inputPort);
    g->SetSigma(m_GaussianSigma);
    g->SetMaximumKernelWidth(m_MaximumKernelWidth);
    g->SetUseImageSpacing(false);
    g->ReleaseDataFlagOn();
    return g;
  };

  auto g_x = makeGaussian(cast1->GetOutput());
  auto g_y = makeGaussian(cast2->GetOutput());
  auto g_xx = makeGaussian(x_times_x->GetOutput());
  auto g_yy = makeGaussian(y_times_y->GetOutput());
  auto g_xy = makeGaussian(x_times_y->GetOutput());

  g_x->Update();
  g_y->Update();
  g_xx->Update();
  g_yy->Update();
  g_xy->Update();

  // ---- Allocate output --------------------------------------------------
  OutputImageType * output = this->GetOutput();
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  const auto outputRegion = output->GetRequestedRegion();

  // ---- Pre-compute SSIM constants --------------------------------------
  const RealType K1 = static_cast<RealType>(m_K1);
  const RealType K2 = static_cast<RealType>(m_K2);
  const RealType L = static_cast<RealType>(m_DynamicRange);
  const RealType C1 = (K1 * L) * (K1 * L);
  const RealType C2 = (K2 * L) * (K2 * L);
  const RealType C3 = C2 / static_cast<RealType>(2);

  const bool useSimplifiedFormula = Math::FloatAlmostEqual(m_LuminanceExponent, 1.0) &&
                                    Math::FloatAlmostEqual(m_ContrastExponent, 1.0) &&
                                    Math::FloatAlmostEqual(m_StructureExponent, 1.0);

  const auto alpha = static_cast<RealType>(m_LuminanceExponent);
  const auto beta = static_cast<RealType>(m_ContrastExponent);
  const auto gamma = static_cast<RealType>(m_StructureExponent);

  const RealImageType * gx = g_x->GetOutput();
  const RealImageType * gy = g_y->GetOutput();
  const RealImageType * gxx = g_xx->GetOutput();
  const RealImageType * gyy = g_yy->GetOutput();
  const RealImageType * gxy = g_xy->GetOutput();

  // ---- Determine the "valid" interior region for mean SSIM ------------
  //
  // Pixels within the half-width of the discrete Gaussian kernel of the
  // image boundary use boundary-extended values inside the convolution and
  // are therefore less reliable.  scikit-image (matching the MATLAB
  // reference) crops by (win_size - 1)/2 before averaging.  We do the same.
  const auto kernelHalfWidth = static_cast<SizeValueType>(m_MaximumKernelWidth / 2);
  auto       interiorRegion = outputRegion;
  bool       interiorIsValid = true;
  for (unsigned int d = 0; d < ImageDimension; ++d)
  {
    const auto sz = interiorRegion.GetSize(d);
    if (sz <= 2 * kernelHalfWidth)
    {
      // Image is too small to crop -- mean SSIM falls back to the entire
      // output region.
      interiorIsValid = false;
      break;
    }
  }
  if (interiorIsValid)
  {
    auto idx = interiorRegion.GetIndex();
    auto sz = interiorRegion.GetSize();
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      idx[d] += static_cast<IndexValueType>(kernelHalfWidth);
      sz[d] -= 2 * kernelHalfWidth;
    }
    interiorRegion.SetIndex(idx);
    interiorRegion.SetSize(sz);
  }

  // ---- Parallelized per-pixel SSIM combination ------------------------
  //
  // Each thread accumulates a partial sum and pixel count over its
  // sub-region.  Atomic doubles aren't portable in C++17, so we serialize
  // the small per-thread reductions through a mutex.

  std::mutex    accumulatorMutex;
  RealType      accumulator{};
  SizeValueType accumulatorCount{};

  TotalProgressReporter progress(this, outputRegion.GetNumberOfPixels());

  this->GetMultiThreader()->template ParallelizeImageRegion<ImageDimension>(
    outputRegion,
    [&](const OutputImageRegionType & subRegion) {
      // Use ImageRegionIteratorWithIndex for the output so we can call
      // GetIndex() inside the inner loop without hitting the
      // ITK_LEGACY_REMOVE deprecation on the index-less iterator's
      // GetIndex() (#6034 CI fix).
      using OutputIteratorType = ImageRegionIteratorWithIndex<OutputImageType>;
      using RealConstIteratorType = ImageRegionConstIterator<RealImageType>;

      OutputIteratorType    outIt(output, subRegion);
      RealConstIteratorType gxIt(gx, subRegion);
      RealConstIteratorType gyIt(gy, subRegion);
      RealConstIteratorType gxxIt(gxx, subRegion);
      RealConstIteratorType gyyIt(gyy, subRegion);
      RealConstIteratorType gxyIt(gxy, subRegion);

      // Per-thread accumulators for the *interior* portion of this region.
      const auto subInterior = [&]() {
        OutputImageRegionType r = subRegion;
        if (interiorIsValid)
        {
          if (!r.Crop(interiorRegion))
          {
            r.SetSize(SizeType{}); // empty
          }
        }
        return r;
      }();

      RealType      localSum{};
      SizeValueType localCount{};

      for (outIt.GoToBegin(),
           gxIt.GoToBegin(),
           gyIt.GoToBegin(),
           gxxIt.GoToBegin(),
           gyyIt.GoToBegin(),
           gxyIt.GoToBegin();
           !outIt.IsAtEnd();
           ++outIt, ++gxIt, ++gyIt, ++gxxIt, ++gyyIt, ++gxyIt)
      {
        const RealType mu_x = gxIt.Get();
        const RealType mu_y = gyIt.Get();
        const RealType mu_xx = gxxIt.Get();
        const RealType mu_yy = gyyIt.Get();
        const RealType mu_xy = gxyIt.Get();

        const RealType var_x = mu_xx - mu_x * mu_x;
        const RealType var_y = mu_yy - mu_y * mu_y;
        const RealType cov_xy = mu_xy - mu_x * mu_y;

        // Numerical floor: floating-point round-off can produce a tiny
        // negative variance for nearly-constant regions.
        const RealType var_x_clipped = std::max(var_x, RealType{});
        const RealType var_y_clipped = std::max(var_y, RealType{});

        RealType ssim;
        if (useSimplifiedFormula)
        {
          const RealType numerator = (RealType{ 2 } * mu_x * mu_y + C1) * (RealType{ 2 } * cov_xy + C2);
          const RealType denominator = (mu_x * mu_x + mu_y * mu_y + C1) * (var_x_clipped + var_y_clipped + C2);
          ssim = numerator / denominator;
        }
        else
        {
          const RealType l_num = RealType{ 2 } * mu_x * mu_y + C1;
          const RealType l_den = mu_x * mu_x + mu_y * mu_y + C1;
          const RealType l = l_num / l_den;

          const RealType sigma_x = std::sqrt(var_x_clipped);
          const RealType sigma_y = std::sqrt(var_y_clipped);

          const RealType c_num = RealType{ 2 } * sigma_x * sigma_y + C2;
          const RealType c_den = var_x_clipped + var_y_clipped + C2;
          const RealType c = c_num / c_den;

          const RealType s_num = cov_xy + C3;
          const RealType s_den = sigma_x * sigma_y + C3;
          const RealType s = s_num / s_den;

          ssim = std::pow(l, alpha) * std::pow(c, beta) * std::pow(s, gamma);
        }

        outIt.Set(static_cast<OutputPixelType>(ssim));

        // Only accumulate over the interior region for the mean.
        if (subInterior.GetNumberOfPixels() > 0 && subInterior.IsInside(outIt.GetIndex()))
        {
          localSum += ssim;
          ++localCount;
        }
      }

      progress.Completed(subRegion.GetNumberOfPixels());

      {
        const std::lock_guard<std::mutex> lock(accumulatorMutex);
        accumulator += localSum;
        accumulatorCount += localCount;
      }
    },
    this);

  if (accumulatorCount > 0)
  {
    m_MeanSSIM = static_cast<double>(accumulator) / static_cast<double>(accumulatorCount);
  }
  else
  {
    m_MeanSSIM = 0.0;
  }
}

template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "GaussianSigma: " << m_GaussianSigma << std::endl;
  os << indent << "MaximumKernelWidth: " << m_MaximumKernelWidth << std::endl;
  os << indent << "K1: " << m_K1 << std::endl;
  os << indent << "K2: " << m_K2 << std::endl;
  os << indent << "DynamicRange: " << m_DynamicRange << std::endl;
  os << indent << "LuminanceExponent: " << m_LuminanceExponent << std::endl;
  os << indent << "ContrastExponent: " << m_ContrastExponent << std::endl;
  os << indent << "StructureExponent: " << m_StructureExponent << std::endl;
  os << indent << "ScaleWeights: " << m_ScaleWeights << std::endl;
  os << indent << "MeanSSIM: " << m_MeanSSIM << std::endl;
}
} // end namespace itk
#endif
