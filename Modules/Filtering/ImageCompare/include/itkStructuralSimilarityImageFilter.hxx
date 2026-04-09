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
#include "itkBufferedImageNeighborhoodPixelAccessPolicy.h"
#include "itkImageNeighborhoodOffsets.h"
#include "itkImageRegionRange.h"
#include "itkIndexRange.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkShapedImageNeighborhoodRange.h"
#include "itkZeroFluxNeumannImageNeighborhoodPixelAccessPolicy.h"
#include <cmath>
#include <mutex>
namespace itk
{
template <typename TInputImage, typename TOutputImage>
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::StructuralSimilarityImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  this->DynamicMultiThreadingOn();
  m_Radius.Fill(1);
}
template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::SetRadius(const RadiusType & radius)
{
  if (m_Radius != radius)
  {
    m_Radius = radius;
    this->Modified();
  }
}
template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::SetRadius(RadiusValueType radius)
{
  RadiusType r;
  r.Fill(radius);
  this->SetRadius(r);
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
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation
  Superclass::GenerateInputRequestedRegion();
  // Pad the requested region by the neighborhood radius
  for (unsigned int i = 0; i < 2; ++i)
  {
    auto * input = const_cast<InputImageType *>(this->GetInput(i));
    if (!input)
    {
      continue;
    }
    InputImageRegionType requestedRegion = input->GetRequestedRegion();
    requestedRegion.PadByRadius(m_Radius);
    requestedRegion.Crop(input->GetLargestPossibleRegion());
    input->SetRequestedRegion(requestedRegion);
  }
}
template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  m_AccumulatedSSIM = RealType{};
  m_PixelCount = 0;
}
template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::AfterThreadedGenerateData()
{
  if (m_PixelCount > 0)
  {
    m_SSIM = m_AccumulatedSSIM / static_cast<RealType>(m_PixelCount);
  }
  else
  {
    m_SSIM = RealType{};
  }
}
template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  const auto * input1 = this->GetInput(0);
  const auto * input2 = this->GetInput(1);
  auto *       output = this->GetOutput();
  const RealType C1 = (m_K1 * m_DynamicRange) * (m_K1 * m_DynamicRange);
  const RealType C2 = (m_K2 * m_DynamicRange) * (m_K2 * m_DynamicRange);
  const RealType C3 = C2 / RealType{ 2.0 };
  const bool useSimplifiedFormula =
    (itk::Math::FloatAlmostEqual(m_LuminanceWeight, RealType{ 1.0 }) &&
     itk::Math::FloatAlmostEqual(m_ContrastWeight, RealType{ 1.0 }) &&
     itk::Math::FloatAlmostEqual(m_StructureWeight, RealType{ 1.0 }));
  const auto neighborhoodOffsets = GenerateRectangularImageNeighborhoodOffsets<ImageDimension>(m_Radius);
  const auto neighborhoodSize = static_cast<RealType>(neighborhoodOffsets.size());
  // Use ImageBoundaryFacesCalculator to split into interior and boundary regions
  const auto calculatorResult =
    NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::Compute(*input1, outputRegionForThread, m_Radius);
  // Lambda to process a subregion with a given pixel access policy
  auto processSubregion = [&](const auto & subregion, auto policyTag1, auto policyTag2)
  {
    using Policy1 = typename decltype(policyTag1)::type;
    using Policy2 = typename decltype(policyTag2)::type;
    auto range1 = ShapedImageNeighborhoodRange<const InputImageType, Policy1>(
      *input1, IndexType(), neighborhoodOffsets);
    auto range2 = ShapedImageNeighborhoodRange<const InputImageType, Policy2>(
      *input2, IndexType(), neighborhoodOffsets);
    auto outputIt = ImageRegionRange<OutputImageType>(*output, subregion).begin();
    RealType      localSum{};
    SizeValueType localCount{};
    for (const auto & index : MakeIndexRange(subregion))
    {
      range1.SetLocation(index);
      range2.SetLocation(index);
      // Compute local means
      RealType sumX{};
      RealType sumY{};
      RealType sumXX{};
      RealType sumYY{};
      RealType sumXY{};
      auto it1 = range1.begin();
      auto it2 = range2.begin();
      for (; it1 != range1.end(); ++it1, ++it2)
      {
        const RealType x = static_cast<RealType>(*it1);
        const RealType y = static_cast<RealType>(*it2);
        sumX += x;
        sumY += y;
        sumXX += x * x;
        sumYY += y * y;
        sumXY += x * y;
      }
      const RealType muX = sumX / neighborhoodSize;
      const RealType muY = sumY / neighborhoodSize;
      // Variance and covariance (using population formula consistent with SSIM literature)
      const RealType sigmaXX = sumXX / neighborhoodSize - muX * muX;
      const RealType sigmaYY = sumYY / neighborhoodSize - muY * muY;
      const RealType sigmaXY = sumXY / neighborhoodSize - muX * muY;
      RealType ssimValue;
      if (useSimplifiedFormula)
      {
        // Standard SSIM: (2*muX*muY + C1)*(2*sigmaXY + C2) /
        //                ((muX^2 + muY^2 + C1)*(sigmaXX + sigmaYY + C2))
        const RealType numerator = (RealType{ 2.0 } * muX * muY + C1) * (RealType{ 2.0 } * sigmaXY + C2);
        const RealType denominator = (muX * muX + muY * muY + C1) * (sigmaXX + sigmaYY + C2);
        ssimValue = numerator / denominator;
      }
      else
      {
        // General form with individual exponents
        const RealType luminance =
          (RealType{ 2.0 } * muX * muY + C1) / (muX * muX + muY * muY + C1);
        const RealType sigmaX = std::sqrt(std::max(sigmaXX, RealType{}));
        const RealType sigmaY = std::sqrt(std::max(sigmaYY, RealType{}));
        const RealType contrast =
          (RealType{ 2.0 } * sigmaX * sigmaY + C2) / (sigmaXX + sigmaYY + C2);
        const RealType structure = (sigmaXY + C3) / (sigmaX * sigmaY + C3);
        ssimValue =
          std::pow(luminance, m_LuminanceWeight) * std::pow(contrast, m_ContrastWeight) * std::pow(structure, m_StructureWeight);
      }
      *outputIt = static_cast<OutputPixelType>(ssimValue);
      ++outputIt;
      localSum += ssimValue;
      ++localCount;
    }
    // Accumulate thread-local results under the mutex
    {
      const std::lock_guard<std::mutex> lock(m_Mutex);
      m_AccumulatedSSIM += localSum;
      m_PixelCount += localCount;
    }
  };
  // Tag types to pass policy as template argument through the lambda
  struct PolicyTag1
  {
    using type = BufferedImageNeighborhoodPixelAccessPolicy<InputImageType>;
  };
  struct PolicyTag2
  {
    using type = ZeroFluxNeumannImageNeighborhoodPixelAccessPolicy<InputImageType>;
  };
  // Process interior region (fast path, no boundary checks)
  const auto & nonBoundaryRegion = calculatorResult.GetNonBoundaryRegion();
  if (nonBoundaryRegion.GetNumberOfPixels() > 0)
  {
    processSubregion(nonBoundaryRegion, PolicyTag1{}, PolicyTag1{});
  }
  // Process boundary faces (with boundary extrapolation)
  for (const auto & face : calculatorResult.GetBoundaryFaces())
  {
    processSubregion(face, PolicyTag2{}, PolicyTag2{});
  }
}
template <typename TInputImage, typename TOutputImage>
void
StructuralSimilarityImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Radius: " << m_Radius << std::endl;
  os << indent << "LuminanceWeight: " << m_LuminanceWeight << std::endl;
  os << indent << "ContrastWeight: " << m_ContrastWeight << std::endl;
  os << indent << "StructureWeight: " << m_StructureWeight << std::endl;
  os << indent << "DynamicRange: " << m_DynamicRange << std::endl;
  os << indent << "K1: " << m_K1 << std::endl;
  os << indent << "K2: " << m_K2 << std::endl;
  os << indent << "SSIM: " << m_SSIM << std::endl;
}
} // end namespace itk
#endif
