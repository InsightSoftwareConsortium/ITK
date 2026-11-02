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
#ifndef itkAdaptiveNonLocalMeansDenoisingImageFilter_h
#define itkAdaptiveNonLocalMeansDenoisingImageFilter_h

#include "itkNonLocalPatchBasedImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkTotalProgressReporter.h"

#include <atomic>

namespace itk
{

/**
 * \class AdaptiveNonLocalMeansDenoisingImageFilter
 * \brief Implementation of a denoising image filter.
 *
 * \author Jose V. Manjon with ITK porting by Nick Tustison
 *
 * Contributed by
 *
 * \par REFERENCE
 *
 * J. V. Manjon, P. Coupe, Luis Marti-Bonmati, D. L. Collins,
 * and M. Robles. "Adaptive Non-Local Means Denoising of MR Images With
 * Spatially Varying Noise Levels, Journal of Magnetic Resonance Imaging,
 * 31:192-203, June 2010.
 *
 * \ingroup AdaptiveDenoising
 */

template <typename TInputImage,
          typename TOutputImage = TInputImage,
          typename TMaskImage = Image<unsigned char, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT AdaptiveNonLocalMeansDenoisingImageFilter final
  : public NonLocalPatchBasedImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AdaptiveNonLocalMeansDenoisingImageFilter);

  /** Standard class typedefs. */
  using Self = AdaptiveNonLocalMeansDenoisingImageFilter;
  using Superclass = NonLocalPatchBasedImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Runtime information support. */
  itkOverrideGetNameOfClassMacro(AdaptiveNonLocalMeansDenoisingImageFilter);

  /** Standard New method. */
  itkNewMacro(Self);

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Some convenient typedefs. */
  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using OutputImageType = TOutputImage;
  using RegionType = typename Superclass::RegionType;

  using MaskImageType = TMaskImage;
  using MaskPixelType = typename MaskImageType::PixelType;
  using LabelType = typename MaskImageType::PixelType;

  using RealType = typename Superclass::RealType;
  using RealImageType = typename Superclass::RealImageType;
  using RealImagePointer = typename Superclass::RealImagePointer;
  using IndexType = typename Superclass::IndexType;

  using ConstNeighborhoodIteratorType = typename Superclass::ConstNeighborhoodIteratorType;
  using NeighborhoodRadiusType = typename Superclass::NeighborhoodRadiusType;
  using NeighborhoodOffsetType = typename Superclass::NeighborhoodOffsetType;
  using NeighborhoodOffsetListType = typename Superclass::NeighborhoodOffsetListType;

  using ModifiedBesselCalculatorType = GaussianOperator<RealType>;

  /**
   * The image expected for input for noise correction.
   */
  void
  SetInput1(const InputImageType * image)
  {
    this->SetInput(image);
  }

  /**
   * Set mask image function.  If a binary mask image is specified, only
   * those input image voxels corresponding with the mask image.
   */
  void
  SetMaskImage(const MaskImageType * mask)
  {
    this->SetNthInput(1, const_cast<MaskImageType *>(mask));
  }
  void
  SetInput2(const MaskImageType * mask)
  {
    this->SetMaskImage(mask);
  }

  /**
   * Get mask image function.  If a binary mask image is specified, only
   * those input image voxels corresponding with the mask image.
   */
  const MaskImageType *
  GetMaskImage() const
  {
    return static_cast<const MaskImageType *>(this->ProcessObject::GetInput(1));
  }

  /**
   * Employ Rician noise model.  Otherwise use a Gaussian noise model.
   * Default = true.
   */
  itkNonVirtualSetMacro(UseRicianNoiseModel, bool);
  itkNonVirtualGetConstMacro(UseRicianNoiseModel, bool);
  itkNonVirtualBooleanMacro(UseRicianNoiseModel);

  /**
   * Smoothing factor for noise.  Default = 1.0.
   */
  itkNonVirtualSetMacro(SmoothingFactor, RealType);
  itkNonVirtualGetConstMacro(SmoothingFactor, RealType);

  /**
   * Smoothing variance for Rician noise.  Default = 2.0.
   */
  itkNonVirtualSetMacro(SmoothingVariance, RealType);
  itkNonVirtualGetConstMacro(SmoothingVariance, RealType);

  /**
   * Epsilon for minimum value of mean and variance at a pixel.
   * Default = 0.00001.
   */
  itkNonVirtualSetMacro(Epsilon, RealType);
  itkNonVirtualGetConstMacro(Epsilon, RealType);

  /**
   * Mean threshold.
   * Default = 0.95.
   */
  itkNonVirtualSetMacro(MeanThreshold, RealType);
  itkNonVirtualGetConstMacro(MeanThreshold, RealType);

  /**
   * Variance threshold.
   * Default = 0.5.
   */
  itkNonVirtualSetMacro(VarianceThreshold, RealType);
  itkNonVirtualGetConstMacro(VarianceThreshold, RealType);

  /**
   * Neighborhood for computing local mean and variance images.
   * Default = 1x1x...
   */
  itkNonVirtualSetMacro(NeighborhoodRadiusForLocalMeanAndVariance, NeighborhoodRadiusType);
  itkNonVirtualGetConstMacro(NeighborhoodRadiusForLocalMeanAndVariance, NeighborhoodRadiusType);

  /** The number of patch centers that the last Update() processed with linear offsets into the image buffers. The
   * other centers, near the border of the target region or when the buffers differ from it, are read through indices.
   */
  [[nodiscard]] SizeValueType
  GetNumberOfLinearOffsetCenters() const
  {
    return this->m_NumberOfLinearOffsetCenters;
  }

protected:
  AdaptiveNonLocalMeansDenoisingImageFilter();
  ~AdaptiveNonLocalMeansDenoisingImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  void
  BeforeThreadedGenerateData() override;

  void
  AfterThreadedGenerateData() override;

  /** The mean, variance and intensity range are computed over the whole input. */
  void
  GenerateInputRequestedRegion() override;

private:
  RealType CalculateCorrectionFactor(RealType);

  using RicianBiasSmootherType = DiscreteGaussianImageFilter<RealImageType, RealImageType>;

  /** The smoother of the Rician bias image, for the geometry of the input. */
  typename RicianBiasSmootherType::Pointer
  MakeRicianBiasSmoother() const;

  void
  AccumulatePatchEstimates(const RegionType &, OutputImageType *, TotalProgressReporter &);

  static constexpr RealType NoMinimumDistance = -1;

  bool
  IsSimilarSearchPosition(RealType meanCenterPixel,
                          RealType varianceCenterPixel,
                          RealType meanNeighborhoodPixel,
                          RealType varianceNeighborhoodPixel) const;

  bool m_UseRicianNoiseModel;

  ModifiedBesselCalculatorType m_ModifiedBesselCalculator;

  RealType m_Epsilon;
  RealType m_MeanThreshold;
  RealType m_VarianceThreshold;
  RealType m_SmoothingFactor;
  RealType m_SmoothingVariance;

  RealType m_MaximumInputPixelIntensity;
  RealType m_MinimumInputPixelIntensity;

  /** The patch centers whose estimates reach the output requested region, directly or through the bias smoothing. */
  RegionType m_CenterRegion{};

  RealImagePointer                 m_MeanImage;
  RealImagePointer                 m_RicianBiasImage;
  RealImagePointer                 m_VarianceImage;
  RealImagePointer                 m_CenterMinimumDistanceImage;
  RealImagePointer                 m_ResidualImage;
  typename InputImageType::Pointer m_SquaredInputImage;
  RealImagePointer                 m_IntensitySquaredDistanceImage;

  NeighborhoodRadiusType m_NeighborhoodRadiusForLocalMeanAndVariance;

  std::atomic<SizeValueType> m_NumberOfLinearOffsetCenters{ 0 };
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAdaptiveNonLocalMeansDenoisingImageFilter.hxx"
#endif

#endif
