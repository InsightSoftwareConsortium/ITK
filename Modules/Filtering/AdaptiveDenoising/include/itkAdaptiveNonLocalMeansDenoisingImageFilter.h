/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkGaussianOperator.h"

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
  typedef AdaptiveNonLocalMeansDenoisingImageFilter                Self;
  typedef NonLocalPatchBasedImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                                       Pointer;
  typedef SmartPointer<const Self>                                 ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(AdaptiveNonLocalMeansDenoisingImageFilter, NonLocalPatchBasedImageFilter);

  /** Standard New method. */
  itkNewMacro(Self);

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** Some convenient typedefs. */
  typedef TInputImage                        InputImageType;
  typedef typename InputImageType::PixelType InputPixelType;
  typedef TOutputImage                       OutputImageType;
  typedef typename Superclass::RegionType    RegionType;

  typedef TMaskImage                        MaskImageType;
  typedef typename MaskImageType::PixelType MaskPixelType;
  typedef typename MaskImageType::PixelType LabelType;

  typedef typename Superclass::RealType         RealType;
  typedef typename Superclass::RealImageType    RealImageType;
  typedef typename Superclass::RealImagePointer RealImagePointer;
  typedef typename Superclass::IndexType        IndexType;

  typedef typename Superclass::ConstNeighborhoodIteratorType ConstNeighborhoodIteratorType;
  typedef typename Superclass::NeighborhoodRadiusType        NeighborhoodRadiusType;
  typedef typename Superclass::NeighborhoodOffsetType        NeighborhoodOffsetType;
  typedef typename Superclass::NeighborhoodOffsetListType    NeighborhoodOffsetListType;

  typedef GaussianOperator<RealType> ModifiedBesselCalculatorType;

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
  itkSetMacro(UseRicianNoiseModel, bool);
  itkGetConstMacro(UseRicianNoiseModel, bool);
  itkBooleanMacro(UseRicianNoiseModel);

  /**
   * Smoothing factor for noise.  Default = 1.0.
   */
  itkSetMacro(SmoothingFactor, RealType);
  itkGetConstMacro(SmoothingFactor, RealType);

  /**
   * Smoothing variance for Rician noise.  Default = 2.0.
   */
  itkSetMacro(SmoothingVariance, RealType);
  itkGetConstMacro(SmoothingVariance, RealType);

  /**
   * Epsilon for minimum value of mean and variance at a pixel.
   * Default = 0.00001.
   */
  itkSetMacro(Epsilon, RealType);
  itkGetConstMacro(Epsilon, RealType);

  /**
   * Mean threshold.
   * Default = 0.95.
   */
  itkSetMacro(MeanThreshold, RealType);
  itkGetConstMacro(MeanThreshold, RealType);

  /**
   * Variance threshold.
   * Default = 0.5.
   */
  itkSetMacro(VarianceThreshold, RealType);
  itkGetConstMacro(VarianceThreshold, RealType);

  /**
   * Neighborhood for computing local mean and variance images.
   * Default = 1x1x...
   */
  itkSetMacro(NeighborhoodRadiusForLocalMeanAndVariance, NeighborhoodRadiusType);
  itkGetConstMacro(NeighborhoodRadiusForLocalMeanAndVariance, NeighborhoodRadiusType);

protected:
  AdaptiveNonLocalMeansDenoisingImageFilter();
  ~AdaptiveNonLocalMeansDenoisingImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  ThreadedGenerateData(const RegionType &, ThreadIdType) override;

  void
  BeforeThreadedGenerateData() override;

  void
  AfterThreadedGenerateData() override;

private:
  RealType CalculateCorrectionFactor(RealType);

  bool m_UseRicianNoiseModel;

  ModifiedBesselCalculatorType m_ModifiedBesselCalculator;

  RealType m_Epsilon;
  RealType m_MeanThreshold;
  RealType m_VarianceThreshold;
  RealType m_SmoothingFactor;
  RealType m_SmoothingVariance;

  RealType m_MaximumInputPixelIntensity;
  RealType m_MinimumInputPixelIntensity;

  RealImagePointer m_MeanImage;
  RealImagePointer m_RicianBiasImage;
  RealImagePointer m_VarianceImage;
  RealImagePointer m_ThreadContributionCountImage;
  RealImagePointer m_IntensitySquaredDistanceImage;

  NeighborhoodRadiusType m_NeighborhoodRadiusForLocalMeanAndVariance;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAdaptiveNonLocalMeansDenoisingImageFilter.hxx"
#endif

#endif
