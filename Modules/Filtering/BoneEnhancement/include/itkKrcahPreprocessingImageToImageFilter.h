/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#ifndef itkKrcahPreprocessingImageToImageFilter_h
#define itkKrcahPreprocessingImageToImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkAddImageFilter.h"

namespace itk
{
/** \class KrcahPreprocessingImageToImageFilter
 * \brief Perform preprocessing as defined by Krcah et al
 *
 * This filters performs an unsharp filter as defined by Krcah
 * et al. The unsharp filter is defined by:
 *  \f{
 *      J = I+k*(I-(I*G))
 *  \f}
 *
 * Where \f$ k \f$ is a scaling constant set to 5 and the Guassian
 * filter has smoothing parameter \f$ s = 1 mm \f$. A user can modify
 * these defaults using the appropriate setter methods.
 *
 * Additionally, this filter provides the ReleaseInternalFilterData
 * flag. When this flag is set, the internal filters used to compute
 * the unsharp filter will release their data after processing. This
 * conserves memory at the expense of computation time if ScalingConstant
 * or Sigma are changed. This flag is on by default.
 *
 * \sa KrcahEigenToScalarImageFilter
 *
 * \author: Thomas Fitze
 * \ingroup BoneEnhancement
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT KrcahPreprocessingImageToImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(KrcahPreprocessingImageToImageFilter);

  /** Standard Self type alias */
  using Self = KrcahPreprocessingImageToImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(KrcahPreprocessingImageToImageFilter, ImageToImageFilter);

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Image related type alias. */
  using PixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using RealType = typename NumericTraits<PixelType>::RealType;
  using OutputPixelValueType = typename NumericTraits<OutputPixelType>::ValueType;
  using InputImageConstPointer = typename TInputImage::ConstPointer;

  /** Typedefs for internal filters */
  using GaussianFilterType = DiscreteGaussianImageFilter<TInputImage, TInputImage>;
  using SubstractFilterType = SubtractImageFilter<TInputImage, TInputImage, TInputImage>;
  using MultiplyFilterType = MultiplyImageFilter<TInputImage, TInputImage, TInputImage>;
  using AddFilterType = AddImageFilter<TInputImage, TInputImage, TOutputImage>;

  /** Flag to release data or not */
  itkSetMacro(ReleaseInternalFilterData, bool);
  itkGetConstMacro(ReleaseInternalFilterData, bool);
  itkBooleanMacro(ReleaseInternalFilterData);

  /** Flag to release data or not */
  itkSetMacro(Sigma, RealType);
  itkGetConstMacro(Sigma, RealType);

  /** Flag to release data or not */
  itkSetMacro(ScalingConstant, RealType);
  itkGetConstMacro(ScalingConstant, RealType);

  /** DiscreteGaussianImageFilter needs a larger input requested region
   * than the output requested region (larger by the size of the
   * Gaussian kernel).  As such, DiscreteGaussianImageFilter needs to
   * provide an implementation for GenerateInputRequestedRegion() in
   * order to inform the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputOutputHaveSamePixelDimensionCheck,
                  (Concept::SameDimension<TInputImage::ImageDimension, TOutputImage::ImageDimension>));
  // End concept checking
#endif
protected:
  KrcahPreprocessingImageToImageFilter();

  /** Single threaded since we are connecting data */
  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /* Internal member variables */
  RealType m_Sigma;
  RealType m_ScalingConstant;
  bool     m_ReleaseInternalFilterData;

  /* Filter member variables */
  typename GaussianFilterType::Pointer  m_GaussianFilter;
  typename SubstractFilterType::Pointer m_SubtractFilter;
  typename MultiplyFilterType::Pointer  m_MultiplyFilter;
  typename AddFilterType::Pointer       m_AddFilter;
}; // end class
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkKrcahPreprocessingImageToImageFilter.hxx"
#endif

#endif // itkKrcahPreprocessingImageToImageFilter_h
