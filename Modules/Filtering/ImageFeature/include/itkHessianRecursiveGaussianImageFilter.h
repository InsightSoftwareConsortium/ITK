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
#ifndef itkHessianRecursiveGaussianImageFilter_h
#define itkHessianRecursiveGaussianImageFilter_h

#include "itkRecursiveGaussianImageFilter.h"
#include "itkNthElementImageAdaptor.h"
#include "itkImage.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkPixelTraits.h"

namespace itk
{
/**
 *\class HessianRecursiveGaussianImageFilter
 * \brief Computes the Hessian matrix of an image by convolution
 *        with the Second and Cross derivatives of a Gaussian.
 *
 * This filter is implemented using the recursive gaussian
 * filters
 *
 *
 * \ingroup GradientFilters
 * \ingroup SingleThreaded
 * \ingroup ITKImageFeature
 */
template <typename TInputImage,
          typename TOutputImage =
            Image<SymmetricSecondRankTensor<typename NumericTraits<typename TInputImage::PixelType>::RealType,
                                            TInputImage::ImageDimension>,
                  TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT HessianRecursiveGaussianImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HessianRecursiveGaussianImageFilter);

  /** Standard class type aliases. */
  using Self = HessianRecursiveGaussianImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Pixel Type of the input image */
  using InputImageType = TInputImage;
  using PixelType = typename TInputImage::PixelType;
  using RealType = typename NumericTraits<PixelType>::RealType;

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Number of smoothing filters. */
  static constexpr unsigned int NumberOfSmoothingFilters =
    (TInputImage::ImageDimension > 2) ? (TInputImage::ImageDimension - 2) : (0);

  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */

  using InternalRealType = float;
  using RealImageType = Image<InternalRealType, TInputImage::ImageDimension>;

  /**  Output Image Nth Element Adaptor
   *  This adaptor allows to use conventional scalar
   *  smoothing filters to compute each one of the
   *  components of the gradient image pixels. */
  using OutputImageAdaptorType = NthElementImageAdaptor<TOutputImage, InternalRealType>;

  using OutputImageAdaptorPointer = typename OutputImageAdaptorType::Pointer;

  /**  Smoothing filter type */
  using GaussianFilterType = RecursiveGaussianImageFilter<RealImageType, RealImageType>;

  /**  Derivative filter type, it will be the first in the pipeline  */
  using DerivativeFilterAType = RecursiveGaussianImageFilter<InputImageType, RealImageType>;

  using DerivativeFilterBType = RecursiveGaussianImageFilter<RealImageType, RealImageType>;

  /**  Pointer to a gaussian filter.  */
  using GaussianFilterPointer = typename GaussianFilterType::Pointer;
  using GaussianFiltersArray = std::vector<GaussianFilterPointer>;

  /**  Pointer to a derivative filter.  */
  using DerivativeFilterAPointer = typename DerivativeFilterAType::Pointer;
  using DerivativeFilterBPointer = typename DerivativeFilterBType::Pointer;

  /**  Pointer to the Output Image */
  using OutputImagePointer = typename TOutputImage::Pointer;

  /** Type of the output Image */
  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;
  using OutputComponentType = typename PixelTraits<OutputPixelType>::ValueType;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(HessianRecursiveGaussianImageFilter, ImageToImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set Sigma value. Sigma is measured in the units of image spacing.  */
  void
  SetSigma(RealType sigma);
  RealType
  GetSigma() const;

  /** Define which normalization factor will be used for the Gaussian
   *  \sa  RecursiveGaussianImageFilter::SetNormalizeAcrossScale
   */
  void
  SetNormalizeAcrossScale(bool normalize);
  itkGetConstMacro(NormalizeAcrossScale, bool);

  /** HessianRecursiveGaussianImageFilter needs all of the input to produce an
   * output. Therefore, HessianRecursiveGaussianImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelType>));
  itkConceptMacro(OutputHasPixelTraitsCheck, (Concept::HasPixelTraits<OutputPixelType>));
  // End concept checking
#endif

protected:
  HessianRecursiveGaussianImageFilter();
  ~HessianRecursiveGaussianImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate Data */
  void
  GenerateData() override;

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

private:
  GaussianFiltersArray      m_SmoothingFilters;
  DerivativeFilterAPointer  m_DerivativeFilterA;
  DerivativeFilterBPointer  m_DerivativeFilterB;
  OutputImageAdaptorPointer m_ImageAdaptor;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHessianRecursiveGaussianImageFilter.hxx"
#endif

#endif
