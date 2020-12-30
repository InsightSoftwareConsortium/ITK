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
#ifndef itkGradientMagnitudeRecursiveGaussianImageFilter_h
#define itkGradientMagnitudeRecursiveGaussianImageFilter_h

#include "itkNthElementImageAdaptor.h"
#include "itkImage.h"
#include "itkPixelTraits.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkSqrtImageFilter.h"
#include "itkBinaryGeneratorImageFilter.h"

namespace itk
{
/**
 *\class GradientMagnitudeRecursiveGaussianImageFilter
 * \brief Computes the Magnitude of the Gradient of an image by convolution
 *        with the first derivative of a Gaussian.
 *
 * This filter is implemented using the recursive gaussian
 * filters
 *
 * \ingroup GradientFilters
 * \ingroup SingleThreaded
 * \ingroup ITKImageGradient
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageGradient/ComputeGradientMagnitude,Compute Gradient Magnitude Of Grayscale Image}
 * \endsphinx
 */
// NOTE that the typename macro has to be used here in lieu
// of "typename" because VC++ doesn't like the typename keyword
// on the defaults of template parameters
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT GradientMagnitudeRecursiveGaussianImageFilter
  : public InPlaceImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GradientMagnitudeRecursiveGaussianImageFilter);

  /** Standard class type aliases. */
  using Self = GradientMagnitudeRecursiveGaussianImageFilter;
  using Superclass = InPlaceImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Pixel Type of the input image */
  using InputImageType = TInputImage;
  using PixelType = typename InputImageType::PixelType;

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using RealType = typename NumericTraits<PixelType>::RealType;

  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */

  using InternalRealType = float;
  using RealImageType = Image<InternalRealType, Self::ImageDimension>;

  /**  Smoothing filter type */
  using GaussianFilterType = RecursiveGaussianImageFilter<RealImageType, RealImageType>;

  /**  Derivative filter type, it will be the first in the pipeline  */
  using DerivativeFilterType = RecursiveGaussianImageFilter<InputImageType, RealImageType>;

  /**  Smoothing filter type */
  using SqrtFilterType = SqrtImageFilter<RealImageType, TOutputImage>;

  /**  Pointer to a gaussian filter.  */
  using GaussianFilterPointer = typename GaussianFilterType::Pointer;

  /**  Pointer to a derivative filter.  */
  using DerivativeFilterPointer = typename DerivativeFilterType::Pointer;

  using SqrtFilterPointer = typename SqrtFilterType::Pointer;

  /**  Pointer to the Output Image */
  using OutputImagePointer = typename TOutputImage::Pointer;

  /** Type of the output Image */
  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;

  /**  Auxiliary image for holding the values of the squared gradient components
   */
  using CumulativeImageType = Image<InternalRealType, Self::ImageDimension>;
  using CumulativeImagePointer = typename CumulativeImageType::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GradientMagnitudeRecursiveGaussianImageFilter, InPlaceImageFilter);

  /** Set Sigma value. Sigma is measured in the units of image spacing.  */
  void
  SetSigma(RealType sigma);
  RealType
  GetSigma();

  /** Define which normalization factor will be used for the Gaussian
   *  \sa  RecursiveGaussianImageFilter::SetNormalizeAcrossScale
   */
  void
  SetNormalizeAcrossScale(bool normalize);
  itkGetConstMacro(NormalizeAcrossScale, bool);

  void
  SetNumberOfWorkUnits(ThreadIdType nb) override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelType>));
  // End concept checking
#endif

protected:
  GradientMagnitudeRecursiveGaussianImageFilter();
  ~GradientMagnitudeRecursiveGaussianImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate Data */
  void
  GenerateData() override;

  /** GradientMagnitudeRecursiveGaussianImageFilter needs all of the
   * input to produce an output. Therefore,
   * GradientMagnitudeRecursiveGaussianImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion in order to
   * inform the pipeline execution model.  \sa
   * ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

  /** GradientMagnitudeRecursiveGaussianImageFilter produces all of
   * the output.  Therefore, it needs to provide an implementation of
   * EnlargeOutputRequestedRegion(). */
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

private:
  using SqrSpacingFilterType = BinaryGeneratorImageFilter<RealImageType, RealImageType, RealImageType>;
  using SqrSpacingFilterPointer = typename SqrSpacingFilterType::Pointer;

  GaussianFilterPointer   m_SmoothingFilters[ImageDimension - 1];
  DerivativeFilterPointer m_DerivativeFilter;
  SqrSpacingFilterPointer m_SqrSpacingFilter;
  SqrtFilterPointer       m_SqrtFilter;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGradientMagnitudeRecursiveGaussianImageFilter.hxx"
#endif

#endif
