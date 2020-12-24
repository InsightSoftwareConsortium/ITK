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

#ifndef itkSmoothingRecursiveGaussianImageFilter_h
#define itkSmoothingRecursiveGaussianImageFilter_h

#include "itkRecursiveGaussianImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkImage.h"
#include "itkPixelTraits.h"
#include "itkCommand.h"

namespace itk
{

/**
 *\class SmoothingRecursiveGaussianImageFilter
 * \brief Computes the smoothing of an image by convolution with the Gaussian kernels implemented as IIR filters.
 *
 * This filter is implemented using the recursive gaussian
 * filters. For multi-component images, the filter works on each
 * component independently.
 *
 * For this filter to be able to run in-place the input and output
 * image types need to be the same and/or the same type as the
 * RealImageType.
 *
 * \ingroup IntensityImageFilters
 * \ingroup SingleThreaded
 * \ingroup ITKSmoothing
 *
 */

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT SmoothingRecursiveGaussianImageFilter : public InPlaceImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SmoothingRecursiveGaussianImageFilter);

  /** Standard class type aliases. */
  using Self = SmoothingRecursiveGaussianImageFilter;
  using Superclass = InPlaceImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Pixel type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using PixelType = typename TInputImage::PixelType;
  using RealType = typename NumericTraits<PixelType>::RealType;
  using ScalarRealType = typename NumericTraits<PixelType>::ScalarRealType;

  /** Runtime information support. */
  itkTypeMacro(SmoothingRecursiveGaussianImageFilter, ImageToImageFilter);

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Define the type for the sigma array. */
  using SigmaArrayType = FixedArray<ScalarRealType, Self::ImageDimension>;

  /** Define the image type for internal computations.
   * RealType is usually 'double' in NumericTraits.
   * Here we prefer float in order to save memory. */
  using InternalRealType = typename NumericTraits<PixelType>::FloatType;
  using RealImageType = typename InputImageType::template Rebind<InternalRealType>::Type;

  /** Typedef for the first Gaussian smoothing in the pipeline. */
  using FirstGaussianFilterType = RecursiveGaussianImageFilter<InputImageType, RealImageType>;

  /** Typedef for the internal Gaussian smoothing filter. */
  using InternalGaussianFilterType = RecursiveGaussianImageFilter<RealImageType, RealImageType>;

  /** Typedef for the casting image filter. */
  using CastingFilterType = CastImageFilter<RealImageType, OutputImageType>;

  /** Pointer to the internal Gaussian filter. */
  using InternalGaussianFilterPointer = typename InternalGaussianFilterType::Pointer;

  /** Pointer to the first Gaussian smoothing filter. */
  using FirstGaussianFilterPointer = typename FirstGaussianFilterType::Pointer;

  /** Pointer to the casting image filter. */
  using CastingFilterPointer = typename CastingFilterType::Pointer;

  /** Pointer to the Output Image */
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set the standard deviation of the Gaussian used for smoothing.
   * Sigma is measured in the units of image spacing. You may use the method
   * SetSigma to set the same value across each axis or use the method
   * SetSigmaArray if you need different values along each axis. */
  void
  SetSigmaArray(const SigmaArrayType & sigma);
  void
  SetSigma(ScalarRealType sigma);

  /** Get the Sigma value. */
  SigmaArrayType
  GetSigmaArray() const;

  /** Get the Sigma scalar. If the Sigma is anisotropic, we will just
   * return the Sigma along the first dimension. */
  ScalarRealType
  GetSigma() const;

  /** Set/Get the flag for normalizing the Gaussian over scale-space.
   * This method does not effect the output of this filter.
   *
   * \sa RecursiveGaussianImageFilter::SetNormalizeAcrossScale */
  void
  SetNormalizeAcrossScale(bool normalize);
  itkGetConstMacro(NormalizeAcrossScale, bool);
  itkBooleanMacro(NormalizeAcrossScale);

  void
  SetNumberOfWorkUnits(ThreadIdType nb) override;

  bool
  CanRunInPlace() const override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  // This concept does not work with variable length vector images
  // itkConceptMacro( InputHasNumericTraitsCheck,
  //( Concept::HasNumericTraits< PixelType > ) );
  // End concept checking
#endif

protected:
  SmoothingRecursiveGaussianImageFilter();
  ~SmoothingRecursiveGaussianImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  /** SmoothingRecursiveGaussianImageFilter needs all of the input to produce an
   * output. Therefore, SmoothingRecursiveGaussianImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

private:
  InternalGaussianFilterPointer m_SmoothingFilters[ImageDimension - 1];
  FirstGaussianFilterPointer    m_FirstSmoothingFilter;
  CastingFilterPointer          m_CastingFilter;

  bool m_NormalizeAcrossScale{ false };

  SigmaArrayType m_Sigma;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSmoothingRecursiveGaussianImageFilter.hxx"
#endif

#endif
