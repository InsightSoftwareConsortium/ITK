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

#ifndef itkSmoothingRecursiveYvvGaussianImageFilter_h
#define itkSmoothingRecursiveYvvGaussianImageFilter_h

#include "itkRecursiveLineYvvGaussianImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkImage.h"
#include "itkPixelTraits.h"
#include "itkCommand.h"
#include "itkFixedArray.h"

namespace itk
{
/**
 * \class SmoothingRecursiveYvvGaussianImageFilter
 * \brief Recursive Gaussian blurring filter based on Young-Van Vliet's
 *  algorithm, implemented for CPU.
 *
 *  This CPU implementation is more efficient than the GPU implamentation for
 *  smaller images (e.g. 512x512 and smaller for quadcores at over 3GHz); use
 *  the benchmark tests to establish the size for which this implementation
 *  performs better for your particular hardware configuration.
 *
 *  More information in the Insight Journal publication:
 *  https://hdl.handle.net/10380/3425
 *
 * \ingroup SmoothingRecursiveYvvGaussianFilter
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_EXPORT SmoothingRecursiveYvvGaussianImageFilter : public InPlaceImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SmoothingRecursiveYvvGaussianImageFilter);

  /** Standard class type alias. */
  using Self = SmoothingRecursiveYvvGaussianImageFilter;
  using Superclass = InPlaceImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Pixel Type of the input image */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using PixelType = typename TInputImage::PixelType;
#ifdef WITH_DOUBLE
  using RealType = typename NumericTraits<PixelType>::RealType;
  using ScalarRealType = typename NumericTraits<PixelType>::ScalarRealType;
#else
  using RealType = typename NumericTraits<PixelType>::FloatType;
  using ScalarRealType = typename NumericTraits<PixelType>::FloatType;
#endif

  /** Runtime information support. */
  itkTypeMacro(SmoothingRecursiveYvvGaussianImageFilter, InPlaceImageFilter);

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Define the type for the sigma array */
  using SigmaArrayType = FixedArray<ScalarRealType, itkGetStaticConstMacro(ImageDimension)>;

  /** Define the image type for internal computations
   RealType is usually 'double' in NumericTraits.
   Here we prefer float in order to save memory.  */

  using InternalRealType = typename NumericTraits<PixelType>::FloatType;
  using RealImageType = typename InputImageType::template Rebind<InternalRealType>::Type;

  /**  The first in the pipeline  */
  typedef RecursiveLineYvvGaussianImageFilter<InputImageType, RealImageType> FirstGaussianFilterType;

  /**  Smoothing filter type */
  typedef RecursiveLineYvvGaussianImageFilter<RealImageType, RealImageType> InternalGaussianFilterType;

  /**  The last in the pipeline  */
  typedef CastImageFilter<RealImageType, OutputImageType> CastingFilterType;

  /**  Pointer to a gaussian filter.  */
  using InternalGaussianFilterPointer = typename InternalGaussianFilterType::Pointer;

  /**  Pointer to the first gaussian filter.  */
  using FirstGaussianFilterPointer = typename FirstGaussianFilterType::Pointer;

  /**  Pointer to the last filter, casting  */
  using CastingFilterPointer = typename CastingFilterType::Pointer;

  /**  Pointer to the Output Image */
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set Sigma value. Sigma is measured in the units of image spacing. You
   may use the method SetSigma to set the same value across each axis or
   use the method SetSigmaArray if you need different values along each
   axis. */
  void
  SetSigmaArray(const SigmaArrayType & sigmas);
  void
  SetSigma(ScalarRealType sigma);
  SigmaArrayType
  GetSigmaArray() const;
  ScalarRealType
  GetSigma() const;

  /** Define which normalization factor will be used for the Gaussian */
  void
  SetNormalizeAcrossScale(bool normalizeInScaleSpace);
  itkGetConstMacro(NormalizeAcrossScale, bool);

  void
  SetNumberOfWorkUnits(ThreadIdType nb) override;

  bool
  CanRunInPlace() const override;

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelType>));
  /** End concept checking */
#endif
protected:
  SmoothingRecursiveYvvGaussianImageFilter();
  ~SmoothingRecursiveYvvGaussianImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate Data */
  void
  GenerateData() override;

  /** SmoothingRecursiveYvvGaussianImageFilter needs all of the input to produce an
   * output. Therefore, SmoothingRecursiveYvvGaussianImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() ITK_NOEXCEPT override;

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

private:
  InternalGaussianFilterPointer m_SmoothingFilters[ImageDimension - 1];
  FirstGaussianFilterPointer    m_FirstSmoothingFilter;
  CastingFilterPointer          m_CastingFilter;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;

  /** Standard deviation of the gaussian used for smoothing */
  SigmaArrayType m_Sigma;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSmoothingRecursiveYvvGaussianImageFilter.hxx"
#endif

#endif
