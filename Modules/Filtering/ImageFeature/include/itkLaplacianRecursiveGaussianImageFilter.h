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
#ifndef itkLaplacianRecursiveGaussianImageFilter_h
#define itkLaplacianRecursiveGaussianImageFilter_h

#include "itkRecursiveGaussianImageFilter.h"
#include "itkImage.h"
#include "itkCommand.h"

namespace itk
{
/**
 *\class LaplacianRecursiveGaussianImageFilter
 * \brief Computes the Laplacian of Gaussian (LoG) of an image.
 *
 * Computes the Laplacian of Gaussian (LoG) of an image by convolution
 * with the second derivative of a Gaussian.
 * This filter is implemented using the recursive gaussian filters.
 *
 * \ingroup GradientFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageFeature
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageFeature/ComputeLaplacian,Compute Laplacian}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT LaplacianRecursiveGaussianImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LaplacianRecursiveGaussianImageFilter);

  /** Standard class type aliases. */
  using Self = LaplacianRecursiveGaussianImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Pixel Type of the input image */
  using InputImageType = TInputImage;
  using PixelType = typename InputImageType::PixelType;

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  static constexpr unsigned int NumberOfSmoothingFilters = ImageDimension - 1;

  using RealType = typename NumericTraits<PixelType>::RealType;

  /** Define the image type for internal computations.
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */
  using InternalRealType = float;
  using RealImageType = Image<InternalRealType, Self::ImageDimension>;

  /**  Smoothing filter type */
  using GaussianFilterType = RecursiveGaussianImageFilter<RealImageType, RealImageType>;

  /**  Derivative filter type, it will be the first in the pipeline  */
  using DerivativeFilterType = RecursiveGaussianImageFilter<InputImageType, RealImageType>;

  /**  Pointer to a gaussian filter.  */
  using GaussianFilterPointer = typename GaussianFilterType::Pointer;

  /**  Pointer to a derivative filter.  */
  using DerivativeFilterPointer = typename DerivativeFilterType::Pointer;

  /**  Pointer to the Output Image */
  using OutputImagePointer = typename TOutputImage::Pointer;

  /** Type of the output Image */
  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;


  /**  Command for observing progress of internal pipeline filters */
  using CommandType = MemberCommand<Self>;
  using CommandPointer = typename CommandType::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LaplacianRecursiveGaussianImageFilter, ImageToImageFilter);

  /** Set Sigma value. Sigma is measured in the units of image spacing. */
  void
  SetSigma(RealType sigma);
  RealType
  GetSigma() const;

  /** Define which normalization factor will be used for the Gaussian
   *  \sa  RecursiveGaussianImageFilter::SetNormalizeAcrossScale
   */
  void
  SetNormalizeAcrossScale(bool normalizeInScaleSpace);
  itkGetConstMacro(NormalizeAcrossScale, bool);

protected:
  LaplacianRecursiveGaussianImageFilter();
  ~LaplacianRecursiveGaussianImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate Data */
  void
  GenerateData() override;

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

private:
  GaussianFilterPointer   m_SmoothingFilters[NumberOfSmoothingFilters];
  DerivativeFilterPointer m_DerivativeFilter;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLaplacianRecursiveGaussianImageFilter.hxx"
#endif

#endif
