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
#ifndef itkBinomialBlurImageFilter_h
#define itkBinomialBlurImageFilter_h

#include "itkImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkSize.h"

namespace itk
{
/**
 *\class BinomialBlurImageFilter
 * \brief Performs a separable blur on each dimension of an image.
 *
 * The binomial blur consists of a nearest neighbor average along each
 * image dimension. The net result after n-iterations approaches
 * convolution with a gaussian.
 *
 * \ingroup ImageEnhancement
 * \ingroup ITKSmoothing
 *
 * \sphinx
 * \sphinxexample{Filtering/Smoothing/BlurringAnImageUsingABinomialKernel,Blurring An Image Using A Binomial Kernel}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT BinomialBlurImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinomialBlurImageFilter);

  /** Standard class type aliases. */
  using Self = BinomialBlurImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinomialBlurImageFilter, ImageToImageFilter);

  /** Number of dimensions */
  static constexpr unsigned int NDimensions = TInputImage::ImageDimension;
  static constexpr unsigned int NOutputDimensions = TOutputImage::ImageDimension;

  /** Typedef for images */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;

  /** Image size type alias */
  using SizeType = Size<Self::NDimensions>;

  /** Image index type alias */
  using IndexType = typename TOutputImage::IndexType;

  /** Image pixel value type alias */
  using PixelType = typename TOutputImage::PixelType;

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Get and set the number of times to repeat the filter. */
  itkSetMacro(Repetitions, unsigned int);
  itkGetConstMacro(Repetitions, unsigned int);

  /** This filter needs to request a larger input than its requested output.
   * If this filter runs "Repetitions" iterations, then it needs an input
   * that is 2*Repetitions larger than the output. In other words, this
   * filter needs a border of "Repetitions" pixels. */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<Self::NDimensions, Self::NOutputDimensions>));
  itkConceptMacro(InputConvertibleToDoubleCheck, (Concept::Convertible<typename TInputImage::PixelType, double>));
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, PixelType>));
  // End concept checking
#endif

protected:
  BinomialBlurImageFilter();
  ~BinomialBlurImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Method for evaluating the implicit function over the image. */
  void
  GenerateData() override;

private:
  /** How many times should we apply the blur? */
  unsigned int m_Repetitions;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinomialBlurImageFilter.hxx"
#endif

#endif
