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
#ifndef itkBilateralImageFilter_h
#define itkBilateralImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"
#include "itkNeighborhoodIterator.h"
#include "itkNeighborhood.h"

namespace itk
{
/**
 * \class BilateralImageFilter
 * \brief Blurs an image while preserving edges
 *
 * This filter uses bilateral filtering to blur an image using both
 * domain and range "neighborhoods". Pixels that are close to a pixel
 * in the image domain and similar to a pixel in the image range are
 * used to calculate the filtered value. Two gaussian kernels (one in
 * the image domain and one in the image range) are used to smooth
 * the image. The result is an image that is smoothed in homogeneous
 * regions yet has edges preserved. The result is similar to
 * anisotropic diffusion but the implementation in non-iterative.
 * Another benefit to bilateral filtering is that any distance metric
 * can be used for kernel smoothing the image range.  Hence, color
 * images can be smoothed as vector images, using the CIE distances
 * between intensity values as the similarity metric (the Gaussian
 * kernel for the image domain is evaluated using CIE distances).
 * A separate version of this filter will be designed for color
 * and vector images.
 *
 * Bilateral filtering is capable of reducing the noise in an image
 * by an order of magnitude while maintaining edges.
 *
 * The bilateral operator used here was described by Tomasi and
 * Manduchi (Bilateral Filtering for Gray and ColorImages. IEEE
 * ICCV. 1998.)
 *
 * \sa GaussianOperator
 * \sa RecursiveGaussianImageFilter
 * \sa DiscreteGaussianImageFilter
 * \sa AnisotropicDiffusionImageFilter
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 *
 * \ingroup ImageEnhancement
 * \ingroup ImageFeatureExtraction
 * \todo Support color images
 * \todo Support vector images
 * \ingroup ITKImageFeature
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageFeature/BilateralFilterAnImage,Bilateral Filter An Image}
 * \endsphinx
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT BilateralImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BilateralImageFilter);

  /** Standard class type aliases. */
  using Self = BilateralImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BilateralImageFilter, ImageToImageFilter);

  /** Image type information. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Superclass type alias. */
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using OutputPixelRealType = typename NumericTraits<OutputPixelType>::RealType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Typedef of double containers */
  using ArrayType = FixedArray<double, Self::ImageDimension>;

  /** Neighborhood iterator types. */
  using NeighborhoodIteratorType = ConstNeighborhoodIterator<TInputImage>;

  /** Kernel type alias. */
  using KernelType = Neighborhood<double, Self::ImageDimension>;
  using SizeType = typename KernelType::SizeType;
  using SizeValueType = typename KernelType::SizeValueType;

  /** Kernel iterator. */
  using KernelIteratorType = typename KernelType::Iterator;
  using KernelConstIteratorType = typename KernelType::ConstIterator;

  /** Gaussian image type */
  using GaussianImageType = Image<double, Self::ImageDimension>;

  /** Standard get/set macros for filter parameters.
   * DomainSigma is specified in the same units as the Image spacing.
   * RangeSigma is specified in the units of intensity. */
  itkSetMacro(DomainSigma, ArrayType);
  itkGetConstMacro(DomainSigma, const ArrayType);
  itkSetMacro(DomainMu, double);
  itkGetConstReferenceMacro(DomainMu, double);
  itkSetMacro(RangeSigma, double);
  itkGetConstMacro(RangeSigma, double);
  itkGetConstMacro(FilterDimensionality, unsigned int);
  itkSetMacro(FilterDimensionality, unsigned int);

  /** Convenience get/set methods for setting all domain parameters to the
   * same values.  */
  void
  SetDomainSigma(const double v)
  {
    m_DomainSigma.Fill(v);
  }

  /** Control automatic kernel size determination. When
   * automatic is "on", the kernel size is a function of the domain
   * sigma. When automatic is "off", the kernel size is whatever is
   * specified by the user.
   * \sa SetRadius() */
  itkBooleanMacro(AutomaticKernelSize);
  itkGetConstMacro(AutomaticKernelSize, bool);
  itkSetMacro(AutomaticKernelSize, bool);

  /** Set/Get the kernel radius, specified in pixels.  This parameter
   * is used only when AutomaticNeighborhoodSize is "off". */
  void
  SetRadius(const SizeValueType);

  itkSetMacro(Radius, SizeType);
  itkGetConstReferenceMacro(Radius, SizeType);

  /** Set/Get the number of samples in the approximation to the Gaussian
   * used for the range smoothing. Samples are only generated in the
   * range of [0, 4*m_RangeSigma]. Default is 100. */
  itkSetMacro(NumberOfRangeGaussianSamples, unsigned long);
  itkGetConstMacro(NumberOfRangeGaussianSamples, unsigned long);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputPixelType>));
  // End concept checking
#endif

protected:
  /** Constructor. */
  BilateralImageFilter();

  /** Destructor. */
  ~BilateralImageFilter() override = default;

  /** PrintSelf. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Do some setup before the ThreadedGenerateData */
  void
  BeforeThreadedGenerateData() override;

  /** Standard pipeline method. This filter is implemented as a multi-threaded
   * filter. */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

  /** BilateralImageFilter needs a larger input requested region than
   * the output requested region (larger by the size of the domain
   * Gaussian kernel).  As such, BilateralImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion() in order to
   * inform the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

private:
  /** The standard deviation of the gaussian blurring kernel in the image
      range. Units are intensity. */
  double m_RangeSigma;

  /** The standard deviation of the gaussian blurring kernel in each
      dimensional direction. Units match image spacing units. */
  ArrayType m_DomainSigma;

  /** Multiplier used to define statistical thresholds.  Gaussians are
   * only evaluated to m_DomainMu*m_DomainSigma or m_RangeMu*m_RangeSigma. */
  double m_DomainMu;
  double m_RangeMu;

  /** Number of dimensions to process. Default is all dimensions */
  unsigned int m_FilterDimensionality;

  /** Gaussian kernel used for smoothing in the spatial domain */
  KernelType m_GaussianKernel;
  SizeType   m_Radius;
  bool       m_AutomaticKernelSize;

  /** Variables for the lookup table of range gaussian values */
  unsigned long       m_NumberOfRangeGaussianSamples;
  double              m_DynamicRange;
  double              m_DynamicRangeUsed;
  std::vector<double> m_RangeGaussianTable;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBilateralImageFilter.hxx"
#endif

#endif
