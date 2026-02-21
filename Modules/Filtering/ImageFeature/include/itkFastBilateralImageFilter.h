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
#ifndef itkFastBilateralImageFilter_h
#define itkFastBilateralImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkLinearInterpolateImageFunction.h"

namespace itk
{
/**
 * \class FastBilateralImageFilter
 * \brief A fast approximation to the bilateral filter
 * \ingroup ITKImageFeature
 *
 * This filter is a fast approximation to the bilateral filter.
 * Blurring is performed on an image based on the distance of pixels in
 * both space and intensity.
 *
 * The algorithm used was originally proposed by Paris and
 * Durand [1].
 *
 * Instead of calculating a kernel for every pixel in
 * an image, this filter places the values of each pixel into a higher
 * dimensional image determined by the position and intensity of a pixel.
 * How many bins are used is determined by the sigma values provided
 * to the filter. Larger sigmas will result in more aggressive downsampling
 * and less running time overall. After the data of an image
 * has been organized into bins, a DiscreteGaussianImageFilter is applied.
 * Finally, the output image is constructed by interpolating the
 * values of the output pixels from the blurred higher
 * dimensional image.
 *
 * This filter is great for large spatial sigmas. Numerical differences to
 * BilateralImageFilter are negligible for most purposes.
 *
 * NOTE: This filter is slow for small intensity sigmas and large pixel types
 * (e.g. short, int, or float with large intensity range).
 *
 *
 * [1] Sylvain Paris and Frédo Durand,
 *     A Fast Approximation of the Bilateral Filter using a Signal Processing
 *     Approach,
 *     European Conference on Computer Vision (ECCV'06)
 *
 * \sa BilateralImageFilter
 * \sa GaussianOperator
 * \sa AnisotropicDiffusionImageFilter
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 *
 * \ingroup ImageEnhancement
 * \ingroup ImageFeatureExtraction
 *
 * \todo Support for color images
 * \todo Support for vector images
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT FastBilateralImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FastBilateralImageFilter);

  /** Standard class typedefs. */
  using Self = FastBilateralImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(FastBilateralImageFilter);

  /** Dimensionality of the input image. Dimensionality of the output image
   *  is assumed to be the same. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Input image typedefs. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;
  using InputImageSpacingType = typename TInputImage::SpacingType;
  using InputImageSizeType = typename TInputImage::SizeType;
  using InputImageIndexType = typename TInputImage::IndexType;

  /** Input image iterator type. */
  using InputImageConstIteratorType = ImageRegionConstIteratorWithIndex<TInputImage>;

  /** Output image typedefs. */
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename TOutputImage::Pointer;

  /** Output image iterator type. */
  using OutputImageIteratorType = ImageRegionIterator<TOutputImage>;

  /** Pixel types. */
  using OutputPixelType = typename TOutputImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;

  /** Typedef for an array of doubles that specifies the DomainSigma
   *  in each spacial dimension. */
  using DomainSigmaArrayType = FixedArray<double, Self::ImageDimension>;

  /** Standard get/set macros for filter parameters.
   *  DomainSigma is specified in the same units as the Image spacing.
   *  RangeSigma is specified in the units of intensity. */
  itkGetConstMacro(DomainSigma, const DomainSigmaArrayType);
  itkSetMacro(DomainSigma, DomainSigmaArrayType);
  itkGetConstMacro(RangeSigma, double);
  itkSetMacro(RangeSigma, double);

  /** Convenience set method for setting all domain standard deviations to the
   *  same value. */
  void
  SetDomainSigma(const double v)
  {
    m_DomainSigma.Fill(v);
  }

protected:
  /** Default Constructor. Default value for DomainSigma is 4. Default
   *  value for RangeSigma is 50. These values were chosen match those of the
   *  BilateralImageFilter */
  FastBilateralImageFilter()
  {
    m_DomainSigma.Fill(4.0);
    m_RangeSigma = 50.0;
  }

  virtual ~FastBilateralImageFilter() {}

  /*
   * The FastBilateralImageFilter needs a larger input requested
   * region than the size of the output requested region. Like
   * the BilateralImageFilter, the FastBilateralImageFilter needs
   * an amount of padding in each dimension based on the domain sigma.
   */
  void
  GenerateInputRequestedRegion() override;

  /** Standard pipeline method */
  void
  GenerateData() override;

  /** Method to print member variables to an output stream */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** The type of image to use as the higher dimensional grid.
   * The blurring is performed on this image type. */
  using GridType = typename itk::Image<float, Self::ImageDimension + 1>;

  /** Grid types */
  using GridPixelType = typename GridType::PixelType;
  using GridIndexType = typename GridType::IndexType;
  using GridSizeType = typename GridType::SizeType;
  using GridSizeValueType = typename Size<Self::ImageDimension + 1>::SizeValueType;
  using GridRegionType = typename GridType::RegionType;

  /** Grid image iterator type. */
  using GridImageIteratorType = ImageRegionIterator<GridType>;
  using GridImageConstIteratorType = ImageRegionConstIterator<GridType>;

  /** The type of blurring to use on the grid. */
  using BlurType = DiscreteGaussianImageFilter<GridType, GridType>;

  /** The type of interpolation done to calculate output pixels. */
  using InterpolatorType = LinearInterpolateImageFunction<GridType, float>;
  using InterpolatedIndexType = typename InterpolatorType::ContinuousIndexType;

  double               m_RangeSigma;
  DomainSigmaArrayType m_DomainSigma;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFastBilateralImageFilter.hxx"
#endif

#endif // itkFastBilateralImageFilter
