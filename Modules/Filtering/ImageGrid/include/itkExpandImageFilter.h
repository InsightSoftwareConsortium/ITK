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
#ifndef itkExpandImageFilter_h
#define itkExpandImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"

namespace itk
{
/**
 *\class ExpandImageFilter
 * \brief Expand the size of an image by an integer factor in each
 * dimension.
 *
 * ExpandImageFilter increases the size of an image by an integer
 * factor in each dimension using a interpolation method.
 * The output image size in each dimension is given by:
 *
 * OutputSize[j] = InputSize[j] * ExpandFactors[j]
 *
 * The output values are obtained by interpolating the input image.
 * The default interpolation type used is the LinearInterpolateImageFunction.
 * The user can specify a particular interpolation function via
 * SetInterpolator(). Note that the input interpolator must derive
 * from base class InterpolateImageFunction.
 *
 * This filter will produce an output with different pixel spacing
 * that its input image such that:
 *
 * OutputSpacing[j] = InputSpacing[j] / ExpandFactors[j]
 *
 * The filter is templated over the input image type and the output
 * image type.
 *
 * This filter is implemented as a multithreaded filter and supports
 * streaming.
 *
 * This filter assumes that the input and output image has the same
 * number of dimensions.
 *
 * \sa InterpolateImageFunction
 * \sa LinearInterpolationImageFunction
 *
 * \ingroup GeometricTransform
 * \ingroup ITKImageGrid
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ExpandImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ExpandImageFilter);

  /** Standard class type aliases. */
  using Self = ExpandImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExpandImageFilter, ImageToImageFilter);

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Inherit some types from superclass. */
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputPixelType = typename OutputImageType::PixelType;
  using InputImagePointer = typename InputImageType::Pointer;
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Typedef support for the interpolation function. */
  using CoordRepType = double;
  using InterpolatorType = InterpolateImageFunction<InputImageType, CoordRepType>;
  using InterpolatorPointer = typename InterpolatorType::Pointer;
  using DefaultInterpolatorType = LinearInterpolateImageFunction<InputImageType, CoordRepType>;

  /** Get/Set the interpolator function. */
  itkSetObjectMacro(Interpolator, InterpolatorType);
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** The type of the expand factors representation */
  using ExpandFactorsType = FixedArray<unsigned int, ImageDimension>;

  /** Set the expand factors. Values are clamped to
   * a minimum value of 1. Default is 1 for all dimensions. */
  itkSetMacro(ExpandFactors, ExpandFactorsType);
  virtual void
  SetExpandFactors(const unsigned int factor);

  /** Get the expand factors. */
  itkGetConstReferenceMacro(ExpandFactors, ExpandFactorsType);


  /** ExpandImageFilter produces an image which is a different resolution and
   * with a different pixel spacing than its input image.  As such,
   * ExpandImageFilter needs to provide an implementation for
   * UpdateOutputInformation() in order to inform the pipeline execution model.
   * The original documentation of this method is below.
   * \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

  /** ExpandImageFilter needs a smaller input requested region than the output
   * requested region.  As such, ShrinkImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform
   * the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

protected:
  ExpandImageFilter();
  ~ExpandImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** ExpandImageFilter is implemented as a multithreaded filter.  Therefore,
   * this implementation provides a DynamicThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling DynamicThreadedGenerateData().
   * DynamicThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  /** This method is used to set the state of the filter before
   * multi-threading. */
  void
  BeforeThreadedGenerateData() override;

private:
  ExpandFactorsType   m_ExpandFactors;
  InterpolatorPointer m_Interpolator;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkExpandImageFilter.hxx"
#endif

#endif
