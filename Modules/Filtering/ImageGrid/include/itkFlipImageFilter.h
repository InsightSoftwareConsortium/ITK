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
#ifndef itkFlipImageFilter_h
#define itkFlipImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"

namespace itk
{
/** \class FlipImageFilter
 * \brief Flips an image across user specified axes.
 *
 * FlipImageFilter flips an image across user specified axes.
 * The flip axes are set via method SetFlipAxes( array ) where
 * the input is a FixedArray<bool,ImageDimension>. The image
 * is flipped across axes for which array[i] is true.
 *
 * In terms of grid coordinates the image is flipped within
 * the LargestPossibleRegion of the input image. As such,
 * the LargestPossibleRegion of the output image is the same
 * as the input.
 *
 * In terms of geometric coordinates, the output origin
 * is such that the image is flipped with respect to the
 * coordinate axes.
 *
 * \ingroup GeometricTransform
 * \ingroup MultiThreaded
 * \ingroup Streamed
 * \ingroup ITKImageGrid
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageGrid/FlipAnImageOverSpecifiedAxes,Flip An Image Over Specified Axes}
 * \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT FlipImageFilter : public ImageToImageFilter<TImage, TImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FlipImageFilter);

  /** Standard class type aliases. */
  using Self = FlipImageFilter;
  using Superclass = ImageToImageFilter<TImage, TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FlipImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Inherited types */
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  /** Index related types */
  using IndexType = typename TImage::IndexType;
  using IndexValueType = typename IndexType::IndexValueType;

  /** FlipAxesArray type */
  using FlipAxesArrayType = FixedArray<bool, Self::ImageDimension>;

  /** Set/Get the axis to be flipped. The image is flipped along axes
   * for which array[i] is true. Default is false. */
  itkSetMacro(FlipAxes, FlipAxesArrayType);
  itkGetConstMacro(FlipAxes, FlipAxesArrayType);

  /** Controls how the output origin is computed. If FlipAboutOrigin is
   * "On", the flip will occur about the origin of the axis, otherwise,
   * the flip will occur about the center of the axis. Default is "On". */
  itkBooleanMacro(FlipAboutOrigin);
  itkGetConstMacro(FlipAboutOrigin, bool);
  itkSetMacro(FlipAboutOrigin, bool);

  /** FlipImageFilter produces an image with different origin and
   * direction than the input image. As such, FlipImageFilter needs to
   * provide an implementation for GenerateOutputInformation() in
   * order to inform the pipeline execution model.
   * The output image meta information is obtained by permuting the input
   * image meta information. The original documentation of this method is
   * below.
   * \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

  /** FlipImageFilter needs different input requested region than the output
   * requested region.  As such, FlipImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   * The required input requested region is obtained by permuting the index and
   * size of the output requested region.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

protected:
  FlipImageFilter();
  ~FlipImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** FlipImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * DynamicThreadedGenerateData().  DynamicThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

private:
  FlipAxesArrayType m_FlipAxes;
  bool              m_FlipAboutOrigin{ true };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFlipImageFilter.hxx"
#endif

#endif
