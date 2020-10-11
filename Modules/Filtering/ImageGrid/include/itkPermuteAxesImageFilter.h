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
#ifndef itkPermuteAxesImageFilter_h
#define itkPermuteAxesImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"

namespace itk
{
/** \class PermuteAxesImageFilter
 * \brief Permutes the image axes according to a user specified order.
 *
 * PermuateAxesImageFilter permutes the image axes according to a
 * user specified order. The permutation order is set via method
 * SetOrder( order ) where the input is an array of ImageDimension
 * number of unsigned int. The elements of the array must be a rearrangment
 * of the numbers from 0 to ImageDimension - 1.
 *
 * The i-th axis of the output image corresponds with the order[i]-th
 * axis of the input image.
 *
 * The output meta image information (LargestPossibleRegion, spacing, origin)
 * is computed by permuting the corresponding input meta information.
 *
 * \ingroup GeometricTransform
 * \ingroup MultiThreaded
 * \ingroup Streamed
 * \ingroup ITKImageGrid
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageGrid/PermuteAxesOfAnImage,Permute Axes Of An Image}
 * \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT PermuteAxesImageFilter : public ImageToImageFilter<TImage, TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PermuteAxesImageFilter);

  /** Standard class type aliases. */
  using Self = PermuteAxesImageFilter;
  using Superclass = ImageToImageFilter<TImage, TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PermuteAxesImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Inherited types */
  using InputImagePointer = typename Superclass::InputImagePointer;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  /** PermuteOrderArray type. */
  using PermuteOrderArrayType = FixedArray<unsigned int, Self::ImageDimension>;

  /** Set the permutation order.  The elements of order must be
   * a rearrangement of the numbers from 0 to ImageDimension - 1. */
  void
  SetOrder(const PermuteOrderArrayType & order);

  /** Get the permutation order. */
  itkGetConstReferenceMacro(Order, PermuteOrderArrayType);

  /** Get the inverse permutation order. */
  itkGetConstReferenceMacro(InverseOrder, PermuteOrderArrayType);

protected:
  /** PermuteAxesImageFilter produces an image which is a different
   * resolution and with a different pixel spacing than its input
   * image.  As such, PermuteAxesImageFilter needs to provide an
   * implementation for GenerateOutputInformation() in order to inform
   * the pipeline execution model.  The original documentation of this
   * method is below.
   * \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

  /** PermuteAxesImageFilter needs different input requested region than the output
   * requested region.  As such, PermuteAxesImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

  PermuteAxesImageFilter();
  ~PermuteAxesImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** PermuteAxesImageFilter can be implemented as a multithreaded filter.
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
  PermuteOrderArrayType m_Order;
  PermuteOrderArrayType m_InverseOrder;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPermuteAxesImageFilter.hxx"
#endif

#endif
