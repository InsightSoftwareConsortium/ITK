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
#ifndef itkPadImageFilter_h
#define itkPadImageFilter_h

#include "itkPadImageFilterBase.h"

#include "itkImageBoundaryCondition.h"

namespace itk
{
/**
 *\class PadImageFilter
 * \brief Increase the image size by padding. Superclass for filters that fill
 * in extra pixels.
 *
 *
 * \image html PadImageFilter.png "Visual explanation of padding regions."
 *
 * PadImageFilter changes the image boundary of an image by padding each
 * dimension with subclass defined algorithms.  The number of pixels to pad
 * for the upper and lower bounds of each dimension must be specified.
 *
 * This filter is implemented as a multithreaded filter.  It provides a
 * ThreadedGenerateData() method for its implementation.
 *
 * \ingroup GeometricTransform
 * \sa WrapPadImageFilter, MirrorPadImageFilter, ConstantPadImageFilter
 *
 * \ingroup ITKImageGrid
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT PadImageFilter : public PadImageFilterBase<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PadImageFilter);

  /** Standard class type aliases. */
  using Self = PadImageFilter;
  using Superclass = PadImageFilterBase<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedef to describe the output and input image region types. */
  using OutputImageRegionType = typename TOutputImage::RegionType;
  using InputImageRegionType = typename TInputImage::RegionType;

  /** Typedef to describe the type of pixel. */
  using OutputImagePixelType = typename TOutputImage::PixelType;
  using InputImagePixelType = typename TInputImage::PixelType;

  /** Typedef to describe the output and input image index and size types. */
  using OutputImageIndexType = typename TOutputImage::IndexType;
  using InputImageIndexType = typename TInputImage::IndexType;
  using OutputImageSizeType = typename TOutputImage::SizeType;
  using InputImageSizeType = typename TInputImage::SizeType;
  using SizeType = typename TInputImage::SizeType;
  using SizeValueType = typename TInputImage::SizeValueType;

  /** Typedef to describe the boundary condition. */
  using BoundaryConditionType = ImageBoundaryCondition<TInputImage, TOutputImage>;
  using BoundaryConditionPointerType = BoundaryConditionType *;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PadImageFilter, PadImageFilterBase);

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Set/Get the output image padding.  Default is no padding
   *  (same as input). */
  itkSetMacro(PadLowerBound, SizeType);
  itkSetMacro(PadUpperBound, SizeType);
  itkGetConstReferenceMacro(PadLowerBound, SizeType);
  itkGetConstReferenceMacro(PadUpperBound, SizeType);
  itkSetVectorMacro(PadLowerBound, const SizeValueType, ImageDimension);
  itkSetVectorMacro(PadUpperBound, const SizeValueType, ImageDimension);

  void
  SetPadBound(const InputImageSizeType & bound)
  {
    this->SetPadLowerBound(bound);
    this->SetPadUpperBound(bound);
  }

protected:
  PadImageFilter();
  ~PadImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** PadImageFilter produces an image which is a different resolution
   * than its input image.  As such, PadImageFilter needs to
   * provide an implementation for GenerateOutputInformation() in order
   * to inform the pipeline execution model.  The original
   * documentation of this method is below.
   * \sa ProcessObject::GenerateOutputInformaton()  */
  void
  GenerateOutputInformation() override;

private:
  SizeType m_PadLowerBound;
  SizeType m_PadUpperBound;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPadImageFilter.hxx"
#endif

#endif
