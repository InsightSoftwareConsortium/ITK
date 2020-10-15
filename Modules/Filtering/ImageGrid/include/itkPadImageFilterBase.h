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
#ifndef itkPadImageFilterBase_h
#define itkPadImageFilterBase_h

#include "itkImageToImageFilter.h"

#include "itkImageBoundaryCondition.h"

namespace itk
{
/**
 *\class PadImageFilterBase
 * \brief Increase the image size by padding. Superclass for filters that fill
 * in extra pixels.
 *
 *
 * \image html PadImageFilter.png "Visual explanation of padding regions."
 *
 * PadImageFilterBase changes the image boundary of an image by padding each
 * dimension with subclass defined algorithms.  The padded region must be
 * specified by the subclasses.
 *
 * This filter is implemented as a multithreaded filter.  It provides a
 * ThreadedGenerateData() method for its implementation.
 *
 * \ingroup GeometricTransform
 * \sa WrapPadImageFilterBase, MirrorPadImageFilterBase, ConstantPadImageFilterBase
 *
 * \ingroup ITKImageGrid
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT PadImageFilterBase : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PadImageFilterBase);

  /** Standard class type aliases. */
  using Self = PadImageFilterBase;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
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
  itkTypeMacro(PadImageFilterBase, ImageToImageFilter);

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Set/get the boundary condition. */
  itkSetMacro(BoundaryCondition, BoundaryConditionPointerType);
  itkGetConstMacro(BoundaryCondition, BoundaryConditionPointerType);

protected:
  PadImageFilterBase();
  ~PadImageFilterBase() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** PadImageFilterBase needs a smaller input requested region than
   * output requested region.  As such, PadImageFilterBase needs to
   * provide an implementation for GenerateInputRequestedRegion() in
   * order to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion()  */
  void
  GenerateInputRequestedRegion() override;

  /** This class can be multithreaded. */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  /** Method for subclasses to set the boundary condition. */
  void
  InternalSetBoundaryCondition(const BoundaryConditionPointerType boundaryCondition);

private:
  BoundaryConditionPointerType m_BoundaryCondition;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPadImageFilterBase.hxx"
#endif

#endif
