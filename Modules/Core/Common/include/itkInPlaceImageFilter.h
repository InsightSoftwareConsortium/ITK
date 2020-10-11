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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkInPlaceImageFilter_h
#define itkInPlaceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkIsSame.h"

namespace itk
{
/** \class InPlaceImageFilter
 * \brief Base class for filters that take an image as input and overwrite that image as the output
 *
 * InPlaceImageFilter is the base class for all process objects whose
 * output image data is constructed by overwriting the input image
 * data. In other words, the output bulk data is the same block of
 * memory as the input bulk data.  This filter provides the mechanisms
 * for in place image processing while maintaining general pipeline
 * mechanics. InPlaceImageFilters use less memory than standard
 * ImageToImageFilters because the input buffer is reused as the
 * output buffer.  However, this benefit does not come without a cost.
 * Since the filter overwrites its input, the ownership of the bulk
 * data is transitioned from the input data object to the output data
 * object.  When a data object has multiple consumers with one
 * of the consumers being an in place filter, the in place filter
 * effectively destroys the bulk data for the data object. Upstream
 * filters will then have to re-execute to regenerate the data object's
 * bulk data for the remaining consumers.
 *
 * Since an InPlaceImageFilter reuses the input bulk data memory for the
 * output bulk data memory, the input image type must match the output
 * image type.  If the input and output image types are not identical,
 * the filter reverts to a traditional ImageToImageFilter behaviour
 * where an output image is allocated. Additionally, the requested
 * region of the output must match that of the input. In place
 * operation can also be controlled (when the input and output image
 * type match) via the methods InPlaceOn() and InPlaceOff().
 *
 * Subclasses of InPlaceImageFilter must take extra care in how they
 * manage memory using (and perhaps overriding) the implementations of
 * ReleaseInputs() and AllocateOutputs() provided here.
 *
 * \ingroup ImageFilters
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/FilterImageWithoutCopying,Filter Image Without Copying Its Data}
 * \sphinxexample{Core/Common/InPlaceFilterOfImage,In Place Filter Of Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT InPlaceImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(InPlaceImageFilter);

  /** Standard class type aliases. */
  using Self = InPlaceImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(InPlaceImageFilter, ImageToImageFilter);

  /** Superclass type alias. */
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using OutputImagePixelType = typename Superclass::OutputImagePixelType;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** In place operation can be turned on and off. Asking for
   * in-place operation, i.e. calling SetInplace(true) or InplaceOn(),
   * will be effective only if CanRunInPlace also returns true.
   * By default CanRunInPlace checks whether the input and output
   * image type match. */
  itkSetMacro(InPlace, bool);
  itkGetConstMacro(InPlace, bool);
  itkBooleanMacro(InPlace);

  /** Can the filter run in place? To do so, the filter's first input
   * and output must have the same dimension and pixel type. This
   * method can be used in conjunction with the InPlace ivar to
   * determine whether a particular use of the filter is really
   * running in place. Some filters may be able to optimize their
   * operation if the InPlace is true and CanRunInPlace is true.
   * CanRunInPlace may also be overridden by InPlaceImageFilter
   * subclasses to fine tune its behavior. */
  virtual bool
  CanRunInPlace() const;

protected:
  InPlaceImageFilter() = default;
  ~InPlaceImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** The GenerateData method normally allocates the buffers for all
   * of the outputs of a filter. Since InPlaceImageFilter's can use an
   * overwritten version of the input for its output, the output
   * buffer should not be allocated. When possible, we graft the input
   * to the filter to the output.  If an InPlaceFilter has multiple
   * outputs, then it would need to override this method to graft one
   * of its outputs and allocate the remaining. If a filter is
   * threaded (i.e. it provides an implementation of
   * ThreadedGenerateData()), this method is called automatically. If
   * an InPlaceFilter is not threaded (i.e. it provides an
   * implementation of GenerateData()), then this method (or
   * equivalent) must be called in GenerateData(). */
  void
  AllocateOutputs() override
  {
    this->InternalAllocateOutputs(IsSame<TInputImage, TOutputImage>());
  }

  /** InPlaceImageFilter may transfer ownership of the input bulk data
   * to the output object.  Once the output object owns the bulk data
   * (done in AllocateOutputs()), the input object must release its
   * hold on the bulk data.  ProcessObject::ReleaseInputs() only
   * releases the input bulk data when the user has set the
   * ReleaseDataFlag.  InPlaceImageFilter::ReleaseInputs() also
   * releases the input that it has overwritten.
   *
   * \sa ProcessObject::ReleaseInputs() */
  void
  ReleaseInputs() override;

  /** This methods should only be called during the GenerateData phase
   *  of the pipeline. This method return true if the input image's
   *  bulk data is the same as the output image's data.
   */
  itkGetConstMacro(RunningInPlace, bool);

private:
  // the type are different we can't run in place
  void
  InternalAllocateOutputs(const FalseType &)
  {
    this->m_RunningInPlace = false;
    this->Superclass::AllocateOutputs();
  }

  void
  InternalAllocateOutputs(const TrueType &);

  bool m_InPlace{ true }; // enable the possibility of in-place
  bool m_RunningInPlace{ false };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkInPlaceImageFilter.hxx"
#endif

#endif
