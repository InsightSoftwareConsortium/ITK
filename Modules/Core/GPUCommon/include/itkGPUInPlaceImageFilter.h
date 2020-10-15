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

#ifndef itkGPUInPlaceImageFilter_h
#define itkGPUInPlaceImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkGPUImageToImageFilter.h"

namespace itk
{
/** \class GPUInPlaceImageFilter
 * \brief Base class for GPU filters that take an image as input and overwrite that image as the output
 *
 * This class is the base class for GPU inplace filter. The template parameter for parent class type
 * must be InPlaceImageFilter type so that the GPU superclass of this class can be correctly defined
 * (NOTE: TParentImageFilter::Superclass is used to define GPUImageToImageFilter class).
 *
 * \ingroup ITKGPUCommon
 */
template <typename TInputImage,
          typename TOutputImage = TInputImage,
          typename TParentImageFilter = InPlaceImageFilter<TInputImage, TOutputImage>>
class ITK_TEMPLATE_EXPORT GPUInPlaceImageFilter
  : public GPUImageToImageFilter<TInputImage, TOutputImage, TParentImageFilter>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GPUInPlaceImageFilter);

  /** Standard class type aliases. */
  using Self = GPUInPlaceImageFilter;
  using GPUSuperclass = GPUImageToImageFilter<TInputImage, TOutputImage, TParentImageFilter>;
  using CPUSuperclass = TParentImageFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUInPlaceImageFilter, GPUImageToImageFilter);

  /** Superclass type alias. */
  using OutputImageType = typename GPUSuperclass::OutputImageType;
  using OutputImagePointer = typename GPUSuperclass::OutputImagePointer;
  using OutputImageRegionType = typename GPUSuperclass::OutputImageRegionType;
  using OutputImagePixelType = typename GPUSuperclass::OutputImagePixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

protected:
  GPUInPlaceImageFilter();
  ~GPUInPlaceImageFilter() override;

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
  AllocateOutputs() override;

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
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUInPlaceImageFilter.hxx"
#endif

#endif
