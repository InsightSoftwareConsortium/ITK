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
#ifndef itkGPUImageToImageFilter_h
#define itkGPUImageToImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkGPUKernelManager.h"

namespace itk
{
/** \class GPUImageToImageFilter
 *
 * \brief class to abstract the behaviour of the GPU filters.
 *
 * GPUImageToImageFilter is the GPU version of ImageToImageFilter.
 * This class can accept both CPU and GPU image as input and output,
 * and apply filter accordingly. If GPU is available for use, then
 * GPUGenerateData() is called. Otherwise, GenerateData() in the
 * parent class (i.e., ImageToImageFilter) will be called.
 *
 * \ingroup ITKGPUCommon
 */
template <typename TInputImage,
          typename TOutputImage,
          typename TParentImageFilter = ImageToImageFilter<TInputImage, TOutputImage>>
class ITK_TEMPLATE_EXPORT GPUImageToImageFilter : public TParentImageFilter
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUImageToImageFilter);

  /** Standard class type aliases. */
  using Self = GPUImageToImageFilter;
  using Superclass = TParentImageFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUImageToImageFilter, TParentImageFilter);

  /** Superclass type alias. */
  using DataObjectIdentifierType = typename Superclass::DataObjectIdentifierType;
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

  // macro to set if GPU is used
  itkSetMacro(GPUEnabled, bool);
  itkGetConstMacro(GPUEnabled, bool);
  itkBooleanMacro(GPUEnabled);

  void
  GenerateData() override;
  virtual void
  GraftOutput(typename itk::GPUTraits<TOutputImage>::Type * output);
  virtual void
  GraftOutput(const DataObjectIdentifierType & key, typename itk::GPUTraits<TOutputImage>::Type * output);

protected:
  void
  GraftOutput(DataObject * output) override;
  void
  GraftOutput(const DataObjectIdentifierType & key, DataObject * output) override;
  GPUImageToImageFilter();
  ~GPUImageToImageFilter() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  virtual void
  GPUGenerateData()
  {}

  // GPU kernel manager
  typename GPUKernelManager::Pointer m_GPUKernelManager;

  // GPU kernel handle - kernel should be defined in specific filter (not in the
  // base class)
  // int m_KernelHandle;

private:
  bool m_GPUEnabled{ true };
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUImageToImageFilter.hxx"
#endif

#endif
