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
#ifndef itkImageFilterToVideoFilterWrapper_h
#define itkImageFilterToVideoFilterWrapper_h

#include "itkVideoToVideoFilter.h"

namespace itk
{

/**
 *\class ImageFilterToVideoFilterWrapper
 * \brief Wrap an ImageToImageFilter as a VideoToVideoFilter that operates on
 * a single frame at a time
 *
 * This filter wrapper allows all of the standard ITK image filters to be used
 * in a video pipeline. This is done by instantiating the image filter, setting
 * its parameters, and then using the SetImageFilter() method of this wrapper
 * to use the filter to process each in a video pipeline. An instance of this
 * wrapper must be templated over the appropriate image filter type.
 *
 * \ingroup ITKVideoFiltering
 */
template <typename TImageToImageFilter>
class ITK_TEMPLATE_EXPORT ImageFilterToVideoFilterWrapper
  : public VideoToVideoFilter<itk::VideoStream<typename TImageToImageFilter::InputImageType>,
                              itk::VideoStream<typename TImageToImageFilter::OutputImageType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageFilterToVideoFilterWrapper);

  /** Standard class type aliases */
  using ImageFilterType = TImageToImageFilter;
  using InputFrameType = typename ImageFilterType::InputImageType;
  using OutputFrameType = typename ImageFilterType::OutputImageType;
  using InputVideoStreamType = itk::VideoStream<InputFrameType>;
  using OutputVideoStreamType = itk::VideoStream<OutputFrameType>;

  using Self = ImageFilterToVideoFilterWrapper<ImageFilterType>;
  using Superclass = VideoToVideoFilter<InputVideoStreamType, OutputVideoStreamType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  itkNewMacro(Self);

  itkTypeMacro(ImageFilterToVideoFilterWrapper, VideoToVideoFilter);

  /** Set the filter to use in the internal pipeline */
  itkSetObjectMacro(ImageFilter, ImageFilterType);
  itkGetModifiableObjectMacro(ImageFilter, ImageFilterType);

protected:
  /** Constructor and Destructor */
  ImageFilterToVideoFilterWrapper();
  ~ImageFilterToVideoFilterWrapper() override = default;

  /** PrintSelf */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Since we just set up a mini image pipeline inside, we override
   * TemporalStreamingGenerateData*/
  void
  TemporalStreamingGenerateData() override;

  /** Pointer to filter to use for internal filter */
  typename ImageFilterType::Pointer m_ImageFilter;

private:
}; // end class ImageFilterToVideoFilterWrapper

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageFilterToVideoFilterWrapper.hxx"
#endif

#endif
