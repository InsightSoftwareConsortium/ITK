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
#ifndef itkFrameDifferenceVideoFilter_h
#define itkFrameDifferenceVideoFilter_h

#include "itkVideoToVideoFilter.h"

namespace itk
{

/**
 *\class FrameDifferenceVideoFilter
 * \brief Compute the squared intensity difference between frame x and frame
 * x+n
 *
 * This filter computes the squared intensity difference between a frame and
 * the nth previous frame.
 *
 * \ingroup ITKVideoFiltering
 */
template <typename TInputVideoStream, typename TOutputVideoStream>
class ITK_TEMPLATE_EXPORT FrameDifferenceVideoFilter : public VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FrameDifferenceVideoFilter);

  /** Standard class type aliases */
  using InputVideoStreamType = TInputVideoStream;
  using OutputVideoStreamType = TOutputVideoStream;
  using Self = FrameDifferenceVideoFilter<InputVideoStreamType, OutputVideoStreamType>;
  using Superclass = VideoToVideoFilter<InputVideoStreamType, OutputVideoStreamType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  using InputFrameType = typename TInputVideoStream::FrameType;
  using InputPixelType = typename InputFrameType::PixelType;
  using InputFrameSpatialRegionType = typename InputFrameType::RegionType;
  using OutputFrameType = typename TOutputVideoStream::FrameType;
  using OutputPixelType = typename OutputFrameType::PixelType;
  using OutputFrameSpatialRegionType = typename OutputFrameType::RegionType;

  itkNewMacro(Self);

  itkTypeMacro(FrameDifferenceVideoFilter, VideoToVideoFilter);

  /** Get/Set the offset for computing frame differences. Defaults to 1. */
  void
  SetFrameOffset(SizeValueType numFrames);
  SizeValueType
  GetFrameOffset();

protected:
  /** Constructor and Destructor */
  FrameDifferenceVideoFilter();
  ~FrameDifferenceVideoFilter() override = default;

  /** PrintSelf */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** FrameDifferenceVideoFilter is implemented as a temporal streaming and
   * spatially multithreaded filter, so we override ThreadedGenerateData */
  void
  ThreadedGenerateData(const OutputFrameSpatialRegionType & outputRegionForThread, int threadId) override;

private:
}; // end class FrameDifferenceVideoFilter

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFrameDifferenceVideoFilter.hxx"
#endif

#endif
