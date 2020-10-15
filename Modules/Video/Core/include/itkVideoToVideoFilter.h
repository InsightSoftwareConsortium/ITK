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
#ifndef itkVideoToVideoFilter_h
#define itkVideoToVideoFilter_h

#include "itkVideoSource.h"
#include "itkVideoStream.h"

namespace itk
{

/**
 *\class VideoToVideoFilter
 * \brief Base class for filters that use a VideoStream as input and output
 *
 * VideoToVideoFilter is the base class for all process objects that output
 * VideoStream data and require VideoStream data as input. This class defines
 * the SetInput() method for setting the input to a filter.
 *
 * An implementation of GenerateInputRequestedRegion() is provided here that
 * uses the implementation from TemporalProcessObject to generate input
 * temporal regions and then provides its own mechanism for generating input
 * spatial regions. The default implementation simply takes the requested
 * spatial region from the first frame of output and uses that as the requested
 * region for each of the input frames.
 *
 * \ingroup ITKVideoCore
 */
template <typename TInputVideoStream, typename TOutputVideoStream>
class ITK_TEMPLATE_EXPORT VideoToVideoFilter : public VideoSource<TOutputVideoStream>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VideoToVideoFilter);

  /** Standard class type aliases */
  using InputVideoStreamType = TInputVideoStream;
  using OutputVideoStreamType = TOutputVideoStream;
  using Self = VideoToVideoFilter<InputVideoStreamType, OutputVideoStreamType>;
  using Superclass = VideoSource<OutputVideoStreamType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  /** Superclass type alias */
  using OutputFrameType = typename Superclass::OutputFrameType;
  using OutputFrameSpatialRegionType = typename Superclass::OutputFrameSpatialRegionType;

  /** Input type alias */
  using InputFrameType = typename InputVideoStreamType::FrameType;
  using InputFrameSpatialRegionType = typename InputVideoStreamType::SpatialRegionType;
  using InputFrameIndexType = typename InputVideoStreamType::IndexType;
  using InputFramePixelType = typename InputVideoStreamType::PixelType;
  using InputFramePointType = typename InputVideoStreamType::PointType;
  using InputFrameSpacingType = typename InputVideoStreamType::SpacingType;
  using InputFrameSizeType = typename InputVideoStreamType::SizeType;
  using InputFrameDirectionType = typename InputVideoStreamType::DirectionType;

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoToVideoFilter, VideoSource);

  /** Set the input VideoStream for this temporal process object */
  using Superclass::SetInput;
  virtual void
  SetInput(const InputVideoStreamType * videoStream);

  virtual void
  SetInput(unsigned int idx, const InputVideoStreamType * videoStream);

  /** Get the input VideoSream for this temporal process object */
  const InputVideoStreamType *
  GetInput() const;

  const InputVideoStreamType *
  GetInput(unsigned int idx) const;

  /** Extend UpdateOutputInformation to propagate largest possible spatial
   * region as well as temporal region. The default implementation here will
   * use the largest spatial region from the first input frame to set the
   * largest spatial region of each of the output frames. This will need to be
   * overwritten for filters that need different behavior (eg: need edge pixels
   * or different spatial regions for different frames) */
  void
  UpdateOutputInformation() override;

protected:
  /** Get a non-const version of the input for internal use when setting
   * input's requested regions. This is the only time input should be modified
   */
  InputVideoStreamType *
  GetInput();

  InputVideoStreamType *
  GetInput(unsigned int idx);

  /** Override GenerateOutputRequestedRegion to handle the case where no
   * requested spatial region has been set for the frames. By default, we set
   * the requested spatial region of each frame to be its largest possible
   * spatial region. */
  void
  GenerateOutputRequestedRegion(DataObject * output) override;

  /** Extend the default implementation of GenerateInputRequestedRegion from
   * TemporalProcessObject to propagate spatial regions as well as temporal
   * regions. This default implementation takes the requested spatial region
   * from the first requested output frame and applies it to all of the
   * requested input frames. */
  void
  GenerateInputRequestedRegion() override;

  /** Method that gets called before individual temporal requests are
   * dispatched by GenerateData. The default implementation makes sure that
   * the input's buffer can hold enough frames for a single input request. */
  void
  BeforeTemporalStreamingGenerateData() override;

  VideoToVideoFilter();
  ~VideoToVideoFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
}; // end class VideoToVideoFilter

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVideoToVideoFilter.hxx"
#endif

#endif
