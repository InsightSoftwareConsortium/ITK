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
#ifndef itkVideoFileReader_h
#define itkVideoFileReader_h

#include "itkVideoSource.h"
#include "itkVideoIOFactory.h"
#include "itkDefaultConvertPixelTraits.h"

namespace itk
{

/**
 *\class VideoFileReader
 * \brief Reader that creates a VideoStream
 *
 * This class is responsible for reading video information from files. It is a
 * subclass of VideoSource, giving it functionality to connect to other
 * TemporalProcessObject classes (specifically, VideoToVideoFilter classes). It
 * uses the temporal streaming implementation provided by TemporalProcessObject
 * to load a single frame at a time into the frame buffer of the output
 * VideoSource.
 *
 * \ingroup ITKVideoIO
 */
template <typename TOutputVideoStream>
class ITK_TEMPLATE_EXPORT VideoFileReader : public VideoSource<TOutputVideoStream>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VideoFileReader);

  /** Standard class type aliases. */
  using Self = VideoFileReader;
  using Superclass = VideoSource<TOutputVideoStream>;
  using Pointer = SmartPointer<Self>;
  using VideoStreamType = TOutputVideoStream;
  using VideoStreamPointer = typename VideoStreamType::Pointer;

  using FrameType = typename VideoStreamType::FrameType;
  using PixelType = typename FrameType::PixelType;
  using RegionType = typename FrameType::RegionType;
  using SizeType = typename FrameType::SizeType;
  using IndexType = typename FrameType::IndexType;
  using PointType = typename FrameType::PointType;
  using SpacingType = typename FrameType::SpacingType;
  using DirectionType = typename FrameType::DirectionType;

  using TemporalOffsetType = typename VideoIOBase::TemporalOffsetType;
  using FrameOffsetType = typename VideoIOBase::FrameOffsetType;
  using TemporalRatioType = typename VideoIOBase::TemporalRatioType;

  static constexpr unsigned int FrameDimension = FrameType::ImageDimension;

  /** Pixel conversion type alias */
  using ConvertPixelTraits = DefaultConvertPixelTraits<PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoFileReader, VideoSource);


  /** Specify the file to read. This is forwarded to the IO instance. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Get/Set IFrameSafe. If true, the last IFrame will be reported as the last
   * frame for the largest possible temporal region */
  itkSetMacro(IFrameSafe, bool);
  itkGetMacro(IFrameSafe, bool);

  /** Set up the output information */
  void
  UpdateOutputInformation() override;

  /** Set the internal VideoIOBase pointer. This will generally be called by
   * the object that creates the RingBuffer (e.g. itk::VideoFileReader) */
  void
  SetVideoIO(VideoIOBase * videoIO);

  /** Get the current position as frame, ratio, or MSec */
  FrameOffsetType
  GetCurrentPositionFrame();

  TemporalRatioType
  GetCurrentPositionRatio();

  TemporalOffsetType
  GetCurrentPositionMSec();

  /** Get number of frames */
  FrameOffsetType
  GetNumberOfFrames();

  /** Get framerate */
  TemporalRatioType
  GetFramesPerSecond();

protected:
  VideoFileReader();
  ~VideoFileReader() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Override TemporalStreamingGenerateData to generate output a single frame.
   * We don't override ThreadedGenerateData because we read whole frames one at
   * a time. As such, we have to handle the allocation of the frames here. */
  void
  TemporalStreamingGenerateData() override;

  /** Convert buffer for output */
  void
  DoConvertBuffer(void * inputData, FrameOffsetType frameNumber);

  /** Set up the VideoIO using VideoIOFactory
   * Warning: this will overwrite any currently set VideoIO */
  void
  InitializeVideoIO();

private:
  /** The file to read */
  std::string m_FileName;

  /** VideoIOBase used to retrieve images. This may be changed if more
   * hierarchy is added to support general ImageSet sources. */
  VideoIOBase::Pointer m_VideoIO;

  /** Flag to store whether or not the pixel type needs to be converted. */
  bool m_PixelConversionNeeded;

  /** Flag to indicate whether to report the last frame as the last IFrame. On
   * by default. */
  bool m_IFrameSafe;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVideoFileReader.hxx"
#endif

#endif
