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

#ifndef itkVideoFileWriter_h
#define itkVideoFileWriter_h

#include "itkTemporalProcessObject.h"
#include "itkVideoIOFactory.h"

namespace itk
{

/**
 *\class VideoFileWriter
 * \brief Writer that takes in a VideoStream and writes the frames to a file
 *
 * This class is a subclass of TemporalProcessObject which specifically takes a
 * single VideoStream as input and writes the frames out to a file in sequence.
 * A call to Update() will write the entire requested temporal region to the
 * file. If no temporal region is requested, the largest possible will be used.
 *
 * \ingroup ITKVideoIO
 */
template <typename TInputVideoStream>
class ITK_TEMPLATE_EXPORT VideoFileWriter : public TemporalProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VideoFileWriter);

  /** Standard class type aliases. */
  using Self = VideoFileWriter<TInputVideoStream>;
  using Superclass = TemporalProcessObject;
  using Pointer = SmartPointer<Self>;

  using IOBaseType = VideoIOBase;
  using IOBasePointer = typename VideoIOBase::Pointer;
  using SizeValueType = typename IOBaseType::SizeValueType;
  using TemporalRatioType = typename IOBaseType::TemporalRatioType;

  using VideoStreamType = TInputVideoStream;
  using VideoStreamPointer = typename VideoStreamType::Pointer;
  using FrameType = typename VideoStreamType::FrameType;
  using PixelType = typename FrameType::PixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoFileWriter, TemporalProcessObject);


  /** Specify the file to read. This is forwarded to the IO instance. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Specify the output FpS. */
  itkSetMacro(FramesPerSecond, TemporalRatioType);
  itkGetMacro(FramesPerSecond, TemporalRatioType);

  /** Specify the FourCC to use for video encoding. FourCC, or four character
   * code, is commonly used to denote the codec to be used to encode video by
   * many libraries. See http://en.wikipedia.org/wiki/FourCC for more
   * information. */
  itkSetStringMacro(FourCC);
  itkGetStringMacro(FourCC);

  /** Get/Set the OutputTemporalRegion */
  itkSetMacro(OutputTemporalRegion, TemporalRegion);
  itkGetMacro(OutputTemporalRegion, TemporalRegion);

  /** Set/Get the input video pointer */
  using Superclass::SetInput;
  void
  SetInput(const VideoStreamType * input);
  const VideoStreamType *
  GetInput();

  /** Manually set the VideoIO to use */
  void
  SetVideoIO(IOBasePointer videoIO);

  /** Write the requested temporal region to a file. If no OutputTemporalRegion
   * has been set, the largest possible temporal region of the input will be
   * used. */
  void
  Write();

  /** Finish writing the video and close the file */
  void
  FinishWriting();

  /** Aliased to the Write() method to be consistent with the rest of the
   * pipeline. */
  void
  Update() override;

  /** Write the entire video to a file, if possible. This is the same as
   * calling write or update without setting an output temporal region. */
  void
  UpdateLargestPossibleRegion() override;

protected:
  VideoFileWriter();
  ~VideoFileWriter() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Initialize output parameters. */
  bool
  InitializeOutputParameters();

  /** Set up the VideoIO using VideoIOFactory. Returns true if successful, false
   * otherwise.
   * Warning: this will overwrite any currently set VideoIO */
  bool
  InitializeVideoIO();

  /** Override TemporalStreamingGenerateData to do the actual writing. */
  void
  TemporalStreamingGenerateData() override;

private:
  /** The file to write. */
  std::string m_FileName;

  /** The VideoIO used for writing. */
  IOBasePointer m_VideoIO;

  /** TemporalRegion to write out. */
  TemporalRegion m_OutputTemporalRegion;

  /** Parameters for writing. */
  TemporalRatioType          m_FramesPerSecond{ 24 };
  std::string                m_FourCC;
  std::vector<SizeValueType> m_Dimensions;
  SizeValueType              m_NumberOfComponents{ 0 };
  IOComponentEnum            m_ComponentType;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVideoFileWriter.hxx"
#endif

#endif
