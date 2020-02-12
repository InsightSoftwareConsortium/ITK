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
#ifndef itkVXLVideoIO_h
#define itkVXLVideoIO_h

// Define support for VXLVideo
#ifndef ITK_VIDEO_USE_VXL
#  define ITK_VIDEO_USE_VXL
#endif

#ifdef _MSC_VER
#  pragma warning(disable : 4786)
#endif

#include "itkVideoIOBase.h"
#include "vidl/vidl_ffmpeg_istream.h"
#include "vidl/vidl_ffmpeg_ostream.h"
#include "vidl/vidl_convert.h"
#include "vul/vul_reg_exp.h"

namespace itk
{
/**
 *\class VXLVideoIO
 *
 * \brief VideoIO object for reading and writing videos using VXL
 *
 * \ingroup ITKVideoBridgeVXL
 *
 */
class VXLVideoIO : public VideoIOBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VXLVideoIO);

  /** Standard class type aliases. */
  using Self = VXLVideoIO;
  using Superclass = VideoIOBase;
  using Pointer = SmartPointer<Self>;

  using TemporalOffsetType = Superclass::TemporalOffsetType;
  using FrameOffsetType = Superclass::FrameOffsetType;
  using TemporalRatioType = Superclass::TemporalRatioType;
  using CameraIDType = Superclass::CameraIDType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VXLVideoIO, Superclass);

  /** Close the reader and writer and reset members */
  void
  FinishReadingOrWriting() override;

  /*-------- This part of the interface deals with reading data. ------ */

  /** Set to reading from file */
  void
  SetReadFromFile() override;

  /** Set to reading from a camera */
  void
  SetReadFromCamera() override;

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Return whether or not the VideoIO can read from a camera */
  bool
  CanReadCamera(CameraIDType cameraID) const override;

  /** Set the spacing and dimension information for the set filename. */
  void
  ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;


  /** Set the next frame that should be read. Return true if you operation
   * successful */
  bool
  SetNextFrameToRead(FrameOffsetType frameNumber) override;

  /** Accessor functions for video specific information */
  TemporalOffsetType
  GetPositionInMSec() const override;
  TemporalRatioType
  GetRatio() const override;
  FrameOffsetType
  GetFrameTotal() const override;
  TemporalRatioType
  GetFramesPerSecond() const override;
  FrameOffsetType
  GetCurrentFrame() const override;
  FrameOffsetType
  GetLastIFrame() const override;
  virtual FrameOffsetType
  GetIFrameInterval() const;

  /** Get/Set the camera index */
  virtual void
  SetCameraIndex(int idx);
  virtual int
  GetCameraIndex();


  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  bool
  CanWriteFile(const char *) override;

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  void
  WriteImageInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  void
  Write(const void * buffer) override;

  /** Set Writer Parameters */
  void
  SetWriterParameters(TemporalRatioType                  fps,
                      const std::vector<SizeValueType> & dim,
                      const char *                       fourCC,
                      unsigned int                       nChannels,
                      IOComponentType                    componentType) override;

protected:
  VXLVideoIO();
  ~VXLVideoIO();

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Update the local members from the internal capture */
  void
  UpdateReaderProperties();

  /** Reset member variables to empty state closed */
  void
  ResetMembers();

  /** Open the reader iff the writer is not open */
  void
  OpenReader();

  /** Open the writer iff the reader is not open */
  void
  OpenWriter();


  /** Translate a FourCC string into to a VXL encoder */
  vidl_ffmpeg_ostream_params::encoder_type
  FourCCtoEncoderType(const char * fourCC);

  /** Get the number of channels from the pixel format */
  unsigned int
  GetNChannelsFromPixelFormat(vidl_pixel_format fmt);

  /** Get the size of the vidl pixel format */
  unsigned int
  GetSizeFromPixelFormat(vidl_pixel_format fmt);

  /** Decide whether or not the pixel format is supported as is (mono, RGB, RGBA) */
  bool
  PixelFormatSupported(vidl_pixel_format fmt);

private:
  /** Member Variables */
  vidl_pixel_format                        m_PixelFormat;
  vidl_frame_sptr                          m_VIDLFrame;
  vidl_ffmpeg_istream *                    m_Reader;
  vidl_ffmpeg_ostream *                    m_Writer;
  vidl_ffmpeg_ostream_params::encoder_type m_Encoder;


  /** device index for reading from a camera (may move to base class) */
  int m_CameraIndex;
};
} // end namespace itk

#endif // itkVXLVideoIO_h
