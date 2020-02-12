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
#ifndef itkVideoIOBase_h
#define itkVideoIOBase_h

#include "itkImageIOBase.h"
#include "ITKVideoIOExport.h"
#include "vnl/vnl_vector.h"

#include <string>

namespace itk
{
/**\class VideoIOBaseEnums
 * \brief This contains all enum classes used from VideoIOBase class.
 * \ingroup ITKVideoIO
 */
class VideoIOBaseEnums
{
public:
  /**
   *\class ReadFrom
   * \ingroup ITKVideoIO
   * Enum used to define weather to read from a file or a camera */
  enum class ReadFrom : uint8_t
  {
    ReadFromFile,
    ReadFromCamera
  };
};
// Define how to print enumeration
extern ITKVideoIO_EXPORT std::ostream &
                         operator<<(std::ostream & out, const VideoIOBaseEnums::ReadFrom value);
/**
 *\class VideoIOBase
 * \brief Abstract superclass defines video IO interface.
 *
 * VideoIOBase is a class that reads and/or writes video data
 * using a particular external technique or external library (OpenCV, vxl). The
 * VideoIOBase encapsulates both the reading and writing of data. The
 * VideoIOBase is used by the VideoFileReader class (to read data)
 * and the VideoFileWriter (to write data). Normally the user does not directly
 * manipulate this class directly.
 *
 * A Pluggable factory pattern is used. This allows different kinds of
 * readers to be registered (even at run time) without having to
 * modify the code in this class.
 *
 * \sa VideoFileWriter
 * \sa VideoFileReader
 *
 * \ingroup ITKVideoIO
 */
class ITKVideoIO_EXPORT VideoIOBase : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VideoIOBase);

  /** Standard class type aliases. */
  using Self = VideoIOBase;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;
  using SizeValueType = ::itk::SizeValueType;

  /** Frame offset type alias */
  using TemporalOffsetType = double;
  using FrameOffsetType = SizeValueType;
  using TemporalRatioType = double;

  /** Video-specific type alias */
  using CameraIDType = SizeValueType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoIOBase, Superclass);

  /** Close the reader and writer and reset members */
  virtual void
  FinishReadingOrWriting() = 0;

  /*-------- This part of the interface deals with reading data. ------ */

  using ReadFromEnum = VideoIOBaseEnums::ReadFrom;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr ReadFromEnum ReadFromFile = ReadFromEnum::ReadFromFile;
  static constexpr ReadFromEnum ReadFromCamera = ReadFromEnum::ReadFromCamera;
#endif

  /** Set to reading from file */
  virtual void
  SetReadFromFile() = 0;

  /** Set to reading from a camera */
  virtual void
  SetReadFromCamera() = 0;

  /** Get the current read type */
  ReadFromEnum
  GetReadFrom()
  {
    return this->m_ReadFrom;
  }

  /** Return whether or not the VideoIO can read from a camera. The cameraID
   * can be a camera number for OpenCV or a guid for VXL */
  virtual bool
  CanReadCamera(CameraIDType cameraID) const = 0;

  /** Set the next frame that should be read. Return true if you operation
   * successful */
  virtual bool
  SetNextFrameToRead(FrameOffsetType frameNumber) = 0;

  /** Virtual accessor functions to be implemented in each derived class */
  virtual TemporalOffsetType
  GetPositionInMSec() const = 0;
  virtual TemporalRatioType
  GetRatio() const = 0;
  virtual FrameOffsetType
  GetFrameTotal() const = 0;
  virtual TemporalRatioType
  GetFramesPerSecond() const = 0;
  virtual FrameOffsetType
  GetCurrentFrame() const = 0;
  virtual FrameOffsetType
  GetLastIFrame() const = 0;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Set Writer Parameters */
  virtual void
  SetWriterParameters(TemporalRatioType                  framesPerSecond,
                      const std::vector<SizeValueType> & dim,
                      const char *                       fourCC,
                      unsigned int                       nChannels,
                      IOComponentEnum                    componentType) = 0;

protected:
  VideoIOBase();
  ~VideoIOBase() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Member Variables */
  ReadFromEnum       m_ReadFrom{ ReadFromEnum::ReadFromFile };
  TemporalRatioType  m_FramesPerSecond{ 0.0 };
  FrameOffsetType    m_FrameTotal;
  FrameOffsetType    m_CurrentFrame;
  FrameOffsetType    m_IFrameInterval;
  FrameOffsetType    m_LastIFrame;
  TemporalRatioType  m_Ratio{ 0.0 };
  TemporalOffsetType m_PositionInMSec{ 0.0 };
  bool               m_WriterOpen{ false };
  bool               m_ReaderOpen{ false };
};
} // end namespace itk

#endif // itkVideoIOBase_h
