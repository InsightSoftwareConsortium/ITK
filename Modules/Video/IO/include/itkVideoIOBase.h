/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "itkExceptionObject.h"
#include "ITKVideoIOExport.h"
#include "vnl/vnl_vector.h"

#include <string>

namespace itk
{
/** \class VideoIOBase
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
  /** Standard class typedefs. */
  typedef VideoIOBase          Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef ::itk::SizeValueType SizeValueType;

  /** Frame offset typedefs */
  typedef double               TemporalOffsetType;
  typedef SizeValueType        FrameOffsetType;
  typedef double               TemporalRatioType;

  /** Video-specific typedefs */
  typedef SizeValueType CameraIDType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoIOBase, Superclass);

  /** Close the reader and writer and reset members */
  virtual void FinishReadingOrWriting() = 0;

  /*-------- This part of the interface deals with reading data. ------ */

  /** Enum used to define weather to read from a file or a camera */
  typedef enum {ReadFromFile, ReadFromCamera} ReadType;

  /** Set to reading from file */
  virtual void SetReadFromFile() = 0;

  /** Set to reading from a camera */
  virtual void SetReadFromCamera() = 0;

  /** Get the current read type */
  ReadType GetReadType() {
    return this->m_ReadType;
  }

  /** Return whether or not the VideoIO can read from a camera. The cameraID
   * can be a camera number for OpenCV or a guid for VXL */
  virtual bool CanReadCamera( CameraIDType cameraID ) const = 0;

  /** Set the next frame that should be read. Return true if you operation
   * successful */
  virtual bool SetNextFrameToRead( FrameOffsetType frameNumber ) = 0;

  /** Virtual accessor functions to be implemented in each derived class */
  virtual TemporalOffsetType GetPositionInMSec() const = 0;
  virtual TemporalRatioType GetRatio() const = 0;
  virtual FrameOffsetType GetFrameTotal() const = 0;
  virtual TemporalRatioType GetFramesPerSecond() const = 0;
  virtual FrameOffsetType GetCurrentFrame() const = 0;
  virtual FrameOffsetType GetLastIFrame() const = 0;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Set Writer Parameters */
  virtual void SetWriterParameters( TemporalRatioType framesPerSecond,
                                    const std::vector<SizeValueType>& dim,
                                    const char* fourCC,
                                    unsigned int nChannels,
                                    IOComponentType componentType) = 0;

protected:

  VideoIOBase();
  virtual ~VideoIOBase() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Member Variables */
  ReadType           m_ReadType;
  TemporalRatioType  m_FramesPerSecond;
  FrameOffsetType    m_FrameTotal;
  FrameOffsetType    m_CurrentFrame;
  FrameOffsetType    m_IFrameInterval;
  FrameOffsetType    m_LastIFrame;
  TemporalRatioType  m_Ratio;
  TemporalOffsetType m_PositionInMSec;
  bool               m_WriterOpen;
  bool               m_ReaderOpen;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VideoIOBase);

};

} // end namespace itk

#endif // itkVideoIOBase_h
