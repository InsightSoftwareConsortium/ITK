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
#ifndef __itkVideoIOBase_h
#define __itkVideoIOBase_h

#include "itkImageIOBase.h"
#include "itkExceptionObject.h"
#include "vnl/vnl_vector.h"

#include <string>

namespace itk
{
/** \class VideoIOBase
 * \brief Abstract superclass defines video IO interface.
 *
 * VideoIOBase is a class that reads and/or writes video data
 * of a particular using a particular external library (eg OpenCV or vxl). The
 * VideoIOBase encapsulates both the reading and writing of data. The
 * VideoIOBase is used by the VideoFileReader class (to read data)
 * and the VideoFileWriter (to write data) into a single file. Normally the
 * user does not directly
 * manipulate this class other than to instantiate it, set the FileName,
 * and assign it to a VideoFileReader/VideoFileWriter.
 *
 * A Pluggable factory pattern is used this allows different kinds of
 * readers to be registered (even at run time) without having to
 * modify the code in this class.
 *
 * \sa VideoFileWriter
 * \sa VideoFileReader
 *
 */
class ITK_EXPORT VideoIOBase:public ImageIOBase
{

public:
  /** Standard class typedefs. */
  typedef VideoIOBase          Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef unsigned int         SizeValueType;

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
  int GetReadType() { return this->m_ReadType; };

  /** Return whether or not the VideoIO can read from a camera. The cameraID
   * can be a camera number for OpenCV or a guid for VXL */
  virtual bool CanReadCamera( unsigned long cameraID ) = 0;

  /** Set the next frame that should be read. Return true if you operation
   * succesful */
  virtual bool SetNextFrameToRead(unsigned long frameNumber) = 0;

  /** Virtual accessor functions to be implemented in each derived class */
  virtual double GetPositionInMSec() = 0;
  virtual double GetRatio() = 0;
  virtual unsigned long GetFrameTotal() = 0;
  virtual double GetFpS() = 0;
  virtual unsigned long GetCurrentFrame() = 0;


  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Set Writer Parameters */
  virtual void SetWriterParameters(double fps, std::vector<SizeValueType> dim, const char* fourCC,
                                   unsigned int nChannels, IOComponentType componentType) = 0;


protected:
  VideoIOBase(){};
  ~VideoIOBase(){};

  void PrintSelf(std::ostream & os, Indent indent) const;
  
  /** Member Variables */
  int             m_ReadType;
  double          m_FpS;
  unsigned long   m_FrameTotal;
  unsigned long   m_CurrentFrame;
  unsigned int    m_IFrameInterval;
  unsigned long   m_LastIFrame;
  double          m_Ratio;
  double          m_PositionInMSec;
  bool            m_WriterOpen;
  bool            m_ReaderOpen;

private:
  VideoIOBase(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented



};


} // end namespace itk

#endif // __itkVideoIOBase_h
