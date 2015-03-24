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
#ifndef itkOpenCVVideoIO_h
#define itkOpenCVVideoIO_h

#include "itkVideoIOBase.h"
#include "cv.h"
#include "highgui.h"


namespace itk
{
/** \class OpenCVVideoIO
 *
 * \brief VideoIO object for reading and writing videos using OpenCV
 *
 * \ingroup ITKVideoBridgeOpenCV
 */
class OpenCVVideoIO:public VideoIOBase
{
public:
  /** Standard class typedefs. */
  typedef OpenCVVideoIO                  Self;
  typedef VideoIOBase                    Superclass;
  typedef SmartPointer< Self >           Pointer;

  typedef Superclass::TemporalOffsetType TemporalOffsetType;
  typedef Superclass::FrameOffsetType    FrameOffsetType;
  typedef Superclass::TemporalRatioType  TemporalRatioType;
  typedef Superclass::CameraIDType       CameraIDType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(OpenCVVideoIO, Superclass);

  /** Close the reader and writer and reset members */
  virtual void FinishReadingOrWriting();

  /*-------- This part of the interface deals with reading data. ------ */

  /** Set to reading from file */
  virtual void SetReadFromFile();

  /** Set to reading from a camera */
  virtual void SetReadFromCamera();

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *);

  /** Return whether or not the VideoIO can read from a camera */
  virtual bool CanReadCamera( CameraIDType cameraID ) const;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer);

  /** Set the next frame that should be read. Return true if you operation
   * successful */
  virtual bool SetNextFrameToRead( FrameOffsetType frameNumber );

  /** Virtual accessor functions to be implemented in each derived class */
  virtual TemporalOffsetType GetPositionInMSec() const;
  virtual TemporalRatioType GetRatio() const;
  virtual FrameOffsetType GetFrameTotal() const;
  virtual TemporalRatioType GetFramesPerSecond() const;
  virtual FrameOffsetType GetCurrentFrame() const;
  virtual FrameOffsetType GetIFrameInterval() const;
  virtual FrameOffsetType GetLastIFrame() const;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Get/Set the camera index */
  virtual void SetCameraIndex(CameraIDType idx);
  virtual CameraIDType GetCameraIndex() const;

  /** Override Accessors to pass default values since OpenCV doesn't handle
   * this type of meta data. */
  virtual double GetSpacing(unsigned int itkNotUsed(i)) const
    { return 1.0; }
  virtual double GetOrigin(unsigned int itkNotUsed(i)) const
    { return 0.0; }
  virtual std::vector< double > GetDirection(unsigned int i) const
    { return this->GetDefaultDirection(i); }

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *);

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void *buffer);

  /** Set Writer Parameters */
  virtual void SetWriterParameters( TemporalRatioType fps,
                                    const std::vector<SizeValueType>& dim,
                                    const char* fourCC,
                                    unsigned int nChannels,
                                    IOComponentType componentType );

protected:
  OpenCVVideoIO();
  ~OpenCVVideoIO();

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Update the local members from the internal capture */
  void UpdateReaderProperties();

  /** Reset member variables to empty state closed */
  void ResetMembers();

  /** Open the reader iff the writer is not open */
  void OpenReader();

  /** Open the writer iff the reader is not open */
  void OpenWriter();

private:
  OpenCVVideoIO(const Self &);     //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  /** Member Variables */
  IplImage*           m_CVImage;
  IplImage*           m_TempImage;
  CvCapture*          m_Capture;
  CvVideoWriter*      m_Writer;
  int                 m_FourCC;

  /** device index for reading from a camera (may move to base class) */
  int                 m_CameraIndex;


};
} // end namespace itk

#endif // itkOpenCVVideoIO_h
