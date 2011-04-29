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

#ifndef __itkVideoFileReader_h
#define __itkVideoFileReader_h

#include "itkVideoSource.h"
#include "itkVideoIOFactory.h"
#include "itkDefaultConvertPixelTraits.h"

namespace itk
{

/** \class VideoFileReader
 * \brief Reader that creates a VideoStream
 *
 * This class is responsible for reading video information from files. It is a
 * subclass of VideoSource, giving it functionality to connect to other
 * TemporalProcessObject classes (specifically, VideoToVideoFilter classes). It
 * uses the temporal streaming implementation provided by TemporalProcessObject
 * to load a single frame at a time into the frame buffer of the output
 * VideoSource.
 */
template< class TOutputVideoStream >
class ITK_EXPORT VideoFileReader:public VideoSource< TOutputVideoStream >
{
public:

  /**-TYPEDEFS---------------------------------------------------------------*/
  typedef VideoFileReader                  Self;
  typedef VideoSource< TOutputVideoStream > Superclass;
  typedef SmartPointer<Self>               Pointer;
  typedef TOutputVideoStream                VideoStreamType;

  typedef typename VideoStreamType::FrameType FrameType;
  typedef typename FrameType::PixelType       PixelType;

  /** Pixel conversion typedefs */
  typedef DefaultConvertPixelTraits<PixelType> ConvertPixelTraits;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoFileReader, VideoSource);

  /**-PUBLIC METHODS---------------------------------------------------------*/

  /** Specify the file to read. This is forwarded to the IO instance. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Set up the output information */
  virtual void UpdateOutputInformation();

  /** Set the internal VideoIOBase pointer. This will generally be called by
   * the object that creates the RingBuffer (e.g. itk::VideoFileReader) */
  void SetVideoIO(VideoIOBase* videoIO);


  /** Get the current position as frame, ratio, or MSec */
  unsigned long GetCurrentPositionFrame();
  double GetCurrentPositionRatio();
  double GetCurrentPositionMSec();

  /** Get number of frames */
  unsigned long GetNumberOfFrames();

  /** Get framerate */
  double GetFpS();

protected:

  /**-PROTECTED METHODS------------------------------------------------------*/
  VideoFileReader();
  virtual ~VideoFileReader();
  void PrintSelf(std::ostream &os, Indent indent) const;

  /** Override TemporalStreamingGenerateData to generate output a single frame.
   * We don't override ThreadedGenerateData because we read whole frames one at
   * a time. As such, we have to handle the allocation of the frames here. */
  virtual void TemporalStreamingGenerateData();

  /** Convert buffer for output */
  void DoConvertBuffer(void* inputData, unsigned long frameNumber);

  /** Set up the VideoIO using VideoIOFactory
   * Warning: this will overwrite any currently set VideoIO */
  void InitializeVideoIO();

  /**-PROTECTED MEMBERS------------------------------------------------------*/

  /** The file to read */
  std::string m_FileName;

  /** VideoIOBase used to retrieve images. This may be changed if more
   * hierarchy is added to support general ImageSet sources */
  VideoIOBase::Pointer m_VideoIO;

  /** Flag to store whether or not the pixel type needs to be converted */
  bool m_PixelConversionNeeded;

private:
  VideoFileReader(const Self &); // purposely not implemented
  void operator=(const Self &);  // purposely not implemented

};


} // end namespace itk

#if ITK_TEMPLATE_TXX
#include "itkVideoFileReader.txx"
#endif

#endif 
