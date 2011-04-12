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

#include "itkImageSource.h"
#include "itkVideoIOFactory.h"
#include "itkDefaultConvertPixelTraits.h"

namespace itk
{

/** \class VideoFileReader
 * \brief Reader that creates a VideoImageSet with an apropriate VideoIOBase
 *
 * This class is responsible for instantiating a valid VideoIO for the given
 * input file and returning a VideoImageSet that will use that VideoIO
 * internally. Unlike VideoFileReader, this does not actually do the reading.
 * that part is handled internally by RingBufferImageSet (parent of
 * VideoImageSet).
 *
 * The relationship between VideoFileReader, VideoImageSet, and Image creates
 * a break in the ITK pipeline system. Typacally, the pipeline consists of
 * ProcessObjects and each ProcessObject passes one or more DataObjects to the
 * next ProcessObject down the line. The problem arises because VideoImageSet
 * needs to function both as a ProcessObject (outputs images for frames) and as
 * a DataObject (output by VideoImageSet). The solution for this is to manually
 * implement the relationship between VideoFileReader and VideoImageSet and to
 * let the rest of the pipeline treat VideoImageSet as an ImageSource. Because
 * of this issue, VideoFileReader implements its own Update() and GetOutput()
 * methods rather than using those provided by ProcessObject
 */
template< class TInputImage >
class ITK_EXPORT VideoFileReader:public ImageSource< TInputImage >
{
public:

  /**-TYPEDEFS---------------------------------------------------------------*/
  typedef VideoFileReader               Self;
  typedef ProcessObject                 Superclass;
  typedef SmartPointer<Self>            Pointer;
  typedef TInputImage                   ImageType;
  typedef typename ImageType::PixelType PixelType;

  /** Pixel conversion typedefs */
  typedef DefaultConvertPixelTraits<PixelType> ConvertPixelTraits;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoFileReader, ImageSource);

  /**-PUBLIC METHODS---------------------------------------------------------*/

  /** Specify the file to read. This is forwarded to the IO instance. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Set up the output information */
  virtual void GenerateOutputInformation();

  /** Override UpdateOutputData to create the output VideoImageSet */
  virtual void UpdateOutputData( DataObject* );

  /** Implement GetOutput to mirror standard pipeline system */
  typename ImageType::Pointer GetOutput();

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

  /** Convert buffer for output */
  void DoConvertBuffer(void* inputData);

  /** Set up the VideoIO using VideoIOFactory
   * Warning: this will overwrite any currently set VideoIO */
  void InitializeVideoIO();

  /**-PROTECTED MEMBERS------------------------------------------------------*/

  /** The file to read */
  std::string m_FileName;

  /** The output VideoImageSet */
  typename ImageType::Pointer m_Output;

  /** VideoIOBase used to retrieve images. This may be changed if more
   * hierarchy is added to support general ImageSet sources */
  VideoIOBase::Pointer m_VideoIO;

  /** Flag to store whether or not the pixel type needs to be converted */
  bool m_PixelConversionNeeded;

  /** Keep track of whether or not the output has been allocated */
  bool m_OutputAllocated;

private:
  VideoFileReader(const Self &); // purposely not implemented
  void operator=(const Self &);  // purposely not implemented

};


} // end namespace itk

#if ITK_TEMPLATE_TXX
#include "itkVideoFileReader.txx"
#endif

#endif 
