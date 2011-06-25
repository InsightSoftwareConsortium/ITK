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

#ifndef __itkVideoFileWriter_h
#define __itkVideoFileWriter_h

#include "itkProcessObject.h"
#include "itkVideoIOFactory.h"

namespace itk
{

/** \class VideoFileWriter
 * \brief Writer that takes in a VideoStream and writes the frames to a file
 *
 * This class is a subclass of TemporalProcessObject which specifically takes a
 * single VideoStream as input and writes the frames out to a file in sequence.
 * A call to Update() will write the entire requested temporal region to the
 * file. If no temporal region is requested, the largest possible will be used.
 *
 * \ingroup Video-IO-Base
 */
template< class TInputVideoStream >
class ITK_EXPORT VideoFileWriter:public TemporalProcessObject
{
public:

  /**-TYPEDEFS---------------------------------------------------------------*/
  typedef VideoFileWriter< TInputVideoStream> Self;
  typedef TemporalProcessObject               Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef unsigned int                        SizeValueType;
  typedef TInputVideoStream                   VideoStreamType;
  typedef typename VideoStreamType::FrameType FrameType;
  typedef typename FrameType::PixelType       PixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoFileWriter, TemporalProcessObject);

  /**-PUBLIC METHODS---------------------------------------------------------*/

  /** Specify the file to read. This is forwarded to the IO instance. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Specify the output FpS */
  itkSetMacro(FpS, double);
  itkGetMacro(FpS, double);

  /** Specify the FourCC to use for video encoding */
  itkSetStringMacro(FourCC);
  itkGetStringMacro(FourCC);

  /** Get/Set the OutputTemporalRegion */
  itkSetMacro(OutputTemporalRegion, TemporalRegion);
  itkGetMacro(OutputTemporalRegion, TemporalRegion);

  /** Set/Get the input video pointer */
  void SetInput( const VideoStreamType* input );
  const VideoStreamType* GetInput();

  /** Manually set the VideoIO to use */
  void SetVideoIO( VideoIOBase* videoIO );

  /** Write the requested temporal region to a file. If no OutputTemporalRegion
   * has been set, the largest possible temporal region of the input will be
   * used. */
  void Write();

  /** Finish writing the video and close the file */
  void FinishWriting();

  /** Aliased to the Write() method to be consistent with the rest of the
   * pipeline. */
  virtual void Update()
  {
    this->Write();
  }

protected:

  /**-PROTECTED METHODS------------------------------------------------------*/

  VideoFileWriter();
  virtual ~VideoFileWriter();
  void PrintSelf(std::ostream &os, Indent indent) const;

  /** Initialize output parameters */
  bool InitializeOutputParameters();

  /** Set up the VideoIO using VideoIOFactory. Returns true if sucessful, false
   * otherwise.
   * Warning: this will overwrite any currently set VideoIO */
  bool InitializeVideoIO();

  /** Override TemporalStreamingGenerateData to do the actual writing. */
  virtual void TemporalStreamingGenerateData();

  /**-PROTECTED MEMBERS------------------------------------------------------*/

  /** The file to read */
  std::string m_FileName;

  /** The VideoIO used for writing */
  VideoIOBase::Pointer m_VideoIO;

  /** TemporalRegion to write out */
  TemporalRegion m_OutputTemporalRegion;

  /** Parameters for writing */
  double m_FpS;
  std::string m_FourCC;
  std::vector<SizeValueType> m_Dimensions;
  unsigned int m_NumberOfComponents;
  ImageIOBase::IOComponentType m_ComponentType;


private:
  VideoFileWriter(const Self &); // purposely not implemented
  void operator=(const Self &);  // purposely not implemented

};


} // end namespace itk

#if ITK_TEMPLATE_TXX
#include "itkVideoFileWriter.txx"
#endif

#endif 
