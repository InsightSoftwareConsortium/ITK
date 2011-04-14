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
 * \brief Writer that takes in a set of frames and writes out a video
 *
 * This class is responsible for holding onto a VideoIO and using it to write
 * frames out to a file one at a time. Despite the fact that this class doesn't
 * actually use a VideoImageSet as input, it is templated over VideoImageSet to
 * enforce that it is only used for video writing.
 */
template< class TInputImage >
class ITK_EXPORT VideoFileWriter:public ProcessObject
{
public:

  /**-TYPEDEFS---------------------------------------------------------------*/
  typedef VideoFileWriter    Self;
  typedef ProcessObject      Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef unsigned int       SizeValueType;
  typedef TInputImage        ImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoFileWriter, ProcessObject);

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

  /** Set the input image pointer */
  void SetInput( ImageType* input );

  /** Manually set the VideoIO to use */
  void SetVideoIO( VideoIOBase* videoIO );

  /** Write a single frame using the VideoIO (create the VideoIO if necessary) */
  void Write();

  /** Finish writing the video and close the file */
  void FinishWriting();

  /** Implement Update to mirror standard pipeline system */
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


  /**-PROTECTED MEMBERS------------------------------------------------------*/

  /** The file to read */
  std::string m_FileName;

  /** Pointer for the input image */
  typename ImageType::Pointer m_InputImage;

  /** The VideoIO used for writing */
  VideoIOBase::Pointer m_VideoIO;

  /** Parameters for writing */
  double m_FpS;
  std::string m_FourCC;
  std::vector<SizeValueType> m_Dimensions;
  unsigned int m_NumberOfComponents;
  ImageIOBase::IOComponentType m_ComponentType;

  /** Flag to keep track of whether or not this is the first call to write */
  bool m_FirstWrite;
  
  


private:
  VideoFileWriter(const Self &); // purposely not implemented
  void operator=(const Self &);  // purposely not implemented

};


} // end namespace itk

#if ITK_TEMPLATE_TXX
#include "itkVideoFileWriter.txx"
#endif

#endif 
