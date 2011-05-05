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
#ifndef __itkFileListVideoIO_h
#define __itkFileListVideoIO_h

// Define support for FileListVideo
#ifndef ITK_VIDEO_USE_FILE_LIST
#define ITK_VIDEO_USE_FILE_LIST
#endif

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkVideoIOBase.h"

namespace itk
{
/** \class FileListVideoIO
 *
 * \brief VideoIO object for reading and writing videos using FileList
 *
 * \ingroup IOFilters
 *
 */
class ITK_EXPORT FileListVideoIO:public VideoIOBase
{
public:
  /** Standard class typedefs. */
  typedef FileListVideoIO        Self;
  typedef VideoIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileListVideoIO, Superclass);

  /** Close the reader and writer and reset members */
  virtual void FinishReadingOrWriting();

  /** Split up the input file names -- This is public so that places where
   * FileListVideoIO is used can access the individual file names. This is
   * mostly an issue for testing. */
  static std::vector<std::string> SplitFileNames(const char* fileList);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Set to reading from file */
  virtual void SetReadFromFile();

  /** Set to reading from a camera */
  virtual void SetReadFromCamera();

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *);

  /** Return whether or not the VideoIO can read from a camera */
  virtual bool CanReadCamera( unsigned long cameraID );

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer);


  /** Set the next frame that should be read. Return true if you operation
   * succesful */
  virtual bool SetNextFrameToRead(unsigned long frameNumber);

  /** Accessor functions for video specific information */
  virtual double GetPositionInMSec();
  virtual double GetRatio();
  virtual unsigned long GetFrameTotal();
  virtual double GetFpS();
  virtual unsigned long GetCurrentFrame();
  virtual unsigned int GetIFrameInterval();
  virtual unsigned long GetLastIFrame();

  /** Override SetFileName to do parsing */
  virtual void SetFileName(const char* fileList);



  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *);

  /** Writes the spacing and dimentions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void *buffer);

  /** Set Writer Parameters */
  virtual void SetWriterParameters(double fps, std::vector<SizeValueType> dim, const char* fourCC,
                                   unsigned int nChannels, IOComponentType componentType);



protected:
  FileListVideoIO();
  ~FileListVideoIO();

  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Reset member variables to empty state closed */
  void ResetMembers();

  /** Open the reader iff the writer is not open */
  void OpenReader();

  /** Open the writer iff the reader is not open */
  void OpenWriter();


private:
  FileListVideoIO(const Self &);     //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  /** Member Variables */
  ImageIOBase::Pointer m_ImageIO;

  /** List of files to read */
  std::vector<std::string> m_FileNames;
  

};
} // end namespace itk

#endif // __itkFileListVideoIO_h
