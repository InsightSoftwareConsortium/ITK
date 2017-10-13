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
#ifndef itkFileListVideoIO_h
#define itkFileListVideoIO_h

#include "itkVideoIOBase.h"
#include "ITKVideoIOExport.h"

namespace itk
{
/** \class FileListVideoIO
 *
 * \brief VideoIO object for reading and writing videos as a sequence of frame
 *  files.
 *
 * This VideoIO treats a sequential list of file names as the frames of a
 * video. The frames must be specified in a comma-separated list. Also, the
 * SplitFileNames(...) static method is made public in order to allow the
 * splitting functionality to be accessed publicly.
 *
 * \ingroup ITKVideoIO
 *
 */
class ITKVideoIO_EXPORT FileListVideoIO : public VideoIOBase
{
public:
  /** Standard class typedefs. */
  typedef FileListVideoIO             Self;
  typedef VideoIOBase                 Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileListVideoIO, VideoIOBase);

  /** Get the internal ImageIOBase object. */
  itkGetConstObjectMacro(ImageIO, ImageIOBase);

  /** Get the list of files to read. */
  itkGetConstMacro(FileNames, std::vector<std::string>);

  /** Override SetFileName to do parsing. */
  virtual void SetFileName(const std::string& fileList) ITK_OVERRIDE;
  virtual void SetFileName(const char* fileList) ITK_OVERRIDE;

  /** Close the reader and writer and reset members. */
  virtual void FinishReadingOrWriting() ITK_OVERRIDE;

  /** Split up the input file names using comma (',') as the separator character.
   * This method is made public so that places where FileListVideoIO is used
   * can access the individual file names. This is mostly an issue for testing. */
  static std::vector<std::string> SplitFileNames(const std::string& fileList);

  /** Set to reading from file. */
  virtual void SetReadFromFile() ITK_OVERRIDE;

  /** Set to reading from a camera. */
  virtual void SetReadFromCamera() ITK_OVERRIDE;

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Return whether or not the VideoIO can read from a camera. */
  virtual bool CanReadCamera( CameraIDType cameraID )const ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /** Set the next frame that should be read. Return true if you operation
   * successful. */
  virtual bool SetNextFrameToRead(FrameOffsetType frameNumber) ITK_OVERRIDE;

  /** Accessor functions for video specific information. */
  virtual TemporalOffsetType GetPositionInMSec() const ITK_OVERRIDE
    {
    return this->m_PositionInMSec;
    }
  virtual TemporalOffsetType GetRatio() const ITK_OVERRIDE
    {
    return this->m_Ratio;
    }
  virtual FrameOffsetType GetFrameTotal() const ITK_OVERRIDE
    {
    return this->m_FrameTotal;
    }
  virtual TemporalRatioType GetFramesPerSecond() const ITK_OVERRIDE
    {
    return this->m_FramesPerSecond;
    }
  virtual FrameOffsetType GetCurrentFrame() const ITK_OVERRIDE
    {
    return this->m_CurrentFrame;
    }
  itkGetConstMacro(IFrameInterval,FrameOffsetType);
  virtual FrameOffsetType GetLastIFrame() const ITK_OVERRIDE
    {
    return this->m_LastIFrame;
    }

  /** Override accessors to pass through to internal image reader. */
  virtual double GetSpacing(unsigned int i) const ITK_OVERRIDE;

  virtual double GetOrigin(unsigned int i) const ITK_OVERRIDE;

  virtual std::vector< double > GetDirection(unsigned int i) const ITK_OVERRIDE;

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

  /** Set Writer parameters. */
  virtual void SetWriterParameters( TemporalRatioType framesPerSecond,
                                    const std::vector<SizeValueType>& dim,
                                    const char* fourCC,
                                    unsigned int nChannels,
                                    IOComponentType componentType ) ITK_OVERRIDE;

protected:
  FileListVideoIO();
  ~FileListVideoIO() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Reset member variables to empty state closed. */
  void ResetMembers();

  /** Open the reader if the reader and writer are not open. */
  void OpenReader();

  /** Open the writer if the reader and reader are not open. */
  void OpenWriter();

  /** Verify that all file names in the have the same extension. */
  bool VerifyExtensions( const std::vector<std::string>& fileList ) const;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FileListVideoIO);

  ImageIOBase::Pointer m_ImageIO;

  std::vector<std::string> m_FileNames;

};
} // end namespace itk

#endif // itkFileListVideoIO_h
