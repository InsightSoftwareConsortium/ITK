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
#ifndef itkFileListVideoIO_h
#define itkFileListVideoIO_h

#include "itkVideoIOBase.h"
#include "ITKVideoIOExport.h"

namespace itk
{
/**
 *\class FileListVideoIO
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
  ITK_DISALLOW_COPY_AND_MOVE(FileListVideoIO);

  /** Standard class type aliases. */
  using Self = FileListVideoIO;
  using Superclass = VideoIOBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileListVideoIO, VideoIOBase);

  /** Get the internal ImageIOBase object. */
  itkGetConstObjectMacro(ImageIO, ImageIOBase);

  /** Get the list of files to read. */
  itkGetConstMacro(FileNames, std::vector<std::string>);

  /** Override SetFileName to do parsing. */
  void
  SetFileName(const std::string & fileList) override;
  void
  SetFileName(const char * fileList) override;

  /** Close the reader and writer and reset members. */
  void
  FinishReadingOrWriting() override;

  /** Split up the input file names using comma (',') as the separator character.
   * This method is made public so that places where FileListVideoIO is used
   * can access the individual file names. This is mostly an issue for testing. */
  static std::vector<std::string>
  SplitFileNames(const std::string & fileList);

  /** Set to reading from file. */
  void
  SetReadFromFile() override;

  /** Set to reading from a camera. */
  void
  SetReadFromCamera() override;

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Return whether or not the VideoIO can read from a camera. */
  bool
  CanReadCamera(CameraIDType cameraID) const override;

  /** Set the spacing and dimension information for the set filename. */
  void
  ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;

  /** Set the next frame that should be read. Return true if you operation
   * successful. */
  bool
  SetNextFrameToRead(FrameOffsetType frameNumber) override;

  /** Accessor functions for video specific information. */
  TemporalOffsetType
  GetPositionInMSec() const override
  {
    return this->m_PositionInMSec;
  }
  TemporalOffsetType
  GetRatio() const override
  {
    return this->m_Ratio;
  }
  FrameOffsetType
  GetFrameTotal() const override
  {
    return this->m_FrameTotal;
  }
  TemporalRatioType
  GetFramesPerSecond() const override
  {
    return this->m_FramesPerSecond;
  }
  FrameOffsetType
  GetCurrentFrame() const override
  {
    return this->m_CurrentFrame;
  }
  itkGetConstMacro(IFrameInterval, FrameOffsetType);
  FrameOffsetType
  GetLastIFrame() const override
  {
    return this->m_LastIFrame;
  }

  /** Override accessors to pass through to internal image reader. */
  double
  GetSpacing(unsigned int i) const override;

  double
  GetOrigin(unsigned int i) const override;

  std::vector<double>
  GetDirection(unsigned int i) const override;

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  bool
  CanWriteFile(const char *) override;

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  void
  WriteImageInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  void
  Write(const void * buffer) override;

  /** Set Writer parameters. */
  void
  SetWriterParameters(TemporalRatioType                  framesPerSecond,
                      const std::vector<SizeValueType> & dim,
                      const char *                       fourCC,
                      unsigned int                       nChannels,
                      IOComponentEnum                    componentType) override;

protected:
  FileListVideoIO();
  ~FileListVideoIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Reset member variables to empty state closed. */
  void
  ResetMembers();

  /** Open the reader if the reader and writer are not open. */
  void
  OpenReader();

  /** Open the writer if the reader and reader are not open. */
  void
  OpenWriter();

  /** Verify that all file names in the have the same extension. */
  bool
  VerifyExtensions(const std::vector<std::string> & fileList) const;

private:
  ImageIOBase::Pointer m_ImageIO;

  std::vector<std::string> m_FileNames;
};
} // end namespace itk

#endif // itkFileListVideoIO_h
