/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

#include "opencv2/videoio.hpp"

#include "ITKVideoBridgeOpenCVExport.h"


namespace itk
{
/**
 * \class OpenCVVideoIO
 *
 * \brief VideoIO object for reading and writing videos using OpenCV
 *
 * \ingroup ITKVideoBridgeOpenCV
 */
class ITKVideoBridgeOpenCV_EXPORT OpenCVVideoIO : public VideoIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(OpenCVVideoIO);

  /** Standard class type aliases. */
  using Self = OpenCVVideoIO;
  using Superclass = VideoIOBase;
  using Pointer = SmartPointer<Self>;

  using TemporalOffsetType = Superclass::TemporalOffsetType;
  using FrameOffsetType = Superclass::FrameOffsetType;
  using TemporalRatioType = Superclass::TemporalRatioType;
  using CameraIDType = Superclass::CameraIDType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(OpenCVVideoIO);

  /** Close the reader and writer and reset members */
  void
  FinishReadingOrWriting() override;

  //
  // Data reading-related methods
  //

  /** Set to reading from file */
  void
  SetReadFromFile() override;

  /** Set to reading from a camera */
  void
  SetReadFromCamera() override;

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Return whether or not the VideoIO can read from a camera */
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

  /** Virtual accessor functions to be implemented in each derived class. */
  TemporalOffsetType
  GetPositionInMSec() const override;
  TemporalRatioType
  GetRatio() const override;
  FrameOffsetType
  GetFrameTotal() const override;
  TemporalRatioType
  GetFramesPerSecond() const override;
  FrameOffsetType
  GetCurrentFrame() const override;
  virtual FrameOffsetType
  GetIFrameInterval() const;
  FrameOffsetType
  GetLastIFrame() const override;

  //
  // Data writing-related methods
  //

  /** Get/Set the device index for reading from a camera. */
  virtual void
  SetCameraIndex(CameraIDType idx);
  virtual CameraIDType
  GetCameraIndex() const;

  /** Override Accessors to pass default values since OpenCV doesn't handle
   * this type of meta data. */
  double
  GetSpacing(unsigned int itkNotUsed(i)) const override
  {
    return 1.0;
  }
  double
  GetOrigin(unsigned int itkNotUsed(i)) const override
  {
    return 0.0;
  }
  std::vector<double>
  GetDirection(unsigned int i) const override
  {
    return this->GetDefaultDirection(i);
  }

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
  SetWriterParameters(TemporalRatioType                  fps,
                      const std::vector<SizeValueType> & dim,
                      const char *                       fourCC,
                      unsigned int                       nChannels,
                      IOComponentEnum                    componentType) override;

protected:
  OpenCVVideoIO();
  ~OpenCVVideoIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Update the local members from the internal capture. */
  void
  UpdateReaderProperties();

  /** Reset member variables to empty state closed. */
  void
  ResetMembers();

  /** Open the reader if the writer is not open. */
  void
  OpenReader();

  /** Open the writer if the reader is not open. */
  void
  OpenWriter();

private:
  cv::Mat          m_CVImage{};
  cv::VideoCapture m_Capture{};
  cv::VideoWriter  m_Writer{};
  int              m_FourCC{};

  int m_CameraIndex{};
};
} // end namespace itk

#endif // itkOpenCVVideoIO_h
