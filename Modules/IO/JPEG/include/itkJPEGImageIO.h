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
#ifndef itkJPEGImageIO_h
#define itkJPEGImageIO_h
#include "ITKIOJPEGExport.h"


#include "itkImageIOBase.h"

namespace itk
{
/**
 *\class JPEGImageIO
 *
 * \brief ImageIO object for reading and writing JPEG images
 *
 * Compression is supported with only the default compressor. The
 * compression level option is supported in the range 0-100.
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKIOJPEG
 */
class ITKIOJPEG_EXPORT JPEGImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(JPEGImageIO);

  /** Standard class type aliases. */
  using Self = JPEGImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(JPEGImageIO, ImageIOBase);

  /** Set/Get the level of quality for the output images. */
  virtual void
  SetQuality(int _JPEGQuality)
  {
    this->SetCompressionLevel(_JPEGQuality);
  }
  virtual int
  GetQuality() const
  {
    return this->GetCompressionLevel();
  }

  /**  */
  itkSetMacro(Progressive, bool);
  itkGetConstMacro(Progressive, bool);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;

  /** Reads 3D data from multiple files assuming one slice per file. */
  virtual void
  ReadVolume(void * buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
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

protected:
  JPEGImageIO();
  ~JPEGImageIO() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  WriteSlice(std::string & fileName, const void * buffer);

  /** Default = true*/
  bool m_Progressive;
};
} // end namespace itk

#endif // itkJPEGImageIO_h
