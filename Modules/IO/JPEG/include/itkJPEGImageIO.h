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
#ifndef itkJPEGImageIO_h
#define itkJPEGImageIO_h
#include "ITKIOJPEGExport.h"


#include "itkImageIOBase.h"

namespace itk
{
/** \class JPEGImageIO
 *
 * \brief ImageIO object for reading and writing JPEG images
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKIOJPEG
 */
class ITKIOJPEG_EXPORT JPEGImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef JPEGImageIO          Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(JPEGImageIO, ImageIOBase);

  /** Set/Get the level of quality for the output images. */
  itkSetMacro(Quality, int);
  itkGetConstMacro(Quality, int);

  /**  */
  itkSetMacro(Progressive, bool);
  itkGetConstMacro(Progressive, bool);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and diemention information for the set filename. */
  void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  void Read(void *buffer) ITK_OVERRIDE;

  /** Reads 3D data from multiple files assuming one slice per file. */
  virtual void ReadVolume(void *buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  void Write(const void *buffer) ITK_OVERRIDE;

protected:
  JPEGImageIO();
  ~JPEGImageIO() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void WriteSlice(std::string & fileName, const void *buffer);

  /** Determines the quality of compression for written files.
   *  default = 95 */
  int m_Quality;
  /** Default = true*/
  bool m_Progressive;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(JPEGImageIO);
};
} // end namespace itk

#endif // itkJPEGImageIO_h
