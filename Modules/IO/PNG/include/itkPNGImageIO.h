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
#ifndef itkPNGImageIO_h
#define itkPNGImageIO_h
#include "ITKIOPNGExport.h"


#include "itkImageIOBase.h"

namespace itk
{
/** \class PNGImageIO
 *
 * \brief ImageIO object for reading and writing PNG images
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKIOPNG
 */
class ITKIOPNG_EXPORT PNGImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef PNGImageIO           Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  typedef RGBPixel< unsigned char >   RGBPixelType;
  typedef std::vector< RGBPixelType > PaletteType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PNGImageIO, ImageIOBase);

  /** Set/Get the level of compression for the output images.
   *  0-9; 0 = none, 9 = maximum. */
  itkSetMacro(CompressionLevel, int);
  itkGetConstMacro(CompressionLevel, int);

  /** Get a const ref to the palette of the image. In the case of non palette
    * image or ExpandRGBPalette set to true, a vector of size
    * 0 is returned */
  itkGetConstReferenceMacro(ColorPalette, PaletteType);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /** Reads 3D data from multiple files assuming one slice per file. */
  virtual void ReadVolume(void *buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

protected:
  PNGImageIO();
  ~PNGImageIO() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void WriteSlice(const std::string & fileName, const void *buffer);

  /** Determines the level of compression for written files.
   *  Range 0-9; 0 = none, 9 = maximum , default = 4 */
  int m_CompressionLevel;

  PaletteType m_ColorPalette;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PNGImageIO);
};
} // end namespace itk

#endif // itkPNGImageIO_h
