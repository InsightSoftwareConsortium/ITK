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
#ifndef itkBMPImageIO_h
#define itkBMPImageIO_h
#include "ITKIOBMPExport.h"


#include <fstream>
#include "itkImageIOBase.h"
#include <cstdio>
#include "itkRGBPixel.h"

namespace itk
{
/** \class BMPImageIO
 *
 *  \brief Read BMPImage file format.
 *
 *  \ingroup IOFilters
 *
 * \ingroup ITKIOBMP
 */
class ITKIOBMP_EXPORT BMPImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef BMPImageIO                Self;
  typedef ImageIOBase               Superclass;
  typedef SmartPointer< Self >      Pointer;

  typedef RGBPixel< unsigned char >   RGBPixelType; //Palette is only unsigned char in BMP files
  typedef std::vector< RGBPixelType > PaletteType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BMPImageIO, Superclass);

  /** Getter for the FileLowerLeft attribute. */
  itkGetConstMacro(FileLowerLeft, bool);

  /** Getter for the BMPCompression attribute. */
  itkGetConstMacro(BMPCompression, long);

  /** Getter for the ColorPalette attribute. */
  itkGetConstReferenceMacro(ColorPalette, PaletteType);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

  BMPImageIO();
  ~BMPImageIO() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BMPImageIO);

  void SwapBytesIfNecessary(void *buffer, SizeValueType numberOfPixels);

  /** This methods ensures that the endianness is respected */
  void Write32BitsInteger(unsigned int value);

  void Write16BitsInteger(unsigned short value);

  RGBPixelType GetColorPaletteEntry(const unsigned char entry) const;

  std::ifstream               m_Ifstream;
  std::ofstream               m_Ofstream;
  long                        m_BitMapOffset;
  bool                        m_FileLowerLeft;
  short                       m_Depth;
  unsigned short              m_NumberOfColors;
  unsigned int                m_ColorPaletteSize;
  long                        m_BMPCompression;
  unsigned long               m_BMPDataSize;
  PaletteType                 m_ColorPalette;
};
} // end namespace itk

#endif // itkBMPImageIO_h
