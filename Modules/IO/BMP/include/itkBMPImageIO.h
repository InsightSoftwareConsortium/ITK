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
#ifndef __itkBMPImageIO_h
#define __itkBMPImageIO_h


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
class ITK_EXPORT BMPImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef BMPImageIO                Self;
  typedef ImageIOBase               Superclass;
  typedef SmartPointer< Self >      Pointer;
  typedef RGBPixel< uint8_t > RGBPixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BMPImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *);

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *);

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer);

  BMPImageIO();
  ~BMPImageIO();
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  BMPImageIO(const Self &);     //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  void SwapBytesIfNecessary(void *buffer, SizeValueType numberOfPixels);

  /** This methods ensures that the endianness is respected */
  void Write32BitsInteger(unsigned int value);

  void Write16BitsInteger(uint16_t value);

  std::ifstream               m_Ifstream;
  std::ofstream               m_Ofstream;
  long                        m_BitMapOffset;
  bool                        m_FileLowerLeft;
  short                       m_Depth;
  bool                        m_Allow8BitBMP;
  uint16_t              m_NumberOfColors;
  unsigned int                m_ColorTableSize;
  long                        m_BMPCompression;
  unsigned long               m_BMPDataSize;
  std::vector< RGBPixelType > m_ColorPalette;
};
} // end namespace itk

#endif // __itkBMPImageIO_h
