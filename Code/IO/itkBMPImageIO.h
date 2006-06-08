/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBMPImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $1.0$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBMPImageIO_h
#define __itkBMPImageIO_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <fstream>
#include "itkImageIOBase.h"
#include <stdio.h>
#include "itkRGBPixel.h"

namespace itk
{

/** \class BMPImageIO 
 *
 *  \brief Read BMPImage file format. 
 *
 *  \ingroup IOFilters
 *
 */
class ITK_EXPORT BMPImageIO : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef BMPImageIO              Self;
  typedef ImageIOBase             Superclass;
  typedef SmartPointer<Self>      Pointer;
  typedef RGBPixel<unsigned char> RGBPixelType;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BMPImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char*) ;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();
  
  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char*);

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation();
  
  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void* buffer);


  BMPImageIO();
  ~BMPImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  BMPImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  void SwapBytesIfNecessary(void* buffer, unsigned long numberOfPixels);
 
  std::ifstream   m_Ifstream;
  std::ofstream   m_Ofstream;
  long            m_BitMapOffset;
  bool            m_FileLowerLeft;
  short           m_Depth;
  bool            m_Allow8BitBMP;
  short           m_NumberOfColors;
  long            m_BMPCompression;
  long            m_BMPDataSize;
  std::vector<RGBPixelType> m_ColorPalette;
 
};

} // end namespace itk

#endif // __itkBMPImageIO_h
