/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJPEGImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkJPEGImageIO_h
#define __itkJPEGImageIO_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkImageIOBase.h"

namespace itk
{

/* \brief ImageIO object for reading and writing JPEG images
 *
 * \ingroup IOFilters
 *
 */
class ITK_EXPORT JPEGImageIO : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef JPEGImageIO            Self;
  typedef ImageIOBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(JPEGImageIO, ImageIOBase);

  /** Set if the compression should be used for writing */
  itkSetMacro(UseCompression, bool);

  /** Set/Get the level of quality for the output images.*/
  itkSetMacro(Quality, int);
  itkGetMacro(Quality, int);

  /**  */
  itkSetMacro(Progressive, bool);
  itkGetMacro(Progressive, bool);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char*);
  
  /** Set the spacing and diemention information for the set filename. */
  virtual void ReadImageInformation();
  
  /** Get the type of the pixel.  */
  virtual const std::type_info& GetPixelType() const;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer);

  /** Reads 3D data from multiple files assuming one slice per file. */
  virtual void ReadVolume(void* buffer);

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a 
   * component size of 1 byte. */
  virtual unsigned int GetComponentSize() const;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char*);

  /** Writes the spacing and dimentions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void* buffer);

protected:
  JPEGImageIO();
  ~JPEGImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;

  void WriteSlice(std::string& fileName, const void* buffer);

  /** Set if the compression should be used for writing 
   *  the value is false by default */
  bool m_UseCompression;
  /** Determines the quality of compression for written files. 
   *  default = 95 */
  int m_Quality;
  /** Default = true*/
  bool m_Progressive;

private:
  JPEGImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};

} // end namespace itk

#endif // __itkJPEGImageIO_h

