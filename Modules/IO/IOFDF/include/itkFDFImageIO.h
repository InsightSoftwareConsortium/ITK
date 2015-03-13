/*  Copyright (C) 2004 Glenn Pierce.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef itkFDFImageIO_h
#define itkFDFImageIO_h
#include "IOFDFExport.h"
#include "itkImageIOBase.h"

namespace itk
{

/** \class FDFImageIO
 * \brief ImageIO object for reading and writing FDF images
 *
 * \ingroup IOFilters
 *
 */
class IOFDF_EXPORT FDFImageIO : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef FDFImageIO         Self;
  typedef ImageIOBase        Superclass;
  typedef SmartPointer<Self> Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FDFImageIO, ImageIOBase);

  virtual bool
  SupportsDimension(unsigned long dim)
  {
    if (dim == 2 || dim == 3)
    {
      return true;
    }
    else
    {
      return false;
    }
  }


  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool
  CanReadFile(const char *);

  /** Set the spacing and diemention information for the set filename. */
  virtual void
  ReadImageInformation();

  /** Get the type of the pixel.  */
  //   virtual const std::type_info& GetPixelType() const;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void
  Read(void * buffer);

  /** Reads 3D data from multiple files assuming one slice per file. */
  virtual void
  ReadVolume(void * buffer);

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a
   * component size of 1 byte. */
  //   virtual unsigned int GetComponentSize() const;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool
  CanWriteFile(const char *);

  /** Writes the spacing and dimentions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void
  WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void
  Write(const void * buffer);

protected:
  FDFImageIO();
  ~FDFImageIO();
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  void
  WriteSlice(std::string & fileName, const void * buffer);

  int
  ReadHeader(const char * FileNameToRead);

private:
  FDFImageIO(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  void
  SwapBytesIfNecessary(void * buffer, unsigned long numberOfPixels);

  // Position after ReadImageInformation.
  size_t m_InputPosition;

  std::string        m_SpatialRank;
  std::string        m_Checksum;
  std::string        m_Bits;
  std::vector<int>   m_Size;
  std::vector<float> m_Location;
  std::vector<float> m_Span;
  std::vector<float> m_Roi;
};

} // end namespace itk


#define RAISE_EXCEPTION()                            \
  {                                                  \
    ExceptionObject exception(__FILE__, __LINE__);   \
    exception.SetDescription("File cannot be read"); \
    throw exception;                                 \
  }

#endif
