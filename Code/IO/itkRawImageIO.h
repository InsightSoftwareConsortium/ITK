/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRawImageIO_h
#define __itkRawImageIO_h

#include "itkImageIOBase.h"
#include "itkIndex.h"
#include "itkImageRegion.h"
#include "itkPixelTraits.h"
#include "itkVersion.h"
#include <string>
#include <fstream>

namespace itk
{

/** \class RawImageIO
 * \brief Read and write raw binary images.
 *
 * This class reads 2D or 3D images. Because raw data has little useful 
 * information, the user is responsible for specifying pixel type, 
 * dimensions, spacing, origin, header type, and so on. (Note: the
 * pixel type and image dimension is defined via the template parameter.)
 *
 * \sa ImageFileReader
 * 
 * \ingroup IOFilters
 */

template <class TPixel, unsigned int VImageDimension=2>
class ITK_EXPORT RawImageIO : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef RawImageIO Self;
  typedef ImageIOBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RawImageIO, ImageIOBase);

  /** Pixel typedef support. Used to declare pixel type in filters
   * or other operations. */
  typedef TPixel PixelType;

  /** If the data is in the tail end of the file, you want to
   * explicitly set the header size. */
  void SetHeaderSize(unsigned long size);
  unsigned long GetHeaderSize();
  
  /** The number of dimensions stored in a file. Defaults to two. If two,
   * each file contains one "slice". If three, each file will contain one
   * "volume". */
  
  itkSetMacro(FileDimensionality, unsigned long);
  itkGetMacro(FileDimensionality, unsigned long);

  
  /** Get the type of the pixel.  */
  virtual const std::type_info& GetPixelType() const
    {return typeid(PixelType);}

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a 
   * component size of 1 byte. */
  virtual unsigned int GetComponentSize() const
    {return sizeof(PixelTraits<PixelType>::ValueType);}

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIOBase can read the
   * file specified. */
  virtual bool CanReadFile(const char*) 
    {return true;}

  /** Binary files have no image information to read. This must be set by the
   * user of the class. */
  virtual void ReadImageInformation() 
    {return;}

  /** Reads data from disk into internal memory (the RequestedRegionData). */
  virtual void Read();
  
  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer);

  /** Set/Get the Data mask. */
  itkGetConstMacro(ImageMask,unsigned short);
  void SetImageMask(unsigned long val) 
    {
      if (val == m_ImageMask) { return; }
      m_ImageMask = ((unsigned short)(val)); 
      this->Modified();
    }
    
  /** Read a file's header to determine image dimensions, etc. */
  virtual void ReadHeader (const std::string fileName="") {};

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char*)
    { return false; }

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(void* buffer)
    { return; }

protected:
  RawImageIO();
  ~RawImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;

  //void ComputeInternalFileName(unsigned long slice);
  void OpenFile();
  
private:
  RawImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  std::ifstream m_File;
  std::string   m_FilePrefix;
  std::string   m_FilePattern;
  std::string   m_InternalFileName;

  unsigned long  m_FileDimensionality;
  bool           m_ManualHeaderSize;
  unsigned long  m_HeaderSize;
  unsigned short m_ImageMask;
};

template <class TPixel, unsigned int VImageDimension>
class ITK_EXPORT RawImageIOFactory : public ObjectFactoryBase
{
public:
  RawImageIOFactory();
  const char* GetITKSourceVersion();
  const char* GetDescription() const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

protected:
  typedef RawImageIO<TPixel,VImageDimension> myProductType;
  const myProductType* m_MyProduct;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION

#include "itkRawImageIO.txx"
#endif

#endif
