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

#include <fstream>
#include "itkImageIOBase.h"
#include "itkIndex.h"
#include "itkImageRegion.h"
#include "itkPixelTraits.h"
#include "itkByteSwapper.h"
#include "itkVersion.h"
#include <string>


namespace itk
{

/** \class RawImageIO
 * \brief Read and write raw binary images.
 *
 * This class reads and writes 2D or 3D images. Because raw data has 
 * little useful information built into the format,
 * so the user is responsible for specifying pixel type, 
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

  /** this type is used in case the pixel has several components */
  typedef typename PixelTraits<PixelType>::ValueType       ComponentType;

  /** Helper class to swap bytes when necessary */
  typedef ByteSwapper<ComponentType>               ByteSwapperType;


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
    {return sizeof(typename PixelTraits<PixelType>::ValueType);}

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIOBase can read the
   * file specified. Always returns false because we don't want to use
   * this reader unless absolutely sure (i.e., manual ImageIO creation). */
  virtual bool CanReadFile(const char*) 
    {return false;}

  /** Binary files have no image information to read. This must be set by the
   * user of the class. */
  virtual void ReadImageInformation() 
    {return;}

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
  virtual void ReadHeader (const std::string = std::string()) {}

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Returns true if this ImageIO can write the specified file. 
   * False is only returned when the file name is not specified. Otherwise
   * true is always returned. */
  virtual bool CanWriteFile(const char*);

  /** Binary files have no image information to read.  */
  virtual void WriteImageInformation(void) 
    {return;}


  /** Writes the data to disk from the memory buffer provided. */
  virtual void Write(const void* buffer);

protected:
  RawImageIO();
  ~RawImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;

  //void ComputeInternalFileName(unsigned long slice);
  void OpenFileForReading(std::ifstream& is);
  void OpenFileForWriting(std::ofstream& os);
  
private:
  RawImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

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
  /** Standard class typedefs. */
  typedef RawImageIOFactory<TPixel,VImageDimension>   Self;
  typedef ObjectFactoryBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Class methods used to interface with the registered factories. */
  const char* GetITKSourceVersion(void) const
    {
    return ITK_SOURCE_VERSION;
    }

  const char* GetDescription(void) const
    {
    return "Raw ImageIO Factory, allows the loading of Raw images into insight";
    }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RawImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
    {
    ObjectFactoryBase::RegisterFactory( Self::New() );
    }


protected:
  RawImageIOFactory() {};
  ~RawImageIOFactory() {};
  typedef RawImageIO<TPixel,VImageDimension> myProductType;
  const myProductType* m_MyProduct;

private:
  RawImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRawImageIO.txx"
#endif

#endif
