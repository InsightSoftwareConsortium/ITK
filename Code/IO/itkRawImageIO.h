/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkRawImageIO_h
#define __itkRawImageIO_h

#include "itkImageIOBase.h"
#include "itkIndex.h"
#include "itkImageRegion.h"
#include "itkVersion.h"
#include <string>
#include <fstream>

namespace itk
{

/**
 * \class RawImageIO
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
 * */

template <class TPixel, unsigned int VImageDimension=2>
class ITK_EXPORT RawImageIO : public ImageIOBase
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef RawImageIO Self;
  typedef SmartPointer<Self>  Pointer;

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RawImageIO, ImageIOBase);

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageIOBase  Superclass;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** 
   * Pixel typedef support. Used to declare pixel type in filters
   * or other operations.
   */
  typedef TPixel PixelType;

  /**
   * Determine the file type. Returns true if this ImageIOBase can read the
   * file specified.
   */
  virtual bool CanReadFile(const char*) 
    {return true;}

  /**
   * Get the type of the pixel. 
   */
  virtual const type_info& GetPixelType() const
    {return typeid(PixelType);}

  /**
   * Loads the data from disk into internal memory (the RequestedRegionData).
   */
  virtual void Load();
  
  /**
   * Loads the data from disk into the memory buffer provided.
   */
  virtual void Load(void* buffer) {}

  /**
   * Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a 
   * component size of 1 byte.
   */
  virtual unsigned int GetComponentSize() const
    {return 0;}

  /** 
   * Set and get the spacing (size of a pixel) of the image.
   * \sa GetSpacing()
   */
  void SetDimensions(const unsigned int *dims);
  unsigned int *GetDimensions() const;
  
  /** 
   * Set and get the spacing (size of a pixel) of the image.
   * \sa GetSpacing()
   */
  itkSetVectorMacro(Spacing, const double, VImageDimension);
  itkGetVectorMacro(Spacing, const double, VImageDimension);
  
  /** 
   * Set and get the origin of the image.
   * \sa GetOrigin()
   */
  itkSetVectorMacro(Origin, const double, VImageDimension);
  itkGetVectorMacro(Origin, const double, VImageDimension);

  /**
   * Get the number of independent variables (dimensions) in the image
   * being read.
   */
  unsigned int GetNumberOfDimensions() const
    {return VImageDimension};

  /**
   * Get the size of the header computed by this object.
   */
  unsigned long GetHeaderSize();

  /**
   * If there is a tail on the file, you want to explicitly set the
   * header size.
   */
  void SetHeaderSize(unsigned long size);
  
  /**
   * Set/Get the Data mask.
   */
  itkGetConstMacro(ImageMask,unsigned short);
  void SetImageMask(unsigned long val) 
    {if (val == m_ImageMask) { return; }
    m_ImageMask = ((unsigned short)(val)); this->Modified();}
  
  /**
   * Enums used to specify raw type: whether binary or ASCII.
   */
  typedef  enum {ASCII,Binary} FileType;
  
  /**
   * Enums used to specify byte order; whether Big Endian or Little Endian.
   */
  typedef  enum {BigEndian,LittleEndian} ByteOrder;
  
  /**
   * These methods indicate the byte ordering of the file you are trying
   * to read in. These methods will then either swap or not swap
   * the bytes depending on the byte ordering of the machine it is
   * being run on. For example, reading in a BigEndian file on a
   * BigEndian machine will result in no swapping. Trying to read
   * the same file on a LittleEndian machine will result in swapping.
   * Note: most UNIX machines are BigEndian while PC's
   * and VAX's are LittleEndian. So if the file you are reading
   * in was generated on a VAX or PC, SetImageByteOrderToLittleEndian 
   * otherwise SetImageByteOrderToBigEndian. 
   */
  itkSetMacro(ImageByteOrder,ByteOrder);
  itkGetConstMacro(ImageByteOrder,ByteOrder);
  void SetImageByteOrderToBigEndian()
    { this->SetImageByteOrder(BigEndian); }
  void SetImageByteOrderToLittleEndian()
    { this->SetImageByteOrder(LittleEndian); }

  /**
   * Read a file's header to determine image dimensions, etc.
   */
  virtual void ReadHeader (const std::string fileName="") {};

protected:
  RawImageIO();
  ~RawImageIO();
  RawImageIO(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  //void ComputeInternalFileName(unsigned long slice);
  void OpenFile();
  
private:
  std::ifstream m_File;
  std::string   m_FilePrefix;
  std::string   m_FilePattern;
  std::string   m_InternalFileName;

  double       m_Spacing[VImageDimension];
  double       m_Origin[VImageDimension];

  unsigned long  m_FileDimensionality;
  bool           m_ManualHeaderSize;
  unsigned long  m_HeaderSize;
  ByteOrder      m_ImageByteOrder;
  unsigned short m_ImageMask;
};

template <class TPixel, unsigned int VImageDimension>
class ITK_EXPORT RawImageIOFactory : public ObjectFactoryBase
{
public:
  RawImageIOFactory();
  const char* GetITKSourceVersion();
  const char* GetDescription() const;

protected:
  typedef RawImageIO<TPixel,VImageDimension> myProductType;
  const myProductType* m_MyProduct;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION

#include "itkRawImageIO.txx"
#endif

#endif
