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

#include "itkImageIO.h"
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
 * This class reads 2D or 3D images.
 *
 * \sa FileIOToImageFilter
 * */

  template <class TPixel, unsigned int VImageDimension=2>
class ITK_EXPORT RawImageIO : public ImageIO
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
  itkTypeMacro(RawImageIO, ImageIO);

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageIO  Superclass;

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
   * Index typedef support. An index is used to access pixel values.
   */
  typedef Index<VImageDimension>  IndexType;

  /** 
   * Size typedef support. A size is used to define region bounds.
   */
  typedef Size<VImageDimension>  SizeType;

  /** 
   * Region typedef support. A region is used to specify a subset of an image.
   */
  typedef ImageRegion<VImageDimension>  RegionType;

  /**
   * Set the region object that defines the size and starting index
   * for the imported image. This will serve as the LargestPossibleRegion,
   * the BufferedRegion, and the RequestedRegion.
   * \sa ImageRegion
   */
  void SetRegion(const RegionType &region)
    { if (m_Region != region) {m_Region = region; this->Modified();} };
  const RegionType& GetRegion() const
    { return m_Region;};
  
  /** 
   * Set the spacing (size of a pixel) of the image.
   * \sa GetSpacing()
   */
  itkSetVectorMacro(Spacing, const double, VImageDimension);
  itkGetVectorMacro(Spacing, const double, VImageDimension);
  
  /** 
   * Set the origin of the image.
   * \sa GetOrigin()
   */
  itkSetVectorMacro(Origin, const double, VImageDimension);
  itkGetVectorMacro(Origin, const double, VImageDimension);


  /** 
   * The number of dimensions stored in a file. This defaults to two.
   */
  itkSetMacro(FileDimensionality, unsigned long);
  itkGetMacro(FileDimensionality, unsigned long);
  
  /**
   * Get the size of the header computed by this object.
   */
  unsigned long GetHeaderSize();
  unsigned long GetHeaderSize(unsigned long slice);

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
    { this->SetImageByteOrder(Superclass::BigEndian); }
  void SetImageByteOrderToLittleEndian()
    { this->SetImageByteOrder(Superclass::LittleEndian); }

  //---------------------------------------------------------------------
  // The following methods satisfy the ImageIO abstract API
  
  /**
   * Default load; do whatever is appropriate for the filetype.
   */
  virtual void Load ();

  /**
   * Read a file's header to determine image dimensions, etc.
   */
  virtual void ReadHeader (const std::string fileName="") {};

  /**
   * Returns the file extension that a particular ImageIO subclass
   * is capable of handling (e.g. .jpg, .mhd, etc.)
   * Currently only a single string is returned, but can be modified
   * so that a whole list of strings is returned.
   */
  virtual FileExtensionsListType& GetSupportedFileExtensions () const;

protected:
  RawImageIO();
  ~RawImageIO();
  RawImageIO(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  void ComputeInternalFileName(unsigned long slice);
  void OpenFile();
  
private:
  std::ifstream m_File;
  std::string m_FilePrefix;
  std::string m_FilePattern;
  std::string m_InternalFileName;

  RegionType  m_Region;
  double      m_Spacing[VImageDimension];
  double      m_Origin[VImageDimension];

  unsigned long m_ImageExtent[6];
  unsigned long m_ImageVOI[6];
  unsigned long m_FileDimensionality;
  bool m_ManualHeaderSize;
  unsigned long m_HeaderSize;
  ByteOrder m_ImageByteOrder;
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
