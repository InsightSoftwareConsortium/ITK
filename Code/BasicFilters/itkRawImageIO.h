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
#include <itkVersion.h>
#include <string>

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
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** 
   * Specify file prefix for the image file(s). You should specify either
   * a FileName or FilePrefix. Use FilePrefix if the data is stored
   * in multiple files. (Note: the FileName ivar is available from the
   * superclass.)
   */
  itkSetStringMacro(FilePrefix);
  itkGetStringMacro(FilePrefix);

  /**
   * The sprintf format used to build filename from FilePrefix and number.
   */
  itkSetStringMacro(FilePattern);
  itkGetStringMacro(FilePattern);

  /**
   * Get/Set the extent of the data on disk.  
   */
  itkSetVectorMacro(DataExtent,unsigned long,6);
  itkGetVectorMacro(DataExtent,const unsigned long,6);
  
  /**
   * Set/get the data VOI. You can limit the reader to only
   * read a subset of the data. 
   */
  itkSetVectorMacro(DataVOI,unsigned long,6);
  itkGetVectorMacro(DataVOI,const unsigned long,6);
  
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
  itkGetConstMacro(DataMask,unsigned short);
  void SetDataMask(unsigned long val) 
    {if (val == m_DataMask) { return; }
    m_DataMask = ((unsigned short)(val)); this->Modified();}
  
  /**
   * These methods indicate the byte ordering of the file you are trying
   * to read in. These methods will then either swap or not swap
   * the bytes depending on the byte ordering of the machine it is
   * being run on. For example, reading in a BigEndian file on a
   * BigEndian machine will result in no swapping. Trying to read
   * the same file on a LittleEndian machine will result in swapping.
   * Note: most UNIX machines are BigEndian while PC's
   * and VAX's are LittleEndian. So if the file you are reading
   * in was generated on a VAX or PC, SetDataByteOrderToLittleEndian 
   * otherwise SetDataByteOrderToBigEndian. 
   */
  void SetDataByteOrderToBigEndian();
  void SetDataByteOrderToLittleEndian();
  ByteOrder GetDataByteOrder();
  void SetDataByteOrder(ByteOrder);

  //---------------------------------------------------------------------
  // The following methods satisfy the ImageIO abstract API
  
  /**
   * Default load; do whatever is appropriate for the filetype.
   */
  virtual void Load ();

  /**
   * Load a 2D image. If fileName="" (the default), will read from m_FileName
   */
  virtual void Load2D(const std::string fileName="");

  /**
   * Load a 2D slice from a volume dataset.  fileName is file to read
   * from. default="", which uses m_FileName instead sliceNum is the slice #
   * to load (starting at 0). default = 0.  offset is the offset, in bytes,
   * into fileData at which the data should be loaded default = 0 
   */
  virtual void Load2DSlice(const std::string fileName="",
                           const unsigned int sliceNum=0,
                           const unsigned int offset=0);

  /**
   * Default save; do whatever is appropriate for MetaImages Since MetaImages
   * can be saved as a header/data file pair, we need to be able to pass in
   * these strings as parameters. Most other file formats will simply use
   * m_FullFileName 
   */
  virtual void Save(const std::string headerFile="", 
                    const std::string dataFile="");

  /**
   * Save a 3D image
   */
  virtual void Save3D(const std::string headerFile="", 
                      const std::string dataFile="");

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

  /**
   * Set/Get the image position.
   */
  itkSetVectorMacro(ImageOrigin,float,3);
  itkGetVectorMacro(ImageOrigin,const float,3);

  /**
   * Set/Get the image spacing.
   */
  itkSetVectorMacro(ImageSpacing,const float,3);
  itkGetVectorMacro(ImageSpacing,const float,3);

protected:
  RawImageIO();
  ~RawImageIO();
  RawImageIO(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  float m_ImageOrigin[3];
  float m_ImageSpacing[3];
  
  std::string m_FilePrefix;
  std::string m_FilePattern;
  unsigned long m_DataExtent[6];
  unsigned long m_DataVOI[6];
  unsigned long m_FileDimensionality;
  unsigned long m_HeaderSize;
  unsigned short m_DataMask;
  
};

class ITK_EXPORT RawImageIOFactory : public ObjectFactoryBase
{
public:
  RawImageIOFactory();
  const char* GetITKSourceVersion();
  const char* GetDescription() const;

protected:
  typedef RawImageIO myProductType;
  const myProductType* m_MyProduct;

};

} // namespace itk

#endif
