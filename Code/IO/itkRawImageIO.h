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
  
  /** Get the number of independent variables (dimensions) in the image
   * being read. */
  unsigned int GetNumberOfDimensions() const
    {return VImageDimension;};

  /** Get the type of the pixel.  */
  virtual const std::type_info& GetPixelType() const
    {return typeid(PixelType);}

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a 
   * component size of 1 byte. */
  virtual unsigned int GetComponentSize() const
    {return 0;}

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
  virtual void Read(void* buffer) {}

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

protected:
  typedef RawImageIO<TPixel,VImageDimension> myProductType;
  const myProductType* m_MyProduct;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION

#include "itkRawImageIO.txx"
#endif

#endif
