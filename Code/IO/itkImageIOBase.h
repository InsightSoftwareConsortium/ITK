/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIOBase.h
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
#ifndef __itkImageIOBase_h
#define __itkImageIOBase_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkIndent.h"
#include "itkImageIORegion.h"
#include <string>

namespace itk
{

/** \brief Abstract superclass defines image IO interface.
 *
 * An itk::ImageIOClass is a class that reads and/or writes image data of a
 * particular format. The ImageIOClass encapsulates both the reading and
 * writing of a particular form of data, such as PNG or raw binary. The
 * ImageIOClass is typically used by the ImageFileReader class (to read data)
 * and the ImageFileWriter (to write data).
 *
 * A Pluggable factory pattern is used this allows different kinds of readers
 * to be registered (even at run time) without having to modify the
 * code in this class.  
 *
 * \sa ImageFileWriter
 * \sa ImageFileReader
 */
class ITK_EXPORT ImageIOBase : public Object
{
public:
  /** Standard class typedefs. */
  typedef ImageIOBase            Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageIOBase, Superclass);

  /** Used to return information when types are unknown. */
  class UnknownType {};

  /** Enums used to manipulate the pixel and component type. (Typically a 
   * pixel is assumed to be made up of one or more components.) */
  typedef  enum {UNKNOWN,UCHAR,CHAR,USHORT,SHORT,UINT,INT,ULONG,LONG,
                 FLOAT,DOUBLE} ComponentType;

  /** Set the filename. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);
  
  /** Set/Get the image dimensions in the x, y, z, etc. directions. 
   * GetDimensions() is typically used after reading the data; the
   * SetDimensions() is used prior to writing the data. */
  virtual void SetDimensions(unsigned int i, unsigned int dim);
  virtual unsigned int GetDimensions(unsigned int i) const;
  
  /** Specify the region of the image data to either read or
   * write. The IORegion specifies the part of the image to read or
   * write. Regions are defined with an index and a size vector. These
   * vectors define the start (lower-left corner) and length of the
   * region within the image. Make sure that the IORegion lies within
   * the image. */
  itkSetMacro(IORegion, ImageIORegion);
  itkGetMacro(IORegion, ImageIORegion);

  /** Set/Get the image origin on a axis-by-axis basis. The SetOrigin() method 
   * is required when writing the image. */
  virtual void SetOrigin(unsigned int i, double origin) {}
  virtual double GetOrigin(unsigned int i) const
    { return 0.0; }

  /** Set/Get the image spacing on an axis-by-axis basis. The
   * SetSpacing() method is required when writing the image. */
  virtual void SetSpacing(unsigned int i, double spacing) {}
  virtual double GetSpacing(unsigned int i) const
    { return 1.0; }

 /*-------- This part of the interfaces deals with reading data ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char*) = 0;
  
  /** Read the spacing and dimentions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void ReadImageInformation() = 0;
  
  /** Set/Get the type of the pixel. The pixel type and component type may
   * be different. By default, they are assumed to be the same. The pixel
   * type may be determined by the reader (from the file) or from the
   * writer (the input type). */
  virtual const std::type_info& GetPixelType() const;
  virtual void SetPixelType(const ComponentType& ctype);

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer) = 0;

  /** The guts of this class. Returns the data in the requested region, 
   * which holds the raw pixels of the image read from disk. */
  void* GetRequestedRegionData() const 
    {return m_RequestedRegionData;}

  /** Set/Get the component type of the image. The readering and writing
   * process typically only supports the native types. */
  itkSetMacro(ComponentType,ComponentType);
  itkGetConstMacro(ComponentType,ComponentType);

  /** Set/Get the number of components per pixel in the image. This may
   * be set by the reading process. */
  itkSetMacro(NumberOfComponents,unsigned int);
  itkGetMacro(NumberOfComponents,unsigned int);

  /** Get the number of independent variables (dimensions) in the image
   * being read. */
  itkGetMacro(NumberOfDimensions, unsigned int);

  /** Convenient method for accessing the number of bytes to get to 
   * the next pixel. Returns m_Strides[1]; */
  virtual unsigned int GetPixelStride () const;

  /** Return the number of bytes in the image. */
  unsigned int GetImageSizeInBytes() const;
  
  /*-------- This part of the interfaces deals with writing data ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char*)  = 0;
  
  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(void* buffer) = 0;

protected:
  ImageIOBase();
  ~ImageIOBase();
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Utility methods for working with ComponentType. */
  const std::type_info& ConvertToTypeInfo(ComponentType ) const;
  unsigned int GetSizeOfType(ComponentType ) const;
    
  /** Set the number of independent variables (dimensions) in the image
   * being read. */
  void SetNumberOfDimensions(unsigned int);

  /** Does the ImageIOBase object have enough info to be of use? */
  bool m_Initialized;

  /** Filename: pathname + filename + file extension. */
  std::string m_FileName;

  /** Stores the number of components per pixel. This will be 1 for 
   * grayscale images, 3 for RGBPixel images, and 4 for RGBPixelA images. */
  unsigned int m_NumberOfComponents;

  /** The region to read or write. The region contains information about the
   * data within the region to read or write. */
  ImageIORegion m_IORegion;

  /** The number of dimensions in the image. */
  unsigned int m_NumberOfDimensions;

  /** The array which stores the number of pixels in the x, y, z directions. */
  std::vector<unsigned int> m_Dimensions;

  /** Stores the number of bytes it takes to get to the next 'thing'
   * e.g. component, pixel, row, slice, etc. */
  std::vector<unsigned int> m_Strides;

  /** Stores the raw pixels of the image. */
  void* m_RequestedRegionData;

  /** Return the object to an initialized state, ready to be used */
  virtual void Reset(const bool freeDynamic = true);

  /** Resize the ImageIOBase object to new dimensions. */
  void Resize(const unsigned int numDimensions, 
              const unsigned int* dimensions);

  /** Calculates the different strides (distance from one thing to the next).
   * Upon return,
   * strides[0] = bytes to get to the next component of a pixel,
   * strides[1] = bytes to get to the next pixel in x direction,
   * strides[2] = bytes to get to the next row in y direction,
   * strides[3] = bytes to get to the next slice in z direction, etc. */
  void ComputeStrides();

  /** Used internally to keep track of the type of the pixel. */
  ComponentType m_PixelType;

  /** Used internally to keep track of the type of the component. It is set
   * when ComputeStrides() is invoked. */
  ComponentType m_ComponentType;

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a 
   * component size of 1 byte. This method can be invoked only after
   * the component type is set. */
  virtual unsigned int GetComponentSize() const;

  /** Return the number of pixels in the image. */
  unsigned int GetImageSizeInPixels() const;

  /** Return the number of pixels times the number 
   * of components in the image. */
  unsigned int GetImageSizeInComponents() const;

  /** Convenient method for accessing number of bytes to get to the next pixel 
   * component. Returns m_Strides[0]. */
  unsigned int GetComponentStride() const;

  /** Convenient method for accessing the number of bytes to get to the 
   * next row. Returns m_Strides[2]. */
  unsigned int GetRowStride () const;

  /** Convenient method for accessing the number of bytes to get to the 
   * next slice. Returns m_Strides[3]. */
  unsigned int GetSliceStride () const;

private:
  ImageIOBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif // __itkImageIOBase_h
