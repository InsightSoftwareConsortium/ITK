/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageIO_h
#define __itkImageIO_h

#include "itkIntTypes.h"
#include "itkProcessObject.h"
#include "itkObjectFactory.h"
#include "itkImageIOCommon.h"
#include <string>
#include "itkIndent.h"
#include <deque>
#include <ctype.h>

namespace itk
{

class ITK_EXPORT ImageIO : public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageIO            Self;
  typedef SmartPointer<Self>  Pointer;
  typedef ProcessObject  Superclass;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageIO, Superclass);

  /** Read a file's header to determine image dimensions, etc.
   * fileName is file to read from. default="", which uses m_FileName instead */
  virtual void ReadHeader(const std::string fileName="") = 0;

  /** Get the image origin. */
  virtual const double* GetOrigin() const =0;

  /** Get the image spacing. */
  virtual const double* GetSpacing() const =0;

  /** The type of list for file extensions. */
  typedef std::deque<std::string> FileExtensionsListType;

  /** Returns the file extension that a particular ImageIO subclass
   * is capable of handling (e.g. .jpg, .mhd, etc.)
   * Currently only a single string is returned, but can be modified
   * so that a whole list of strings is returned. */
  virtual FileExtensionsListType& GetSupportedFileExtensions() const = 0;

  /** Set the filename. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);
  
  /** Specify file prefix for the image file(s). You should specify either
   * a FileName or FilePrefix. Use FilePrefix if the data is stored
   * in multiple files. (Note: the FileName ivar is available from the
   * superclass.) */
  itkSetStringMacro(FilePrefix);
  itkGetStringMacro(FilePrefix);
  
  /** The sprintf format used to build filename from FilePrefix and number. */
  itkSetStringMacro(FilePattern);
  itkGetStringMacro(FilePattern);
  
  /** Set the number of components per pixel in the image. This may
   * be set by the reading process. */
  itkSetMacro(NumberOfComponents,unsigned int);
  itkGetConstMacro(NumberOfComponents,unsigned int);
    
  /** The guts of this class. Returns FileData, which holds the raw
   * pixels of the image read from disk. */
  void* GetFileData();

  /** Set the number of independent variables (dimensions) in the image
   * being read. */
  itkSetMacro(NumberOfDimensions, unsigned int);
  itkGetMacro(NumberOfDimensions, unsigned int);
  
  /** Return and set the size in x, y, z, etc. dimensions. */
  virtual unsigned int GetDimensions(unsigned int i) const;

  /** Set/Get the type of the pixel. Often this is set during the read
   * operation and does not always need to be set. */
  itkSetMacro(PixelType, AtomicPixelType);
  itkGetMacro(PixelType, AtomicPixelType);
  
  /** Convenient method for accessing the number of bytes to get to 
   * the next pixel. Returns m_Strides[1]; */
  virtual unsigned int GetPixelStride () const;

  /** Convenient enum for specifyin byte order. */
  enum ByteOrder {LittleEndian, BigEndian};

protected:
  ImageIO();
  ~ImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Does the ImageIO object have enough info to be of use? */
  bool m_Initialized;

  /** Filename: pathname + filename + file extension. */
  std::string m_FileName;
  std::string m_FilePrefix;
  std::string m_FilePattern;
  
  /** Type of the pixel. */
  AtomicPixelType  m_PixelType;

  /** Stores the number of components per pixel. This will be 1 for 
   * grayscale images, 3 for RGBPixel images, and 4 for RGBPixelA images. */
  unsigned int m_NumberOfComponents;

  /** The number of dimensions in the image. */
  unsigned int m_NumberOfDimensions;

  /** The array which stores the number of pixels in the x, y, z directions. */
  unsigned int m_Dimensions[ITK_MAX_DIMENSIONS];

  /** Stores the number of bytes it takes to get to the next 'thing'
   * e.g. component, pixel, row, slice, etc. */
  unsigned int m_Strides[ITK_MAX_DIMENSIONS];

  /** Stores the raw pixels of the image */
  void* m_FileData;

  /** Return the object to an initialized state, ready to be used */
  virtual void Reset(const bool freeDynamic = true);

  /** Resize the ImageIO object to new dimensions. */
  void Resize(const unsigned int numDimensions, 
              const unsigned int* dimensions);

  /** Calculates the different strides (distance from one thing to the next).
   * Upon return,
   * strides[0] = bytes to get to the next component of a pixel,
   * strides[1] = bytes to get to the next pixel in x direction,
   * strides[2] = bytes to get to the next row in y direction,
   * strides[3] = bytes to get to the next slice in z direction, etc. */
  void ComputeStrides();

  /** Return the number of pixels in the image. */
  unsigned int ImageSizeInPixels() const;

  /** Return the number of pixels times the number 
   * of components in the image. */
  unsigned int ImageSizeInComponents() const;

  /** Return the number of bytes in the image. */
  unsigned int ImageSizeInBytes() const;

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
  ImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};

} // end namespace itk

#endif // __itkImageIO_h
