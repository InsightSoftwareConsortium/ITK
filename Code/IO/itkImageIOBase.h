/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIOBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageIOBase_h
#define __itkImageIOBase_h

#include "itkLightProcessObject.h"
#include "itkObjectFactory.h"
#include "itkIndent.h"
#include "itkImageIORegion.h"
#include <string>

namespace itk
{

class FileIteratorBase;

/** \brief Abstract superclass defines image IO interface.
 *
 * ImageIOBase is a class that reads and/or writes image data
 * of a particular format (such as PNG or raw binary). The
 * ImageIOBase encapsulates both the reading and writing of data. The
 * ImageIOBase is used by the ImageFileReader class (to read data)
 * and the ImageFileWriter (to write data) into a single file. The 
 * ImageSeriesReader and ImageSeriesWriter classes are used to read
 * and write data (in conjunction with ImageIOBase) when the data is 
 * represented by a series of files. Normally the user does not directly
 * manipulate this class other than to instantiate it, set the FileName,
 * and assign it to a ImageFileReader/ImageFileWriter or 
 * ImageSeriesReader/ImageSeriesWriter.
 *
 * A Pluggable factory pattern is used this allows different kinds of readers
 * to be registered (even at run time) without having to modify the
 * code in this class.  
 *
 * \sa ImageFileWriter
 * \sa ImageFileReader
 * \sa ImageSeriesWriter
 * \sa ImageSeriesReader
 *
 * \ingroup IOFilters
 *
 */
class ITK_EXPORT ImageIOBase : public LightProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageIOBase            Self;
  typedef LightProcessObject  Superclass;
  typedef SmartPointer<Self>  Pointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageIOBase, Superclass);

  /** Set/Get the name of the file to be read. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);
  
  /** Used to return information when types are unknown. */
  class UnknownType {};

  /** Enums used to manipulate the pixel and component type. (Typically a 
   * pixel is assumed to be made up of one or more components.) */
  typedef  enum {UNKNOWN,UCHAR,CHAR,USHORT,SHORT,UINT,INT,
                 ULONG,LONG, FLOAT,DOUBLE,
                 RGB,RGBA,OFFSET,VECTOR,POINT,COVARIANTVECTOR} IODataType;

  /** Set/Get the number of independent variables (dimensions) in the
   * image being read or written. Note this is not necessarily what
   * is written, rather the IORegion controls that. */
  void SetNumberOfDimensions(unsigned int);
  itkGetMacro(NumberOfDimensions, unsigned int);

  /** Set/Get the image dimensions in the x, y, z, etc. directions. 
   * GetDimensions() is typically used after reading the data; the
   * SetDimensions() is used prior to writing the data. */
  virtual void SetDimensions(unsigned int i, unsigned int dim);
  virtual unsigned int GetDimensions(unsigned int i) const
    { return m_Dimensions[i]; }

  /** Set/Get the image origin on a axis-by-axis basis. The SetOrigin() method 
   * is required when writing the image. */
  virtual void SetOrigin(unsigned int i, double origin);
  virtual double GetOrigin(unsigned int i) const
    { return m_Origin[i]; }

  /** Set/Get the image spacing on an axis-by-axis basis. The
   * SetSpacing() method is required when writing the image. */
  virtual void SetSpacing(unsigned int i, double spacing);
  virtual double GetSpacing(unsigned int i) const
    { return m_Spacing[i]; }

  /** Specify the region of the image data to either read or
   * write. The IORegion specifies the part of the image to read or
   * write. Regions are defined with an index and a size vector. These
   * vectors define the start (lower-left corner) and length of the
   * region within the image. Make sure that the IORegion lies within
   * the image. */
  itkSetMacro(IORegion, ImageIORegion);
  itkGetMacro(IORegion, ImageIORegion);

  /** Set/Get the type of the pixel. The pixel type and component type may
   * be different. By default, they are assumed to be the same. The pixel
   * type may be determined by the reader (from the file) or from the
   * writer (the writer's input type). */
  virtual const std::type_info& GetPixelType() const;
  virtual void SetPixelType(const IODataType ctype);

  /** This special, convenience version of SetPixelType() also sets
   * the number of components and the component type. The function
   * returns false if the pixel type is unsupported. */
  virtual bool SetPixelType(const std::type_info& ptype);

  /** Set/Get the component type of the image. The readering and writing
   * process typically only supports the native types, with special
   * case support like RGBPixel. */
  itkSetMacro(ComponentType,IODataType);
  itkGetMacro(ComponentType,IODataType);

  /** Set/Get the number of components per pixel in the image. This may
   * be set by the reading process. */
  itkSetMacro(NumberOfComponents,unsigned int);
  itkGetMacro(NumberOfComponents,unsigned int);

  /** Convenience method returns the IODataType as a string. This can be
   * used for writing output files. */
  std::string ReturnTypeAsString(IODataType) const;

  /** Enums used to specify write style: whether binary or ASCII. Some
   * subclasses use this, some ignore it. */
  typedef  enum {ASCII,Binary,TypeNotApplicable} FileType;
  
  /** Enums used to specify byte order; whether Big Endian or Little Endian.
   * Some subclasses use this, some ignore it. */
  typedef  enum {BigEndian,LittleEndian,OrderNotApplicable} ByteOrder;
  
  /** These methods control whether the file is written binary or ASCII.
   * Many file formats (i.e., subclasses) ignore this flag. */
  itkSetMacro(FileType,FileType);
  itkGetConstMacro(FileType,FileType);
  void SetFileTypeToASCII()
    { this->SetFileType(ASCII); }
  void SetFileTypeToBinary()
    { this->SetFileType(Binary); }

  /** These methods indicate the byte ordering of the file you are
   * trying to read in. These methods will then either swap or not
   * swap the bytes depending on the byte ordering of the machine it
   * is being run on. For example, reading in a BigEndian file on a
   * BigEndian machine will result in no swapping. Trying to read the
   * same file on a LittleEndian machine will result in swapping.
   * Note: most UNIX machines are BigEndian while PC's and VAX's are
   * LittleEndian. So if the file you are reading in was generated on
   * a VAX or PC, SetByteOrderToLittleEndian() otherwise
   * SetByteOrderToBigEndian().  Some ImageIOBase subclasses
   * ignore these methods. */
  itkSetMacro(ByteOrder,ByteOrder);
  itkGetConstMacro(ByteOrder,ByteOrder);
  void SetByteOrderToBigEndian()
    { this->SetByteOrder(BigEndian); }
  void SetByteOrderToLittleEndian()
    { this->SetByteOrder(LittleEndian); }
  
  /** Convenient method for accessing the number of bytes to get to 
   * the next pixel. Returns m_Strides[1]; */
  virtual unsigned int GetPixelStride () const;

  /** Return the number of pixels in the image. */
  unsigned int GetImageSizeInPixels() const;

  /** Return the number of bytes in the image. */
  unsigned int GetImageSizeInBytes() const;
  
  /** Return the number of pixels times the number 
   * of components in the image. */
  unsigned int GetImageSizeInComponents() const;

 /*-------- This part of the interfaces deals with reading data ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char*) = 0;
  
  /** Read the spacing and dimentions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void ReadImageInformation() = 0;
  
  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer) = 0;


  /*-------- This part of the interfaces deals with writing data ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char*)  = 0;

  /** Writes the spacing and dimentions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation() = 0;
  
  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. The buffer is cast to a 
   * pointer to the beginning of the image data. */
  virtual void Write( const void* buffer) = 0;

  /* --- Support reading and writing data as a series of files. --- */

  /** The file iterator interfaces with the ImageSeriesReader
   * and ImageSeriesWriter to read and write multiple files. The
   * subclasses of ImageIOBase create and return the correct type 
   * of file iterator to use. */
  virtual FileIteratorBase* NewFileIterator();

  /** The different types of ImageIO's can support data of varying
   * dimensionality. For example, some file formats are strictly 2D
   * while others can support 2D, 3D, or even n-D. This method returns
   * true/false as to whether the ImageIO can support the dimension
   * indicated. */
  virtual bool SupportsDimension(unsigned long dim)
    {return (dim == 2);}

protected:
  ImageIOBase();
  ~ImageIOBase();
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Utility methods for working with IODataType. */
  const std::type_info& ConvertToTypeInfo(IODataType ) const;
  unsigned int GetSizeOfType(IODataType ) const;
    
  /** Used internally to keep track of the type of the pixel. */
  IODataType m_PixelType;

  /** Used internally to keep track of the type of the component. It is set
   * when ComputeStrides() is invoked. */
  IODataType m_ComponentType;

  /** Big or Little Endian, and the type of the file. (May be ignored.) */
  ByteOrder      m_ByteOrder;
  FileType       m_FileType;

  /** Does the ImageIOBase object have enough info to be of use? */
  bool m_Initialized;

  /** Filename to read */
  std::string m_FileName;

  /** Stores the number of components per pixel. This will be 1 for 
   * grayscale images, 3 for RGBPixel images, and 4 for RGBPixelA images. */
  unsigned int m_NumberOfComponents;

  /** The number of independent dimensions in the image. */
  unsigned int m_NumberOfDimensions;

  /** The region to read or write. The region contains information about the
   * data within the region to read or write. */
  ImageIORegion m_IORegion;

  /** The array which stores the number of pixels in the x, y, z directions. */
  std::vector<unsigned int> m_Dimensions;

  /** The array which stores the spacing of pixels in the 
   * x, y, z directions. */
  std::vector<double> m_Spacing;

  /** The array which stores the origin of the image. */
  std::vector<double> m_Origin;

  /** Stores the number of bytes it takes to get to the next 'thing'
   * e.g. component, pixel, row, slice, etc. */
  std::vector<unsigned int> m_Strides;

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

  /** Compute the size (in bytes) of the pixel. For
   * example, and RGB pixel of unsigned char would have size 3 bytes. */
  virtual unsigned int GetPixelSize() const;

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a 
   * component size of 1 byte. This method can be invoked only after
   * the component type is set. */
  virtual unsigned int GetComponentSize() const;

  /** Convenient method for accessing number of bytes to get to the next pixel 
   * component. Returns m_Strides[0]. */
  unsigned int GetComponentStride() const;

  /** Convenient method for accessing the number of bytes to get to the 
   * next row. Returns m_Strides[2]. */
  unsigned int GetRowStride () const;

  /** Convenient method for accessing the number of bytes to get to the 
   * next slice. Returns m_Strides[3]. */
  unsigned int GetSliceStride () const;

  /** Convenient method to write a buffer as ASCII text. */
  void WriteBufferAsASCII(std::ostream& os, const void *buffer, IODataType ctype,
                          unsigned int numComp);

  /** Convenient method to read a buffer as ASCII text. */
  void ReadBufferAsASCII(std::istream& os, void *buffer, IODataType ctype,
                         unsigned int numComp);

private:
  ImageIOBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif // __itkImageIOBase_h
