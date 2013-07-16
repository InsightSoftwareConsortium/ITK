/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkRawImageIO_h
#define __itkRawImageIO_h

#include "itkImageIOBase.h"
#include "itkImageRegion.h"
#include "itkPixelTraits.h"
#include "itkByteSwapper.h"
#include "itkVersion.h"
#include <string>
#include <fstream>

namespace itk
{
/** \class RawImageIO
 *
 * \brief Read and write raw binary images.
 *
 * This class reads and writes 2D or 3D images. Because raw data has
 * little useful information built into the format,
 * the user is responsible for specifying pixel type,
 * dimensions, spacing, origin, header type, and so on. (Note: the
 * pixel type and image dimension is defined via the template parameter.)
 *
 * \sa ImageFileReader
 *
 * \ingroup IOFilters
 * \ingroup ITKIORAW
 */

template< class TPixel, unsigned int VImageDimension = 2 >
class RawImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef RawImageIO           Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RawImageIO, ImageIOBase);

  /** Pixel typedef support. Used to declare pixel type in filters
   * or other operations. */
  typedef TPixel PixelType;

  /** Type used for counting elements. */
  typedef Superclass::SizeValueType    SizeValueType;

  /** this type is used in case the pixel has several components */
  typedef typename PixelTraits< PixelType >::ValueType ComponentType;

  /** Helper class to swap bytes when necessary */
  typedef ByteSwapper< ComponentType > ByteSwapperType;

  /** If the data is in the tail end of the file, you want to
   * explicitly set the header size. */
  void SetHeaderSize(SizeValueType size);

  SizeValueType GetHeaderSize();

  /** The number of dimensions stored in a file. Defaults to two. If two,
   * each file contains one "slice". If three, each file will contain one
   * "volume". */
  itkSetMacro(FileDimensionality, unsigned long);
  itkGetConstMacro(FileDimensionality, unsigned long);

  /** The different types of ImageIO's can support data of varying
   * dimensionality. For example, some file formats are strictly 2D
   * while others can support 2D, 3D, or even n-D. This method returns
   * true/false as to whether the ImageIO can support the dimension
   * indicated. */
  virtual bool SupportsDimension(unsigned long dim)
  { return ( dim == m_FileDimensionality ); }

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIOBase can read the
   * file specified. Always returns false because we don't want to use
   * this reader unless absolutely sure (i.e., manual ImageIO creation). */
  virtual bool CanReadFile(const char *) { return false; }

  /** Binary files have no image information to read. This must be set by the
   * user of the class. */
  virtual void ReadImageInformation() { return; }

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer);

  /** Set/Get the Data mask. */
  itkGetConstReferenceMacro(ImageMask, unsigned short);
  void SetImageMask(unsigned long val)
  {
    if ( val == m_ImageMask ) { return; }
    m_ImageMask = ( (unsigned short)( val ) );
    this->Modified();
  }

  /** Read a file's header to determine image dimensions, etc. */
  virtual void ReadHeader( const std::string = std::string() ) {}

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Returns true if this ImageIO can write the specified file.
   * False is only returned when the file name is not specified. Otherwise
   * true is always returned. */
  virtual bool CanWriteFile(const char *);

  /** Binary files have no image information to read.  */
  virtual void WriteImageInformation(void) { return; }

  /** Writes the data to disk from the memory buffer provided. */
  virtual void Write(const void *buffer);

protected:
  RawImageIO();
  ~RawImageIO();
  void PrintSelf(std::ostream & os, Indent indent) const;

  //void ComputeInternalFileName(unsigned long slice);
  void OpenFileForReading(std::ifstream & is);

  void OpenFileForWriting(std::ofstream & os);

private:
  RawImageIO(const Self &);     //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  std::string m_InternalFileName;

  unsigned long  m_FileDimensionality;
  bool           m_ManualHeaderSize;
  SizeValueType  m_HeaderSize;
  unsigned short m_ImageMask;
};

template< class TPixel, unsigned int VImageDimension >
class RawImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef RawImageIOFactory< TPixel, VImageDimension > Self;
  typedef ObjectFactoryBase                            Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  /** Class methods used to interface with the registered factories. */
  const char * GetITKSourceVersion(void) const
  {
    return ITK_SOURCE_VERSION;
  }

  const char * GetDescription(void) const
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
  RawImageIOFactory() {}
  ~RawImageIOFactory() {}
  typedef RawImageIO< TPixel, VImageDimension > myProductType;
  const myProductType *m_MyProduct;

private:
  RawImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);    //purposely not implemented
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRawImageIO.hxx"
#endif

#endif
