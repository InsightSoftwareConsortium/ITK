/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileIOToImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFileIOToImageFilter_h
#define __itkFileIOToImageFilter_h

#include "itkImageIO.h"
#include "itkImageSource.h"
#include "itkImage.h"
#include "itkExceptionObject.h"

namespace itk
{

/** \brief Base exception class for IO conflicts
 *
 * \deprecated Please refer to Exceptions defined in ImageFileReader and ImageFileWriter
 * \ingroup IOFilters
 * \ingroup Exceptions
 * \ingroup Deprecated
 */
class FileIOException : public ExceptionObject 
{
public:
  itkTypeMacro( FileIOException, ExceptionObject );

  FileIOException(const char *file, unsigned int line) : ExceptionObject(file, line)
  {
    SetDescription("Problem during File IO");
  }

  FileIOException(const std::string &file, unsigned int line) : ExceptionObject(file, line)
  {
    SetDescription("Problem during File IO");
  }

  /** Copy constructor. Needed to ensure the exception object can be copied. */
  FileIOException(const FileIOException &orig) : ExceptionObject(orig)
    {
    }
};

/** \brief Filter to convert input data to a particular internal type
 *
 *  TOutputImage is the type expected by the external users of the 
 *  filter. Data comming from a file can be stored in any other format
 *  (or type) this filter converts data between the file type and the
 *  external expected type.
 *
 *  The Pluggable factory pattern is used. Different kinds of readers
 *  can be registered (even at run time) without having to modify the
 *  code in this class. (todo: how to use the pluggable)
 *
 *  \deprecated Use itk::ImageFileFilter instead
 *  \ingroup IOFilters Deprecated
 */
template <class TOutputImage>
class ITK_EXPORT FileIOToImageFilter : public ImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef FileIOToImageFilter         Self;
  typedef ImageSource<TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileIOToImageFilter, ImageSource);

  /** Typedef for Size. */
  typedef Size<TOutputImage::ImageDimension>  Size;

  /** Typedef for Region. */
  typedef ImageRegion<TOutputImage::ImageDimension>  Region;

  /** Specify the file to load. This is forwarded to the IO instance. 
   * Either the FileName or FilePrefix plus pattern are used to read
   * files. */
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

  /** Get the ImageIO helper class. Often this is created via the object
   * factory mechanism which keys off of the filename extension. This
   * method provides a way to get the ImageIO instance that is 
   * created. */
  itkGetObjectMacro(IO,ImageIO);

  /** Set the ImageIO helper class. Often this is created via the object
   * factory mechanism which keys off of the filename extension, so you 
   * don't have to specify it. This method is provided so that you can 
   * manually specify the IO helper class. */
  itkSetObjectMacro(IO,ImageIO);

  /** Cause the filter to read data. */
  virtual void Update();

protected:
  FileIOToImageFilter();
  ~FileIOToImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData();

private:
  FileIOToImageFilter(const Self&); //purposely not implemented
  void operator= (const Self&); //purposely not implemented

  ImageIO::Pointer m_IO;
  LightObject::Pointer m_LightObjectIO;

  std::string m_FileName;
  std::string m_FilePrefix;
  std::string m_FilePattern;

};

} //namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFileIOToImageFilter.txx"
#endif

#endif // __itkFileIOToImageFilter_h
