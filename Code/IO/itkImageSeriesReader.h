/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSeriesReader.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageSeriesReader_h
#define __itkImageSeriesReader_h

#include "itkImageIOBase.h"
#include "itkImageSource.h"
#include "itkExceptionObject.h"
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkDefaultConvertPixelTraits.h"

namespace itk
{

/** \brief Base exception class for IO conflicts. */
class ImageSeriesReaderException : public ExceptionObject 
{
public:
  /** Run-time information. */
  itkTypeMacro( ImageSeriesReaderException, ExceptionObject );

  /** Constructor. */
  ImageSeriesReaderException(char *file, unsigned int line, 
                           const char* message = "Error in IO") : 
    ExceptionObject(file, line)
    {
      SetDescription(message);
    }

  /** Constructor. */
  ImageSeriesReaderException(const std::string &file, unsigned int line, 
                           const char* message = "Error in IO") : 
    ExceptionObject(file, line)
    {
      SetDescription(message);
    }
};


/** \brief Data source that reads image data from a series of disk files.
 *
 * This source object is a general filter that reads image data from a
 * variety of file formats and assumes that the data is stored in a
 * series of files (the series can be one file long). It works with
 * the ImageIOBase class to actually do the reading of the
 * data. Object factory machinery can be used to automatically create
 * the ImageIOBase, or the ImageIOBase can be manually created and
 * set. The format of the series is controlled with a FileIteratorBase.
 * This class typically works in conjunction with the ImageIO class
 * (ImageIO provides an instance of FileIteratorBase to use for determing
 * the names of files in a series; however the FileIterator subclass can
 * also be manually set). 
 *
 * TOutputImage is the type expected by the external users of the
 * filter. If data stored in the file is stored in a different format
 * then specified by TOutputImage, than this filter converts data 
 * between the file type and the external expected type.  The 
 * ConvertTraits template argument is used to do the conversion.
 *
 * A Pluggable factory pattern is used this allows different kinds of readers
 * to be registered (even at run time) without having to modify the
 * code in this class.
 *
 * \sa ImageFileReader
 * \sa ImageIOBase
 * \sa FileIteratorBase
 * \sa ImageSeriesWriter
 *
 * \ingroup IOFilters
 *
 */
template <class TOutputImage,
  class ConvertPixelTraits = 
DefaultConvertPixelTraits< ITK_TYPENAME TOutputImage::PixelType> >
class ITK_EXPORT ImageSeriesReader : public ImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageSeriesReader          Self;
  typedef ImageSource<TOutputImage>  Superclass;
  typedef SmartPointer<Self>         Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageSeriesReader, ImageSource);

  /** The size of the output image. */
  typedef typename TOutputImage::SizeType  SizeType;

  /** The region of the output image. */
  typedef typename TOutputImage::RegionType  ImageRegionType;

  /** The pixel type of the output image. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;
  
  /** The naming of the input files is controlled by using a subclass of 
   * FileIteratorBase (e.g., NumericSeriesFileIterator). */
  itkSetObjectMacro(FileIterator,FileIteratorBase);
  itkGetObjectMacro(FileIterator,FileIteratorBase);
  
  /** Set/Get the ImageIO helper class. Often this is created via the object
   * factory mechanism that determines whether a particular ImageIO can
   * read a certain file. This method provides a way to get the ImageIO 
   * instance that is created. Or you can directly specify the ImageIO
   * to use to read a particular file in case the factory mechanism will
   * not work properly (e.g., unknown or unusual extension). */
  itkSetObjectMacro(ImageIO,ImageIOBase);
  itkGetObjectMacro(ImageIO,ImageIOBase);
  
  /** Prepare the allocation of the output image during the first back
   * propagation of the pipeline. */
  virtual void GenerateOutputInformation(void);

  /** Give the reader a chance to indicate that it will produce more
   * output than it was requested to produce. ImageSeriesReader cannot
   * currently read a portion of an image (since the ImageIO objects
   * cannot read a portion of an image), so the ImageSeriesReader must
   * enlarge the RequestedRegion to the size of the image on disk. */
  virtual void EnlargeOutputRequestedRegion(DataObject *output);
  
protected:
  ImageSeriesReader();
  ~ImageSeriesReader();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Convert a block of pixels from one type to another. */
  void DoConvertBuffer(void* buffer, unsigned long numberOfPixels);

  /** Does the real work. */
  virtual void GenerateData();

  ImageIOBase::Pointer m_ImageIO;
  bool m_UserSpecifiedImageIO; //track whether the ImageIO is user specified

  /** Used to produce the names of the files in the series. */
  FileIteratorBase::Pointer m_FileIterator;
  
private:
  ImageSeriesReader(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};


} //namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSeriesReader.txx"
#endif

#endif // __itkImageSeriesReader_h

