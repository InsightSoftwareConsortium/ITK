/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileWriter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageFileWriter_h
#define __itkImageFileWriter_h

#include "itkProcessObject.h"
#include "itkImageIOBase.h"
#include "itkExceptionObject.h"
#include "itkSize.h"
#include "itkImageRegion.h"

namespace itk
{

/** \brief Base exception class for IO problems during writing. */
class ImageFileWriterException : public ExceptionObject 
{
public:
  /** Run-time information. */
  itkTypeMacro( ImageFileWriterException, ExceptionObject );

  /** Constructor. */
  ImageFileWriterException(char *file, unsigned int line, 
                           const char* message = "Error in IO") : 
    ExceptionObject(file, line)
    {
      SetDescription(message);
    }

  /** Constructor. */
  ImageFileWriterException(const std::string &file, unsigned int line, 
                           const char* message = "Error in IO") : 
    ExceptionObject(file, line)
    {
      SetDescription(message);
    }
};


/** \class ImageFileWriter
 * \brief The base class for all image data writers.
 *
 * ImageFileWriter is the base class for all Insight image data writers.
 * ImageFileWriter interfaces with an ImageIO class to write out its
 * data.
 *
 * A Pluggable factory pattern is used that allows different kinds of writers
 * to be registered (even at run time) without having to modify the
 * code in this class. You can either manually instantiate the ImageIO
 * object and associate it with the ImageFileWriter, or let the class
 * figure it out from the extension.
 *
 * \ingroup IOFilters 
 */
template <class TInputImage>
class ITK_EXPORT ImageFileWriter : public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageFileWriter              Self;
  typedef ProcessObject  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageFileWriter,ProcessObject);

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType; 
  typedef typename InputImageType::PixelType InputImagePixelType; 
  
  /** Set/Get the image input of this writer.  */
  void SetInput(InputImageType *input);
  InputImagePointer GetInput();
  InputImagePointer GetInput(unsigned int idx);
  
  /** Specify the name of the output file. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);
  
  /** Specify file prefix for the image file(s). You should specify either
   * a FileName or FilePrefix. Use FilePrefix if the data can be stored
   * in multiple files. */
  itkSetStringMacro(FilePrefix);
  itkGetStringMacro(FilePrefix);
  
  /** The sprintf format used to build filename from FilePrefix and number. */
  itkSetStringMacro(FilePattern);
  itkGetStringMacro(FilePattern);
  
  /** Set/Get the ImageIO helper class. Often this is created via the object
   * factory mechanism that determines whether a particular ImageIO can
   * write a certain file. This method provides a way to get the ImageIO 
   * instance that is created. */
  itkSetObjectMacro(ImageIO,ImageIOBase);
  itkGetObjectMacro(ImageIO,ImageIOBase);
  
  /** A special version of the Update() method for writers.  It
   * invokes start and end events and handles releasing data. It
   * eventually calls GenerateData() which does the actual writing.
   * Note: if the write method is called without arguments, then the
   * entire image is written. The Write(region) method writes the
   * specified region (if the ImageIO supports it). Note that the
   * region will be cropped to fit the input image's
   * LargestPossibleRegion. */
  virtual void Write();
  virtual void Write(const ImageIORegion& region);

protected:
  ImageFileWriter();
  ~ImageFileWriter();
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Does the real work. */
  void GenerateData();
  
private:
  ImageFileWriter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  std::string        m_FileName;
  std::string        m_FilePrefix;
  std::string        m_FilePattern;
  
  ImageIOBase::Pointer m_ImageIO;
  bool m_UserSpecifiedImageIO; //track whether the ImageIO is user specified
  
  ImageIORegion *m_Region;
  bool m_UserSpecifiedRegion; //track whether the region is user specified
  
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageFileWriter.txx"
#endif

#endif // __itkImageFileWriter_h
  
