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
#include "itkImageIORegion.h"

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
 * \brief Writes image data to a single file.
 *
 * ImageFileWriter writes its input data to a single output file.
 * ImageFileWriter interfaces with an ImageIO class to write out the
 * data. If you wish to write data into a series of files (e.g., a
 * slice per file) use ImageSeriesWriter.
 *
 * A pluggable factory pattern is used that allows different kinds of writers
 * to be registered (even at run time) without having to modify the
 * code in this class. You can either manually instantiate the ImageIO
 * object and associate it with the ImageFileWriter, or let the class
 * figure it out from the extension. Normally just setting the filename
 * with a suitable suffix (".png", ".jpg", etc) and setting the input 
 * to the writer is enough to get the writer to work properly.
 *
 * \sa ImageSeriesReader
 * \sa ImageIOBase
 *
 * \ingroup IOFilters 
 */
template <class TInputImage>
class ITK_EXPORT ImageFileWriter : public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageFileWriter           Self;
  typedef ProcessObject             Superclass;
  typedef SmartPointer<Self>        Pointer;
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
  void SetInput(const InputImageType *input);
  const InputImageType * GetInput(void);
  const InputImageType * GetInput(unsigned int idx);
  
  /** Specify the name of the output file to write. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);
  
  /** Set/Get the ImageIO helper class. Usually this is created via the object
   * factory mechanism that determines whether a particular ImageIO can
   * write a certain file. This method provides a way to get the ImageIO 
   * instance that is created, or one can be manually set where the
   * IO factory mechanism may not work (for example, raw image files or
   * image files with non-standard filename suffix's. */
  itkSetObjectMacro(ImageIO,ImageIOBase);
  itkGetObjectMacro(ImageIO,ImageIOBase);
  
  /** A special version of the Update() method for writers.  It
   * invokes start and end events and handles releasing data. It
   * eventually calls GenerateData() which does the actual writing.
   * Note: the write method will write data specified by the
   * IORegion. If not set, then then the whole image is written.  Note
   * that the region will be cropped to fit the input image's
   * LargestPossibleRegion. */
  virtual void Write(void);

  /** Specify the region to write. If left NULL, then the whole image
   * is written. */
  void SetIORegion(const ImageIORegion & region);
  itkGetConstMacro( IORegion, ImageIORegion );

  /** Aliased to the Write() method to be consistent with the rest of the
   * pipeline. */
  virtual void Update()
  {this->Write();}
  
protected:
  ImageFileWriter();
  ~ImageFileWriter();
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Does the real work. */
  void GenerateData(void);
  
private:
  ImageFileWriter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  std::string        m_FileName;
  
  ImageIOBase::Pointer m_ImageIO;
  bool m_UserSpecifiedImageIO; //track whether the ImageIO is user specified
  
  ImageIORegion m_IORegion;
  bool m_UserSpecifiedIORegion; //track whether the region is user specified
  
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageFileWriter.txx"
#endif

#endif // __itkImageFileWriter_h
  
