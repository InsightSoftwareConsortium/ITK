/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSeriesWriter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageSeriesWriter_h
#define __itkImageSeriesWriter_h

#include "itkProcessObject.h"
#include "itkImageIOBase.h"
#include "itkFileIteratorBase.h"
#include "itkExceptionObject.h"
#include "itkSize.h"
#include "itkImageRegion.h"

namespace itk
{

/** \brief Base exception class for IO problems during writing. */
class ImageSeriesWriterException : public ExceptionObject 
{
public:
  /** Run-time information. */
  itkTypeMacro( ImageSeriesWriterException, ExceptionObject );

  /** Constructor. */
  ImageSeriesWriterException(char *file, unsigned int line, 
                           const char* message = "Error in IO") : 
    ExceptionObject(file, line)
    {
      SetDescription(message);
    }

  /** Constructor. */
  ImageSeriesWriterException(const std::string &file, unsigned int line, 
                           const char* message = "Error in IO") : 
    ExceptionObject(file, line)
    {
      SetDescription(message);
    }
};


/** \class ImageSeriesWriter
 * \brief Writes image data to a series of data files.
 *
 * ImageSeriesWriter writes its input data to a series of output files.
 * ImageSeriesWriter interfaces with the classes ImageIOBase and 
 * FileIteratorBase to write out the data. If you wish to write 
 * data into a single file, ImageFileWriter is easier to use.
 *
 * A pluggable factory pattern is used that allows different file formats
 * to be written (even at run time) without having to modify the
 * code in this class. You can either manually instantiate the ImageIO
 * object and associate it with the ImageSeriesWriter, or create a
 * FileIteratorBase subclass and set the SeriesFormat with a suitable
 * suffix (".png", ".jpg", etc). Of course the input to the to the
 * writer must be set as well.
 *
 * Note: the name of the output files is controlled by a subclass of 
 * FileIteratorBase. If an ImageIO is set, it will produce a file iterator.
 * However, the FileIterator can be manually set to control how the files
 * are named. If an ImageIO is not set, but a file iterator is set, then the
 * appropriate ImageIO (if possible) is determined from the SeriesFormat ivar
 * of the FileIterator.
 *
 * \sa ImageFileWriter
 * \sa ImageIOBase
 * \sa FileIteratorBase
 * \sa ImageSeriesReader
 *
 * \ingroup IOFilters 
 */
template <class TInputImage>
class ITK_EXPORT ImageSeriesWriter : public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageSeriesWriter         Self;
  typedef ProcessObject             Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageSeriesWriter,ProcessObject);

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType; 
  typedef typename InputImageType::PixelType InputImagePixelType; 
  
  /** Set/Get the image input of this writer.  */
  void SetInput(const InputImageType *input);
  const InputImageType *GetInput(void);
  const InputImageType *GetInput(unsigned int idx);
  
  /** The naming of the output files is controlled by using a subclass of 
   * FileIteratorBase (e.g., NumericSeriesFileIterator). */
  itkSetObjectMacro(FileIterator,FileIteratorBase);
  itkGetObjectMacro(FileIterator,FileIteratorBase);
  
  /** Set/Get the ImageIO helper class. Usually this is created via the object
   * factory mechanism that determines whether a particular ImageIO can
   * write a certain file. This method provides a way to get the ImageIO 
   * instance that is created, or to manually set one when the factory
   * mechanism may not work (e.g., for raw files or for non-standard
   * file suffix). */
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

  /** Aliased to the Write() method to be consistent with the rest of the
   * pipeline. */
  virtual void Update()
    {this->Write();}
  
protected:
  ImageSeriesWriter();
  ~ImageSeriesWriter();
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Does the real work. */
  void GenerateData(void);

  /** Used to produce the names of the output files in the series */
  FileIteratorBase::Pointer m_FileIterator;
  
  ImageIOBase::Pointer m_ImageIO;
  bool m_UserSpecifiedImageIO; //track whether the ImageIO is user specified
  
  ImageIORegion m_IORegion;
  bool m_UserSpecifiedIORegion; //track whether the region is user specified
  
  /** Specify the region to write. If left NULL, then the whole image
   * is written. (Left protected for now to insure that the user doesn't
   * try to use writing of partial images just yet since it's really not
   * working at this time.) */
  void SetIORegion(const ImageIORegion & region);
  const ImageIORegion & GetIORegion() const;

private:
  ImageSeriesWriter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSeriesWriter.txx"
#endif

#endif // __itkImageSeriesWriter_h
  
