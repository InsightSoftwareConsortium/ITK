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
#ifndef __itkImageFileReader_h
#define __itkImageFileReader_h

#include "itkImageIOBase.h"
#include "itkImageSource.h"
#include "itkMacro.h"
#include "itkImageRegion.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
/** \class ImageFileReaderException
 *
 * \brief Base exception class for IO conflicts.
 * \ingroup ITKIOImageBase
 */
class ITK_ABI_EXPORT ImageFileReaderException:public ExceptionObject
{
public:
  /** Run-time information. */
  itkTypeMacro(ImageFileReaderException, ExceptionObject);

  /** Constructor. */
  ImageFileReaderException(const char *file, unsigned int line,
                           const char *message = "Error in IO",
                           const char *loc = "Unknown"):
    ExceptionObject(file, line, message, loc)
  {}

  /** Constructor. */
  ImageFileReaderException(const std::string & file, unsigned int line,
                           const char *message = "Error in IO",
                           const char *loc = "Unknown"):
    ExceptionObject(file, line, message, loc)
  {}

  /** Has to have empty throw(). */
  virtual ~ImageFileReaderException() throw( )
  {}
};

/** \brief Data source that reads image data from a single file.
 *
 * This source object is a general filter to read data from
 * a variety of file formats. It works with a ImageIOBase subclass
 * to actually do the reading of the data. Object factory machinery
 * can be used to automatically create the ImageIOBase, or the
 * ImageIOBase can be manually created and set. Note that this
 * class reads data from a single file; if you wish to read data
 * from a series of files use ImageSeriesReader.
 *
 * TOutputImage is the type expected by the external users of the
 * filter. If data stored in the file is stored in a different format
 * then specified by TOutputImage, than this filter converts data
 * between the file type and the external expected type.  The
 * ConvertTraits template argument is used to do the conversion.
 *
 * A Pluggable factory pattern is used this allows different kinds of readers
 * to be registered (even at run time) without having to modify the
 * code in this class. Normally just setting the FileName with the
 * appropriate suffix is enough to get the reader to instantiate the
 * correct ImageIO and read the file properly. However, some files (like
 * raw binary format) have no accepted suffix, so you will have to
 * manually create the ImageIO instance of the write type.
 *
 * \sa ImageSeriesReader
 * \sa ImageIOBase
 *
 * \ingroup IOFilters
 *
 */
/** \class ImageFileReader
 * \ingroup ITKIOImageBase
 *
 * \wiki
 * \wikiexample{IO/ReadVectorImage,Read an image file with an unknown number of components}
 * \wikiexample{IO/ReadUnknownImageType,Read an image file without knowing its type before hand}
 * \wikiexample{IO/ImageFileReader,Read an image}
 * \endwiki
 */
template< class TOutputImage,
          class ConvertPixelTraits = DefaultConvertPixelTraits<
            typename TOutputImage::IOPixelType > >
class ITK_EXPORT ImageFileReader:public ImageSource< TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ImageFileReader             Self;
  typedef ImageSource< TOutputImage > Superclass;
  typedef SmartPointer< Self >        Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageFileReader, ImageSource);

  /** The size of the output image. */
  typedef typename TOutputImage::SizeType SizeType;

  /** The size of the output image. */
  typedef typename TOutputImage::IndexType IndexType;

  /** The region of the output image. */
  typedef typename TOutputImage::RegionType ImageRegionType;

  /** The pixel type of the output image. */
  typedef typename TOutputImage::InternalPixelType OutputImagePixelType;

  /** Specify the file to read. This is forwarded to the IO instance. */
  itkSetGetDecoratedInputMacro(FileName, std::string);

  /** Set/Get the ImageIO helper class. Often this is created via the object
   * factory mechanism that determines whether a particular ImageIO can
   * read a certain file. This method provides a way to get the ImageIO
   * instance that is created. Or you can directly specify the ImageIO
   * to use to read a particular file in case the factory mechanism will
   * not work properly (e.g., unknown or unusual extension). */
  void  SetImageIO(ImageIOBase *imageIO);
  itkGetModifiableObjectMacro(ImageIO, ImageIOBase);

  /** Prepare the allocation of the output image during the first back
   * propagation of the pipeline. */
  virtual void GenerateOutputInformation(void);

  /** Give the reader a chance to indicate that it will produce more
   * output than it was requested to produce. ImageFileReader cannot
   * currently read a portion of an image (since the ImageIO objects
   * cannot read a portion of an image), so the ImageFileReader must
   * enlarge the RequestedRegion to the size of the image on disk. */
  virtual void EnlargeOutputRequestedRegion(DataObject *output);

  /** Set the stream On or Off */
  itkSetMacro(UseStreaming, bool);
  itkGetConstReferenceMacro(UseStreaming, bool);
  itkBooleanMacro(UseStreaming);

protected:
  ImageFileReader();
  ~ImageFileReader();
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Convert a block of pixels from one type to another. */
  void DoConvertBuffer(void *buffer, size_t numberOfPixels);

  /** Test whether the given filename exist and it is readable, this
    * is intended to be called before attempting to use  ImageIO
    * classes for actually reading the file. If the file doesn't exist
    * or it is not readable, and exception with an approriate message
    * will be thrown. */
  void TestFileExistanceAndReadability();

  /** Does the real work. */
  virtual void GenerateData();

  ImageIOBase::Pointer m_ImageIO;

  bool m_UserSpecifiedImageIO; // keep track whether the
                               // ImageIO is user specified

  bool m_UseStreaming;

private:
  ImageFileReader(const Self &); //purposely not implemented
  void operator=(const Self &);  //purposely not implemented

  std::string m_ExceptionMessage;

  // The region that the ImageIO class will return when we ask to
  // produce the requested region.
  ImageIORegion m_ActualIORegion;
};
} //namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageFileReader.hxx"
#endif

#ifdef ITK_IO_FACTORY_REGISTER_MANAGER
#include "itkImageIOFactoryRegisterManager.h"
#endif

#endif // __itkImageFileReader_h
