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
#ifndef itkImageFileWriter_h
#define itkImageFileWriter_h
#include "ITKIOImageBaseExport.h"

#include "itkProcessObject.h"
#include "itkImageIOBase.h"
#include "itkMacro.h"

namespace itk
{
/** \brief Base exception class for IO problems during writing.
 *
 * \class ImageFileWriterException
 * \ingroup ITKIOImageBase
 */
class ITKIOImageBase_EXPORT ImageFileWriterException:public ExceptionObject
{
public:
  /** Run-time information. */
  itkTypeMacro(ImageFileWriterException, ExceptionObject);

  /** Constructor. */
  ImageFileWriterException(const char *file, unsigned int line,
                           const char *message = "Error in IO",
                           const char *loc = "Unknown"):
    ExceptionObject(file, line, message, loc)
  {}

  /** Constructor. */
  ImageFileWriterException(const std::string & file, unsigned int line,
                           const char *message = "Error in IO",
                           const char *loc = "Unknown"):
    ExceptionObject(file, line, message, loc)
  {}

  /** Has to have empty throw(). */
  virtual ~ImageFileWriterException() ITK_NOEXCEPT ITK_OVERRIDE;
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
 * \ingroup ITKIOImageBase
 *
 * \wiki
 * \wikiexample{IO/ImageFileWriter,Write an image}
 * \endwiki
 */
template< typename TInputImage >
class ITKIOImageBase_HIDDEN ImageFileWriter:public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageFileWriter            Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageFileWriter, ProcessObject);

  /** Some convenient typedefs. */
  typedef TInputImage                         InputImageType;
  typedef typename InputImageType::Pointer    InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType;
  typedef typename InputImageType::PixelType  InputImagePixelType;

  /** Set/Get the image input of this writer.  */
  using Superclass::SetInput;
  void SetInput(const InputImageType *input);

  const InputImageType * GetInput();

  const InputImageType * GetInput(unsigned int idx);

  /** Specify the name of the output file to write. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Set/Get the ImageIO helper class. Usually this is created via the object
   * factory mechanism that determines whether a particular ImageIO can
   * write a certain file. This method provides a way to get the ImageIO
   * instance that is created, or one can be manually set where the
   * IO factory mechanism may not work (for example, raw image files or
   * image files with non-standard filename suffix's.
   * If the user specifies the ImageIO, we assume she makes the
   * correct choice and will allow a file to be created regardless of
   * the file extension. If the factory has set the ImageIO, the
   * extension must be supported by the specified ImageIO. */
  void SetImageIO(ImageIOBase *io)
  {
    if ( this->m_ImageIO != io )
      {
      this->Modified();
      this->m_ImageIO = io;
      }
    m_FactorySpecifiedImageIO = false;
  }
  itkGetModifiableObjectMacro(ImageIO, ImageIOBase);

  /** A special version of the Update() method for writers.  It
   * invokes start and end events and handles releasing data. It
   * eventually calls GenerateData() which does the actual writing.
   * Note: the write method will write data specified by the
   * IORegion. If not set, then then the whole image is written.  Note
   * that the region will be cropped to fit the input image's
   * LargestPossibleRegion. */
  virtual void Write();

  /** Specify the region to write. If left ITK_NULLPTR, then the whole image
   * is written. */
  void SetIORegion(const ImageIORegion & region);

  const ImageIORegion & GetIORegion(void) const
  {
    return m_PasteIORegion;
  }

  /** Set/Get the number of pieces to divide the input.  The upstream pipeline
   * will try to be executed this many times. */
  itkSetMacro(NumberOfStreamDivisions, unsigned int);
  itkGetConstReferenceMacro(NumberOfStreamDivisions, unsigned int);

  /** Aliased to the Write() method to be consistent with the rest of the
   * pipeline. */
  virtual void Update() ITK_OVERRIDE
  {
    this->Write();
  }

  /** \brief Writes the entire image to file.
   *
   * Updates the pipeline, streaming it the NumberOfStreamDivisions times.
   * Existing PasteIORegion is reset.
   */
  virtual void UpdateLargestPossibleRegion() ITK_OVERRIDE
  {
    m_PasteIORegion = ImageIORegion(TInputImage::ImageDimension);
    m_UserSpecifiedIORegion = false;
    this->Write();
  }

  /** Set the compression On or Off */
  itkSetMacro(UseCompression, bool);
  itkGetConstReferenceMacro(UseCompression, bool);
  itkBooleanMacro(UseCompression);

  /** By default the MetaDataDictionary is taken from the input image and
   *  passed to the ImageIO. In some cases, however, a user may prefer to
   *  introduce her/his own MetaDataDictionary. This is often the case of
   *  the ImageSeriesWriter. This flag defined whether the MetaDataDictionary
   *  to use will be the one from the input image or the one already set in
   *  the ImageIO object. */
  itkSetMacro(UseInputMetaDataDictionary, bool);
  itkGetConstReferenceMacro(UseInputMetaDataDictionary, bool);
  itkBooleanMacro(UseInputMetaDataDictionary);

protected:
  ImageFileWriter();
  ~ImageFileWriter() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Does the real work. */
  virtual void GenerateData(void) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageFileWriter);

  std::string m_FileName;

  ImageIOBase::Pointer m_ImageIO;
  bool                 m_UserSpecifiedImageIO; // track whether the ImageIO
                                               // is user specified

  ImageIORegion m_PasteIORegion;
  unsigned int  m_NumberOfStreamDivisions;
  bool          m_UserSpecifiedIORegion;    // track whether the region
                                            // is user specified
  bool m_FactorySpecifiedImageIO;           //track whether the factory
                                            //  mechanism set the ImageIO
  bool m_UseCompression;
  bool m_UseInputMetaDataDictionary;        // whether to use the
                                            // MetaDataDictionary from the
                                            // input or not.
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageFileWriter.hxx"
#endif

#ifdef ITK_IO_FACTORY_REGISTER_MANAGER
#include "itkImageIOFactoryRegisterManager.h"
#endif

#endif // itkImageFileWriter_h
