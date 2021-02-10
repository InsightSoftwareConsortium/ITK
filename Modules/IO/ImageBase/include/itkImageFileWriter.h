/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkMetaProgrammingLibrary.h"

namespace itk
{
/** \brief Base exception class for IO problems during writing.
 *
 * \class ImageFileWriterException
 * \ingroup ITKIOImageBase
 */
class ITKIOImageBase_EXPORT ImageFileWriterException : public ExceptionObject
{
public:
  /** Run-time information. */
  itkTypeMacro(ImageFileWriterException, ExceptionObject);

  /** Constructor. */
  ImageFileWriterException(const char * file,
                           unsigned int line,
                           const char * message = "Error in IO",
                           const char * loc = "Unknown")
    : ExceptionObject(file, line, message, loc)
  {}

  /** Constructor. */
  ImageFileWriterException(const std::string & file,
                           unsigned int        line,
                           const char *        message = "Error in IO",
                           const char *        loc = "Unknown")
    : ExceptionObject(file, line, message, loc)
  {}

  /** Has to have empty throw(). */
  ~ImageFileWriterException() noexcept override;
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
 * \sphinx
 * \sphinxexample{IO/ImageBase/WriteAnImage,Write An image}
 * \endsphinx
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT ImageFileWriter : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageFileWriter);

  /** Standard class type aliases. */
  using Self = ImageFileWriter;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageFileWriter, ProcessObject);

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  /** Set/Get the image input of this writer.  */
  using Superclass::SetInput;
  void
  SetInput(const InputImageType * input);

  const InputImageType *
  GetInput();

  const InputImageType *
  GetInput(unsigned int idx);

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
  void
  SetImageIO(ImageIOBase * io)
  {
    if (this->m_ImageIO != io)
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
  virtual void
  Write();

  /** Specify the region to write. If left nullptr, then the whole image
   * is written. */
  void
  SetIORegion(const ImageIORegion & region);

  const ImageIORegion &
  GetIORegion() const
  {
    return m_PasteIORegion;
  }

  /** Set/Get the number of pieces to divide the input.  The upstream pipeline
   * will try to be executed this many times. */
  itkSetMacro(NumberOfStreamDivisions, unsigned int);
  itkGetConstReferenceMacro(NumberOfStreamDivisions, unsigned int);

  /** Aliased to the Write() method to be consistent with the rest of the
   * pipeline. */
  void
  Update() override
  {
    this->Write();
  }

  /** \brief Writes the entire image to file.
   *
   * Updates the pipeline, streaming it the NumberOfStreamDivisions times.
   * Existing PasteIORegion is reset.
   */
  void
  UpdateLargestPossibleRegion() override
  {
    m_PasteIORegion = ImageIORegion(TInputImage::ImageDimension);
    m_UserSpecifiedIORegion = false;
    this->Write();
  }

  /** Set the compression On or Off */
  itkSetMacro(UseCompression, bool);
  itkGetConstReferenceMacro(UseCompression, bool);
  itkBooleanMacro(UseCompression);

  /** Set the compression level. \sa ImageIOBase for details.
   * Set to a negative number to use ImageIO's default compression level. */
  itkSetMacro(CompressionLevel, int);
  itkGetConstReferenceMacro(CompressionLevel, int);

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
  ImageFileWriter() = default;
  ~ImageFileWriter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Does the real work. */
  void
  GenerateData() override;

private:
  std::string m_FileName;

  ImageIOBase::Pointer m_ImageIO;
  bool                 m_UserSpecifiedImageIO{ false };

  ImageIORegion m_PasteIORegion{ TInputImage::ImageDimension };
  unsigned int  m_NumberOfStreamDivisions{ 1 };
  bool          m_UserSpecifiedIORegion{ false };

  bool m_FactorySpecifiedImageIO{ false }; // did factory mechanism set the ImageIO?
  bool m_UseCompression{ false };
  int  m_CompressionLevel{ -1 };
  bool m_UseInputMetaDataDictionary{ true };
};


/** Convenience function for writing an image.
 *
 * The image parameter may be a either SmartPointer or a raw pointer and const or non-const.
 * */
template <typename TImagePointer>
ITK_TEMPLATE_EXPORT void
WriteImage(TImagePointer && image, const std::string & filename, bool compress = false)
{
  using NonReferenceImagePointer = typename std::remove_reference<TImagePointer>::type;
  static_assert(std::is_pointer<NonReferenceImagePointer>::value ||
                  mpl::IsSmartPointer<NonReferenceImagePointer>::Value,
                "WriteImage requires a raw pointer or SmartPointer.");

  using ImageType = typename std::remove_const<typename std::remove_reference<decltype(*image)>::type>::type;
  auto writer = ImageFileWriter<ImageType>::New();
  writer->SetInput(image);
  writer->SetFileName(filename);
  writer->SetUseCompression(compress);
  writer->Update();
}

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageFileWriter.hxx"
#endif

#ifdef ITK_IO_FACTORY_REGISTER_MANAGER
#  include "itkImageIOFactoryRegisterManager.h"
#endif

#endif // itkImageFileWriter_h
