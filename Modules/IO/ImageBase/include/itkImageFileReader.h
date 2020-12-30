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
#ifndef itkImageFileReader_h
#define itkImageFileReader_h
#include "itkImageFileReaderException.h"

#include "ITKIOImageBaseExport.h"

#include "itkImageIOBase.h"
#include "itkImageSource.h"
#include "itkMacro.h"
#include "itkImageRegion.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{

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
 * `ConvertPixelTraits` template parameter is used to do the conversion.
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
 * \sphinx
 * \sphinxexample{Core/Common/ReadWriteVectorImage,Read Write Vector Image}
 * \sphinxexample{IO/ImageBase/ReadUnknownImageType, Read Unknown Image Type}
 * \sphinxexample{IO/ImageBase/ReadAnImage,Read An Image}
 * \endsphinx
 */
template <typename TOutputImage,
          typename ConvertPixelTraits = DefaultConvertPixelTraits<typename TOutputImage::IOPixelType>>
class ITK_TEMPLATE_EXPORT ImageFileReader : public ImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageFileReader);

  /** Standard class type aliases. */
  using Self = ImageFileReader;
  using Superclass = ImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageFileReader, ImageSource);

  /** The size of the output image. */
  using SizeType = typename TOutputImage::SizeType;

  /** The size of the output image. */
  using IndexType = typename TOutputImage::IndexType;

  /** The region of the output image. */
  using ImageRegionType = typename TOutputImage::RegionType;

  /** The pixel type of the output image. */
  using OutputImagePixelType = typename TOutputImage::InternalPixelType;

  /** Specify the file to read. This is forwarded to the IO instance. */
  itkSetGetDecoratedInputMacro(FileName, std::string);

  /** Set/Get the ImageIO helper class. Often this is created via the object
   * factory mechanism that determines whether a particular ImageIO can
   * read a certain file. This method provides a way to get the ImageIO
   * instance that is created. Or you can directly specify the ImageIO
   * to use to read a particular file in case the factory mechanism will
   * not work properly (e.g., unknown or unusual extension). */
  void
  SetImageIO(ImageIOBase * imageIO);
  itkGetModifiableObjectMacro(ImageIO, ImageIOBase);

  /** Set the stream On or Off */
  itkSetMacro(UseStreaming, bool);
  itkGetConstReferenceMacro(UseStreaming, bool);
  itkBooleanMacro(UseStreaming);

protected:
  ImageFileReader();
  ~ImageFileReader() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Convert a block of pixels from one type to another. */
  void
  DoConvertBuffer(void * inputData, size_t numberOfPixels);

  /** Test whether the given filename exist and it is readable, this
   * is intended to be called before attempting to use  ImageIO
   * classes for actually reading the file. If the file doesn't exist
   * or it is not readable, and exception with an appropriate message
   * will be thrown. */
  void
  TestFileExistanceAndReadability();

  /** Prepare the allocation of the output image during the first back
   * propagation of the pipeline. */
  void
  GenerateOutputInformation() override;

  /** Give the reader a chance to indicate that it will produce more
   * output than it was requested to produce. ImageFileReader cannot
   * currently read a portion of an image (since the ImageIO objects
   * cannot read a portion of an image), so the ImageFileReader must
   * enlarge the RequestedRegion to the size of the image on disk. */
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  /** Does the real work. */
  void
  GenerateData() override;

  ImageIOBase::Pointer m_ImageIO;

  bool m_UserSpecifiedImageIO; // keep track whether the
                               // ImageIO is user specified

  bool m_UseStreaming;

private:
  std::string m_ExceptionMessage;

  // The region that the ImageIO class will return when we ask to
  // produce the requested region.
  ImageIORegion m_ActualIORegion;
};


/** Convenience function for reading an image.
 *
 * `TOutputImage` is the expected output image type, and the optional
 * `ConvertPixelTraits` template parameter is used to do the conversion,
 * as specified by ImageFileReader.
 *
 * The function reads the image from the specified file, and returns the
 * image that it has read.
 * */
template <typename TOutputImage,
          typename ConvertPixelTraits = DefaultConvertPixelTraits<typename TOutputImage::IOPixelType>>
typename TOutputImage::Pointer
ReadImage(const std::string & filename)
{
  const auto reader = ImageFileReader<TOutputImage, ConvertPixelTraits>::New();
  reader->SetFileName(filename);
  reader->Update();
  return reader->GetOutput();
}


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageFileReader.hxx"
#endif

#ifdef ITK_IO_FACTORY_REGISTER_MANAGER
#  include "itkImageIOFactoryRegisterManager.h"
#endif

#endif // itkImageFileReader_h
