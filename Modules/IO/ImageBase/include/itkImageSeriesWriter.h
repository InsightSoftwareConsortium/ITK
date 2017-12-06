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
#ifndef itkImageSeriesWriter_h
#define itkImageSeriesWriter_h
#include "ITKIOImageBaseExport.h"

#include "itkImageRegion.h"
#include "itkImageFileWriter.h"
#include <vector>
#include <string>

namespace itk
{
/** \class ImageSeriesWriterException
 *  \brief Base exception class for IO problems during writing.
 * \ingroup ITKIOImageBase
 */
class ITKIOImageBase_EXPORT ImageSeriesWriterException:public ExceptionObject
{
public:
  /** Has to have empty throw(). */
  virtual ~ImageSeriesWriterException() ITK_NOEXCEPT ITK_OVERRIDE;

  /** Run-time information. */
  itkTypeMacro(ImageSeriesWriterException, ExceptionObject);

  /** Constructor. */
  ImageSeriesWriterException(char *file, unsigned int line,
                             const char *message = "Error in IO"):
    ExceptionObject(file, line)
  {
    SetDescription(message);
  }

  /** Constructor. */
  ImageSeriesWriterException(const std::string & file, unsigned int line,
                             const char *message = "Error in IO"):
    ExceptionObject(file, line)
  {
    SetDescription(message);
  }
};

/** \class ImageSeriesWriter
 * \brief Writes image data to a series of data files.
 *
 * ImageSeriesWriter writes its input data to a series of output files.
 * The writer is templated over an input image type and an output
 * image type. Usually, the output image type will have fewer
 * dimensions than the input image type. Each file has a name created
 * using the SeriesFormat. This string is used as a sprintf argument
 * to build a filename. The string should contain zero or one  "%d" or
 * equivalent. The "%d" is an incremental file number that starts at
 * StartIndex and is incremented by IncrementIndex.
 * Since this writer uses an internal instance of an ImageFileWriter,
 * the type of file is determined by either the file extension or an
 * ImageIO class if specified.
 *
 * \sa ImageFileWriter
 * \sa ImageIOBase
 * \sa ImageSeriesReader
 *
 * \ingroup IOFilters
 * \ingroup ITKIOImageBase
 */
template< typename TInputImage, typename TOutputImage >
class ITKIOImageBase_HIDDEN ImageSeriesWriter:public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageSeriesWriter          Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageSeriesWriter, ProcessObject);

  /** Some convenient typedefs. */
  typedef TInputImage                          InputImageType;
  typedef typename InputImageType::RegionType  InputImageRegionType;
  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef ImageFileWriter< TOutputImage >      WriterType;
  typedef std::vector< std::string >           FileNamesContainer;

  /** The pixel type of the output image. */
  typedef MetaDataDictionary                  DictionaryType;
  typedef MetaDataDictionary *                DictionaryRawPointer;
  typedef std::vector< DictionaryRawPointer > DictionaryArrayType;
  typedef const DictionaryArrayType *         DictionaryArrayRawPointer;

  /** Set/Get the image input of this writer.  */
  using Superclass::SetInput;
  void SetInput(const InputImageType *input);

  const InputImageType * GetInput();

  const InputImageType * GetInput(unsigned int idx);

  /** Set/Get the ImageIO helper class. Usually this is created via
   * the object factory mechanism that determines whether a particular
   * ImageIO can write a certain file. This method provides a way to
   * get the ImageIO instance that is created, or to manually set one
   * when the factory mechanism may not work (e.g., for raw files or
   * for non-standard file suffix). */
  itkSetObjectMacro(ImageIO, ImageIOBase);
  itkGetModifiableObjectMacro(ImageIO, ImageIOBase);

  /** A special version of the Update() method for writers.  It
   * invokes start and end events and handles releasing data. It
   * eventually calls GenerateData() which does the actual writing.
   * The whole image is written. */
  virtual void Write();

  /** Aliased to the Write() method to be consistent with the rest of the
   * pipeline. */
  virtual void Update() ITK_OVERRIDE
  {
    this->Write();
  }

  /** Use this method to set the starting index of the series.
   * The default value is 1. */
  itkSetMacro(StartIndex, SizeValueType);
  itkGetConstMacro(StartIndex, SizeValueType);

  /** Set the increment of the index of the series. The
   * default value is 1.  */
  itkSetMacro(IncrementIndex, SizeValueType);
  itkGetConstMacro(IncrementIndex, SizeValueType);

  /** The format string used to generate each filename in the
   * series. The filename is built with sprintf(filename, SeriesFormat,
   * number) where number starts at StartIndex and is incremented by
   * IncrementIndex. */
  itkSetStringMacro(SeriesFormat);
  itkGetStringMacro(SeriesFormat);

  /** Set/Get the vector of strings that contains the file names. Files
   *  are processed in sequential order. */
  void SetFileNames(const FileNamesContainer & name)
  {
    if ( m_FileNames != name )
      {
      m_FileNames = name;
      this->Modified();
      }
  }

  const FileNamesContainer & GetFileNames() const
  {
    return m_FileNames;
  }

  /** Set the first file name to be processed. This deletes previous
   * filenames. */
  void SetFileName(std::string const & name)
  {
    m_FileNames.clear();
    m_FileNames.push_back(name);
    this->Modified();
  }

  /** Add a single filename to the list of files. To add a vector of
   * filenames, use the AddFileNames method. */
  void AddFileName(std::string const & name)
  {
    m_FileNames.push_back(name);
    this->Modified();
  }

  /** Set the array of MetaDataDictionaries this is an optinal entry,
   *  mostly intended to be used when writing DICOM slices.  */
  itkSetMacro(MetaDataDictionaryArray, DictionaryArrayRawPointer);

  /** Set the compression On or Off */
  itkSetMacro(UseCompression, bool);
  itkGetConstReferenceMacro(UseCompression, bool);
  itkBooleanMacro(UseCompression);

protected:
  ImageSeriesWriter();
  ~ImageSeriesWriter() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Does the real work. */
  virtual void GenerateData(void) ITK_OVERRIDE;

  /** Transition method used for DEPRECATING old functionality.
   *  This method should be removed after release ITK 1.8 */
  void GenerateNumericFileNamesAndWrite();

  ImageIOBase::Pointer m_ImageIO;

  //track whether the ImageIO is user specified
  bool m_UserSpecifiedImageIO;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageSeriesWriter);

  /** A list of filenames to be processed. */
  FileNamesContainer m_FileNames;

  /** These variables are used for generating filenames using a numeric
   * approach This functionality is being DEPRECATED since it belongs to a
   * NumericSeriesFileNames class. Removing this functionality from here allows
   * to use additional SeriesFileNames such as the DICOM filenames generators.
   * */
  std::string   m_SeriesFormat;
  SizeValueType m_StartIndex;
  SizeValueType m_IncrementIndex;

  bool m_UseCompression;

  /** Array of MetaDataDictionary used for passing information to each slice */
  DictionaryArrayRawPointer m_MetaDataDictionaryArray;

  // These two methods provide now a common implementation for the
  // GenerateNumericFileNamesAndWrite() and avoid the duplication of code that
  // was leaving one of the code branches out of date.
  void GenerateNumericFileNames();

  void WriteFiles();
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSeriesWriter.hxx"
#endif

#endif // itkImageSeriesWriter_h
