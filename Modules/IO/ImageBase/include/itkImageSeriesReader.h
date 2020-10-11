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
#ifndef itkImageSeriesReader_h
#define itkImageSeriesReader_h
#include "ITKIOImageBaseExport.h"

#include "itkSize.h"
#include <vector>
#include <string>
#include "itkMetaDataDictionary.h"
#include "itkImageFileReader.h"

namespace itk
{
/** \brief Data source that reads image data from a series of disk files.
 *
 * This class builds an n-dimension image from multiple n-1
 * dimension image files. The files stored in a vector of strings
 * are read using the ImageFileReader. File format may vary between
 * the files, but the image data must have the same Size for all
 * dimensions.
 *
 * \sa GDCMSeriesFileNames
 * \sa NumericSeriesFileNames
 * \ingroup IOFilters
 *
 */

template <typename TOutputImage>
class ITK_TEMPLATE_EXPORT ImageSeriesReader : public ImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageSeriesReader);

  /** Standard class type aliases. */
  using Self = ImageSeriesReader;
  using Superclass = ImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageSeriesReader, ImageSource);

  /** The size of the output image. */
  using SizeType = typename TOutputImage::SizeType;

  /** The index of the output image. */
  using IndexType = typename TOutputImage::IndexType;

  /** The region of the output image. */
  using ImageRegionType = typename TOutputImage::RegionType;

  /** The pixel type of the output image. */
  using OutputImagePixelType = typename TOutputImage::PixelType;

  /** The pixel type of the output image. */
  using DictionaryType = MetaDataDictionary;
  using DictionaryRawPointer = MetaDataDictionary *;
  using DictionaryArrayType = std::vector<DictionaryRawPointer>;
  using DictionaryArrayRawPointer = const DictionaryArrayType *;

  using FileNamesContainer = std::vector<std::string>;

  /** Set the vector of strings that contains the file names. Files
   * are processed in sequential order. */
  void
  SetFileNames(const FileNamesContainer & name)
  {
    if (m_FileNames != name)
    {
      m_FileNames = name;
      this->Modified();
    }
  }

  const FileNamesContainer &
  GetFileNames() const
  {
    return m_FileNames;
  }

  /** Set the first file name to be processed. This deletes previous
   * filenames. */
  void
  SetFileName(std::string const & name)
  {
    m_FileNames.clear();
    m_FileNames.push_back(name);
    this->Modified();
  }

  /** Add a single filename to the list of files. */
  void
  AddFileName(std::string const & name)
  {
    m_FileNames.push_back(name);
    this->Modified();
  }

  /** ReverseOrderOn changes the order of travesal of the file names
   * from last to first */
  itkSetMacro(ReverseOrder, bool);
  itkGetConstMacro(ReverseOrder, bool);
  itkBooleanMacro(ReverseOrder);

  /** Do we want to force orthogonal direction cosines? On by default.
   * Turning it off enables proper reading of DICOM series with gantry tilt. */
  itkSetMacro(ForceOrthogonalDirection, bool);
  itkGetConstMacro(ForceOrthogonalDirection, bool);
  itkBooleanMacro(ForceOrthogonalDirection);

  /** Set/Get the ImageIO helper class. By default, the
   * ImageSeriesReader uses the factory mechanism of the
   * ImageFileReader to determine the file type. This method can be
   * used to specify which IO to use. */
  itkSetObjectMacro(ImageIO, ImageIOBase);
  itkGetModifiableObjectMacro(ImageIO, ImageIOBase);

  /** \brief Set/Get MetaDataDictionaryArrayUpdate enables the updating of
   * the MetaDataDictionaryArray.
   *
   * By default this is enabled. It may be advantageous to disable
   * this feature when streaming, if this array is not need, as it may
   * redure the number of times each file is read.
   *
   * /sa GetMetaDataDictionaryArray()
   */
  itkSetMacro(MetaDataDictionaryArrayUpdate, bool);
  itkGetConstMacro(MetaDataDictionaryArrayUpdate, bool);
  itkBooleanMacro(MetaDataDictionaryArrayUpdate);

  /** Prepare the allocation of the output image during the first back
   * propagation of the pipeline. */
  void
  GenerateOutputInformation() override;

  /** Give the reader a chance to indicate that it will produce more
   * output than it was requested to produce. ImageSeriesReader cannot
   * currently read a portion of an image (since the ImageIO objects
   * cannot read a portion of an image), so the ImageSeriesReader must
   * enlarge the RequestedRegion to the size of the image on disk. */
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  /** Get access to the Array of MetaDataDictionaries which are
   * updated in the GenerateData methods */
  DictionaryArrayRawPointer
  GetMetaDataDictionaryArray() const;

  /** Set the stream On or Off */
  itkSetMacro(UseStreaming, bool);
  itkGetConstReferenceMacro(UseStreaming, bool);
  itkBooleanMacro(UseStreaming);

  /** Set the relative threshold for issuing warnings about non-uniform sampling */
  itkSetMacro(SpacingWarningRelThreshold, double);
  itkGetConstMacro(SpacingWarningRelThreshold, double);

protected:
  ImageSeriesReader()
    : m_ImageIO(nullptr)

  {}
  ~ImageSeriesReader() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Does the real work. */
  void
  GenerateData() override;

  /** The image format, 0 will use the factory mechanism. */
  ImageIOBase::Pointer m_ImageIO;

  /** Select the traversal order. */
  bool m_ReverseOrder{ false };

  /** Do we want to force orthogonal direction cosines? */
  bool m_ForceOrthogonalDirection{ true };

  /** A list of filenames to be processed. */
  FileNamesContainer m_FileNames;

  /** The number of independent variables in the images that comprise
   *  the series. This is also used as the slice moving dimension
   *  index for the output image. That is for reading a series of 2D
   *  images into  a 3D image, the moving dimension index is 2.
   */
  unsigned int m_NumberOfDimensionsInImage{ 0 };

  /** Array of MetaDataDictionaries. This allows to hold information from the
   * ImageIO objects after reading every sub image in the series */
  DictionaryArrayType m_MetaDataDictionaryArray;

  bool m_UseStreaming{ true };

  bool m_SpacingDefined{ false };

  double m_SpacingWarningRelThreshold{ 1e-4 };

private:
  using ReaderType = ImageFileReader<TOutputImage>;

  int
  ComputeMovingDimensionIndex(ReaderType * reader);

  /** Modified time of the MetaDataDictionaryArray */
  TimeStamp m_MetaDataDictionaryArrayMTime;

  /** Indicated if the MMDA should be updated */
  bool m_MetaDataDictionaryArrayUpdate{ true };
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageSeriesReader.hxx"
#endif

#endif // itkImageSeriesReader_h
