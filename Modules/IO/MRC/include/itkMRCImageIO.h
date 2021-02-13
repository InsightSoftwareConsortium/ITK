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
#ifndef itkMRCImageIO_h
#define itkMRCImageIO_h
#include "ITKIOMRCExport.h"
#include "itkStreamingImageIOBase.h"
#include "itkMRCHeaderObject.h"
#include <numeric>

namespace itk
{
/**
 *\class MRCImageIO
 *
 *  \brief An ImageIO class to read the MRC file format.
 * The MRC file format frequently has the extension ".mrc" or
 * ".rec". It is used frequently for electron microscopy and is an
 * emerging standard for cryo-electron tomography and molecular
 * imaging. The format is used to represent 2D, 3D images along with
 * 2D tilt series for tomography.
 *
 * The header of the file can contain important information which can
 * not be represented in an Image. Therefor the header is placed into
 * the MetaDataDictionary of "this". The key to access this is
 * MetaDataHeaderName ( fix me when renamed ).
 * \sa MRCHeaderObject MetaDataDictionary
 *
 * This implementation is designed to support IO Streaming of
 * arbitrary regions.
 *
 * As with all ImageIOs this class is designed to work with
 * ImageFileReader and ImageFileWriter, so its direct use is
 * discouraged.
 *
 * This code was contributed in the Insight Journal paper:
 * "A Streaming IO Base Class and Support for Streaming the MRC and VTK File Format"
 * by Lowekamp B., Chen D.
 * https://www.insight-journal.org/browse/publication/729
 *
 * \sa ImageFileWriter ImageFileReader ImageIOBase
 * \ingroup ITKIOMRC
 */
class ITKIOMRC_EXPORT MRCImageIO : public StreamingImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MRCImageIO);

  /** Standard class type aliases. */
  using Self = MRCImageIO;
  using Superclass = StreamingImageIOBase;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MRCImageIO, StreamingImageIOBase);

  // we don't use this method
  void
  WriteImageInformation() override
  {}

  //-------- This part of the interface deals with reading data. ------

  // See super class for documentation
  bool
  CanReadFile(const char *) override;

  // See super class for documentation
  void
  ReadImageInformation() override;

  // See super class for documentation
  void
  Read(void * buffer) override;

  // -------- This part of the interfaces deals with writing data. -----

  /** \brief Returns true if this ImageIO can write the specified
   * file.
   *
   * The methods verifies that the file extension is known to be
   * supported by this class.
   */
  bool
  CanWriteFile(const char *) override;

  // see super class for documentation
  void
  Write(const void * buffer) override;

  /** \todo Move to itkIOCommon with the other MetaDataDictionary
   * keys, likely rename the symbol to something like
   * ITK_MRCHHeader. (remember to fix class doc too)
   */
  static const char * m_MetaDataHeaderName;

protected:
  MRCImageIO();
  // ~MRCImageIO(); // default works
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Overloaded to return the actually header size of the file
   * specified. The header must be read before this methods is
   * called.
   */
  SizeType
  GetHeaderSize() const override;

private:
  // internal methods to update the min and max in the header based on
  // the data, in the image buffer to be written
  template <typename TPixelType>
  void
  UpdateHeaderWithMinMaxMean(const TPixelType * bufferBegin);

  void
  UpdateHeaderWithMinMaxMean(const void * bufferBegin);

  // internal methods to update the header object from the ImageIO's
  // set member variables
  void
  UpdateHeaderFromImageIO();

  // reimplemented
  void
  InternalReadImageInformation(std::ifstream & file);

  virtual void
  WriteImageInformation(const void * buffer);

  MRCHeaderObject::Pointer m_MRCHeader;
};
} // namespace itk

#endif
