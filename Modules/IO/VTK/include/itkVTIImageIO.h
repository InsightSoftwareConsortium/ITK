/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkVTIImageIO_h
#define itkVTIImageIO_h
#include "ITKIOVTKExport.h"

#include <fstream>
#include "itkImageIOBase.h"

namespace itk
{
/**
 * \class VTIImageIO
 *
 *  \brief ImageIO class for reading and writing VTK XML ImageData (.vti) files.
 *
 * Supports the VTK XML ImageData format (version 0.1 and 2.2), including
 * ASCII, binary (base64-encoded), and raw-appended data formats.
 * Scalar, vector (3-component), RGB, RGBA, and symmetric second rank tensor
 * pixel types are supported.
 *
 * The XML structure is parsed using the expat XML library (provided by
 * ITKExpat / ITKIOXML), so the parser is robust to attribute ordering,
 * whitespace, comments, and CDATA sections.  The raw appended data section
 * (which is XML-illegal binary content following an `_` marker) is read
 * directly from the file at the byte offset recorded by the parser when
 * it encountered the `<AppendedData>` element.
 *
 * \ingroup IOFilters
 * \ingroup ITKIOVTK
 */
class ITKIOVTK_EXPORT VTIImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VTIImageIO);

  /** Standard class type aliases. */
  using Self = VTIImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(VTIImageIO);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Set the spacing and dimension information for the current filename. */
  void
  ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  bool
  CanWriteFile(const char *) override;

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  void
  WriteImageInformation() override
  {}

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  void
  Write(const void * buffer) override;

protected:
  VTIImageIO();
  ~VTIImageIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Parse the XML header to fill image information. */
  void
  InternalReadImageInformation();

  /** Map VTK type string to ITK IOComponentEnum. */
  static IOComponentEnum
  VTKTypeStringToITKComponent(const std::string & vtkType);

  /** Map ITK IOComponentEnum to VTK type string. */
  static std::string
  ITKComponentToVTKTypeString(IOComponentEnum t);

  /** Decode a base64-encoded string into raw bytes.  Returns the number
   *  of decoded bytes. */
  static SizeType
  DecodeBase64(const std::string & encoded, std::vector<unsigned char> & decoded);

  /** Encode raw bytes as a base64 string. */
  static std::string
  EncodeBase64(const unsigned char * data, SizeType numBytes);

  /** Trim leading/trailing whitespace from a string. */
  static std::string
  TrimString(const std::string & s);

  // Encoding format of the data array found in the file.
  // (Plain comment, not doxygen, to satisfy KWStyle's `\class` rule for
  // class-like declarations.)
  enum class DataEncoding : std::uint8_t
  {
    ASCII,
    Base64,     // binary data encoded in base64 (format="binary")
    RawAppended // raw binary appended data (format="appended" with raw encoding)
  };

  DataEncoding m_DataEncoding{ DataEncoding::Base64 };

  /** Cached ASCII data text content captured by the expat parser when the
   *  active DataArray is in ASCII format.  Empty otherwise. */
  std::string m_AsciiDataContent{};

  /** Cached base64 data text content captured by the expat parser when the
   *  active DataArray is in base64 ("binary") format.  Empty otherwise. */
  std::string m_Base64DataContent{};

  /** Byte offset into the file where the appended data section begins
   *  (only relevant when m_DataEncoding == RawAppended). */
  std::streampos m_AppendedDataOffset{ 0 };

  /** Offset within the appended data block for this DataArray (in bytes,
   *  not counting the leading block-size UInt32/UInt64). */
  SizeType m_DataArrayOffset{ 0 };

  /** Whether the header uses 64-bit block-size integers (header_type="UInt64"). */
  bool m_HeaderTypeUInt64{ false };
};
} // end namespace itk

#endif // itkVTIImageIO_h
