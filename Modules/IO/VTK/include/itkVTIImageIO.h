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
 * \brief ImageIO class for reading and writing VTK XML ImageData (.vti)
 *        files.
 *
 * Supported on read (every encoding ParaView 5.x emits by default):
 *   * <VTKFile> attributes: type="ImageData", any version, any byte_order
 *     (LittleEndian / BigEndian), header_type in {UInt32, UInt64}.
 *   * <ImageData> attributes: WholeExtent, Origin, Spacing, and
 *     Direction (VTK 9+; defaults to identity when absent).
 *   * Single-<Piece> images.
 *   * <DataArray> format = "ascii" | "binary" (inline base64) |
 *     "appended" (raw or base64 AppendedData).
 *   * compressor = vtkZLibDataCompressor, or absent (uncompressed).
 *   * Pixel types: Scalar, Vector (3-component), RGB (3), RGBA (4), and
 *     symmetric second-rank tensor (6 components, VTK canonical
 *     [XX, YY, ZZ, XY, YZ, XZ] layout remapped to ITK's internal
 *     [e00, e01, e02, e11, e12, e22] on read).
 *
 * Supported on write:
 *   * <VTKFile version="1.0" header_type="UInt64"> matching ParaView 5.7+.
 *   * format = "ascii" and "binary" (inline base64) for every supported
 *     pixel type except binary symmetric tensor (see deferred list).
 *   * format = "appended" encoding="raw" + vtkZLibDataCompressor: enabled
 *     by calling SetUseCompression(true); produces the smallest files on
 *     disk and matches what ParaView emits by default for large images.
 *   * Direction is always emitted as a row-major 3x3 Direction attribute,
 *     padded with identity for images of dimension < 3.
 *
 * Deferred to the follow-up PR (each has a tagged guard exception so
 * `git grep F-NNN` locates the guard + test + commit):
 *   * F-001 vtkLZ4DataCompressor read
 *   * F-002 vtkLZMADataCompressor read
 *   * F-005 multi-<Piece> images
 *   * F-007 binary symmetric-tensor write
 *   * F-009 MetaDataDictionary round-trip
 *   * F-010 catch-all for unknown compressors
 *
 * Implementation notes:
 *   * XML header parsing uses expat (ITKExpat); <!DOCTYPE>/<!ENTITY>
 *     declarations are rejected up-front to mitigate billion-laughs
 *     and XXE attacks.
 *   * Expat is fed the file in chunks and suspended (XML_StopParser) at
 *     the <AppendedData> start tag so binary bytes never enter the parser.
 *     For large files this avoids a full in-memory copy of the binary
 *     payload during ReadImageInformation().
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

  /** Byte-swap \a numComponents values of \a componentSize bytes each in
   *  place when \a fileByteOrder differs from \a targetByteOrder.  The
   *  implementation reverses bytes within each component unconditionally
   *  (via std::reverse) rather than going through ByteSwapper's
   *  system-relative helpers, so it is deterministic on hosts of either
   *  endianness and therefore unit-testable without a big-endian runner.
   *  Public so that endianness behaviour can be exercised directly in
   *  tests. */
  static void
  SwapBufferForByteOrder(void *          buffer,
                         std::size_t     componentSize,
                         std::size_t     numComponents,
                         IOByteOrderEnum fileByteOrder,
                         IOByteOrderEnum targetByteOrder);

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
    Base64,            // binary data encoded in base64 (format="binary")
    RawAppended,       // raw binary appended data (format="appended" with encoding="raw")
    Base64Appended,    // base64-encoded appended data (format="appended" with encoding="base64")
    ZLibBase64,        // zlib-compressed data encoded in base64 (inline)
    ZLibAppended,      // zlib-compressed raw appended data
    ZLibBase64Appended // zlib-compressed base64 appended data
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

  /** Whether the file uses zlib compression (compressor="vtkZLibDataCompressor"). */
  bool m_IsZLibCompressed{ false };

  /** Whether appended data uses base64 encoding (encoding="base64") vs raw (encoding="raw"). */
  bool m_AppendedDataIsBase64{ false };

  /** Cached base64 content from appended data section when encoding="base64". */
  std::string m_AppendedBase64Content{};

  /** Decompress a VTK zlib-compressed block sequence into raw bytes.
   *  The input buffer begins with the VTK multi-block compression header
   *  (nblocks / uncompressed_blocksize / last_partial_size / compressed_sizes[]).
   *  Header integers are UInt32 or UInt64 per \a headerUInt64. */
  static void
  DecompressZLib(const unsigned char *        compressedData,
                 std::size_t                  compressedDataSize,
                 bool                         headerUInt64,
                 std::vector<unsigned char> & uncompressed);
};
} // end namespace itk

#endif // itkVTIImageIO_h
