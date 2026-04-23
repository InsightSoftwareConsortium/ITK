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
#include "itkVTIImageIO.h"
#include "itkByteSwapper.h"
#include "itkMakeUniqueForOverwrite.h"

#include "itk_expat.h"
#include "itk_zlib.h"
#include "itksys/Base64.h"

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace itk
{

namespace
{
// Case-insensitive string compare.
bool
IequalsStr(const std::string & a, const std::string & b)
{
  if (a.size() != b.size())
  {
    return false;
  }
  for (std::size_t i = 0; i < a.size(); ++i)
  {
    if (std::tolower(static_cast<unsigned char>(a[i])) != std::tolower(static_cast<unsigned char>(b[i])))
    {
      return false;
    }
  }
  return true;
}

// Lower-case a string.
std::string
ToLower(std::string s)
{
  std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) { return std::tolower(c); });
  return s;
}

// Look up an attribute value from an expat-style nullptr-terminated
// (key, value, key, value, ..., nullptr) array.  Returns empty string if
// not found.
std::string
FindAttribute(const char ** atts, const char * name)
{
  if (atts == nullptr)
  {
    return {};
  }
  for (int i = 0; atts[i] != nullptr; i += 2)
  {
    if (std::strcmp(atts[i], name) == 0)
    {
      return std::string(atts[i + 1] != nullptr ? atts[i + 1] : "");
    }
  }
  return {};
}

// ---------------------------------------------------------------------------
// VTI XML parser state.  Populated by expat callbacks.
// ---------------------------------------------------------------------------
struct VTIParseState
{
  // <VTKFile ...>
  bool        sawVTKFile{ false };
  std::string fileType;
  std::string byteOrder;
  std::string headerType;
  std::string compressor;

  // <ImageData ...>
  bool        sawImageData{ false };
  std::string wholeExtent;
  std::string origin;
  std::string spacing;
  std::string direction;

  // Count of <Piece> elements encountered inside <ImageData>.  F-005
  // (multi-Piece support) rejects files with pieceCount > 1 during
  // ReadImageInformation rather than risking silent data misassembly.
  int pieceCount{ 0 };

  // <PointData ...>
  std::string activeScalars;
  std::string activeVectors;
  std::string activeTensors;

  // First / active <DataArray ...>
  bool        haveDataArray{ false };
  std::string daType;
  std::string daName;
  std::string daFormat;
  std::string daNumberOfComponents;
  std::string daOffset;

  // Whether we are currently inside the active DataArray (so character
  // data should be appended to either ASCII or base64 buffer).
  bool inActiveDataArray{ false };
  bool isAsciiActive{ false };
  bool isBase64Active{ false };

  std::string asciiContent;
  std::string base64Content;

  // Set when the <AppendedData> start tag is seen.  Used by the caller
  // to know that the file has an appended-data block (whose binary
  // contents are read directly, not via expat).
  bool        sawAppendedData{ false };
  std::string appendedDataEncoding; // "raw" or "base64"

  // Handle to the active expat parser, set before the first XML_Parse call.
  // Used by VTIStartElement to call XML_StopParser when <AppendedData> is
  // encountered so we avoid feeding binary bytes to expat.
  XML_Parser parserHandle{ nullptr };
};

// Determine whether a DataArray (identified by its Name attribute) is the
// "active" one for this file.  If no PointData Scalars/Vectors/Tensors
// attribute is set, the very first DataArray is taken as active.
bool
IsActiveDataArray(const VTIParseState & st, const std::string & daName)
{
  const bool noActive = st.activeScalars.empty() && st.activeVectors.empty() && st.activeTensors.empty();
  if (noActive)
  {
    return true;
  }
  return (!st.activeScalars.empty() && daName == st.activeScalars) ||
         (!st.activeVectors.empty() && daName == st.activeVectors) ||
         (!st.activeTensors.empty() && daName == st.activeTensors);
}

extern "C"
{
  static void
  VTIStartElement(void * userData, const char * name, const char ** atts)
  {
    auto * st = static_cast<VTIParseState *>(userData);

    if (std::strcmp(name, "VTKFile") == 0)
    {
      st->sawVTKFile = true;
      st->fileType = FindAttribute(atts, "type");
      st->byteOrder = FindAttribute(atts, "byte_order");
      st->headerType = FindAttribute(atts, "header_type");
      st->compressor = FindAttribute(atts, "compressor");
    }
    else if (std::strcmp(name, "ImageData") == 0)
    {
      st->sawImageData = true;
      st->wholeExtent = FindAttribute(atts, "WholeExtent");
      st->origin = FindAttribute(atts, "Origin");
      st->spacing = FindAttribute(atts, "Spacing");
      st->direction = FindAttribute(atts, "Direction");
    }
    else if (std::strcmp(name, "Piece") == 0)
    {
      ++st->pieceCount;
    }
    else if (std::strcmp(name, "PointData") == 0)
    {
      st->activeScalars = FindAttribute(atts, "Scalars");
      st->activeVectors = FindAttribute(atts, "Vectors");
      st->activeTensors = FindAttribute(atts, "Tensors");
    }
    else if (std::strcmp(name, "DataArray") == 0)
    {
      const std::string daName = FindAttribute(atts, "Name");
      if (st->haveDataArray)
      {
        // We have already captured an active DataArray; ignore subsequent
        // ones (we read only one image array per file).
        return;
      }
      if (!IsActiveDataArray(*st, daName))
      {
        return;
      }
      st->haveDataArray = true;
      st->daType = FindAttribute(atts, "type");
      st->daName = daName;
      st->daFormat = ToLower(FindAttribute(atts, "format"));
      st->daNumberOfComponents = FindAttribute(atts, "NumberOfComponents");
      st->daOffset = FindAttribute(atts, "offset");

      st->inActiveDataArray = true;
      st->isAsciiActive = (st->daFormat == "ascii");
      st->isBase64Active = (st->daFormat == "binary"); // VTK XML "binary" == base64
    }
    else if (std::strcmp(name, "AppendedData") == 0)
    {
      st->sawAppendedData = true;
      st->appendedDataEncoding = ToLower(FindAttribute(atts, "encoding"));
      // Suspend expat immediately after this start tag so the parser never
      // sees the binary payload that follows the `_` marker.  Using
      // XML_TRUE (resumable/suspended) rather than XML_FALSE (aborted)
      // so that XML_Parse returns XML_STATUS_SUSPENDED, not an error code,
      // and XML_GetCurrentByteIndex remains valid.
      if (st->parserHandle != nullptr)
      {
        XML_StopParser(st->parserHandle, XML_TRUE);
      }
    }
  }

  static void
  VTIEndElement(void * userData, const char * name)
  {
    auto * st = static_cast<VTIParseState *>(userData);
    if (std::strcmp(name, "DataArray") == 0)
    {
      st->inActiveDataArray = false;
      st->isAsciiActive = false;
      st->isBase64Active = false;
    }
  }

  static void
  VTICharData(void * userData, const char * data, int length)
  {
    auto * st = static_cast<VTIParseState *>(userData);
    if (!st->inActiveDataArray)
    {
      return;
    }
    if (st->isAsciiActive)
    {
      st->asciiContent.append(data, static_cast<std::size_t>(length));
    }
    else if (st->isBase64Active)
    {
      st->base64Content.append(data, static_cast<std::size_t>(length));
    }
  }
} // extern "C"

} // end anonymous namespace

// ---------------------------------------------------------------------------
VTIImageIO::VTIImageIO()
{
  this->SetNumberOfDimensions(3);
  m_ByteOrder = IOByteOrderEnum::LittleEndian;
  m_FileType = IOFileEnum::Binary;

  this->AddSupportedReadExtension(".vti");
  this->AddSupportedWriteExtension(".vti");
}

VTIImageIO::~VTIImageIO() = default;

// ---------------------------------------------------------------------------
void
VTIImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

// ---------------------------------------------------------------------------
bool
VTIImageIO::CanReadFile(const char * filename)
{
  if (!this->HasSupportedReadExtension(filename))
  {
    return false;
  }

  std::ifstream file(filename, std::ios::in);
  if (!file.is_open())
  {
    return false;
  }

  // Read first 256 bytes and look for VTKFile + ImageData.
  char buf[256]{};
  file.read(buf, 255);
  const std::string header(buf);
  return header.find("VTKFile") != std::string::npos && header.find("ImageData") != std::string::npos;
}

// ---------------------------------------------------------------------------
bool
VTIImageIO::CanWriteFile(const char * filename)
{
  return this->HasSupportedWriteExtension(filename);
}

// ---------------------------------------------------------------------------
// Static helpers
// ---------------------------------------------------------------------------

std::string
VTIImageIO::TrimString(const std::string & s)
{
  const std::size_t start = s.find_first_not_of(" \t\r\n");
  if (start == std::string::npos)
  {
    return {};
  }
  const std::size_t end = s.find_last_not_of(" \t\r\n");
  return s.substr(start, end - start + 1);
}

VTIImageIO::SizeType
VTIImageIO::DecodeBase64(const std::string & encoded, std::vector<unsigned char> & decoded)
{
  // itksysBase64_Decode stops at any non-Base64 byte (including whitespace and '=' padding),
  // so we need to handle that by invoking it multiple times.

  // Allocate output buffer: worst case is (encoded.size() / 4) * 3 bytes
  decoded.assign((encoded.size() / 4) * 3 + 3, 0);

  const unsigned char * inputStart = reinterpret_cast<const unsigned char *>(encoded.data());
  const unsigned char * inputPtr = inputStart;
  unsigned char *       outputPtr = decoded.data();
  bool                  workLeft = true;
  while (workLeft)
  {
    size_t            processed = inputPtr - inputStart;
    const std::size_t produced = itksysBase64_Decode(inputPtr,
                                                     0, // max_output_length ignored when max_input_length is non-zero
                                                     outputPtr,
                                                     encoded.size() - processed);

    // Advance input by the number of Base64 chars that produced the output
    inputPtr += (produced + produced % 3) / 3 * 4;
    outputPtr += produced;
    while (inputPtr < inputStart + encoded.size() && (std::isspace(*inputPtr) || *inputPtr == '='))
    {
      ++inputPtr;
    }
    if (produced == 0 || inputPtr >= inputStart + encoded.size())
    {
      workLeft = false;
    }
  }

  decoded.resize(outputPtr - decoded.data());
  return static_cast<SizeType>(decoded.size());
}

std::string
VTIImageIO::EncodeBase64(const unsigned char * data, SizeType numBytes)
{
  // Worst-case output length: ceil(numBytes/3)*4.
  std::string       out(((static_cast<std::size_t>(numBytes) + 2) / 3) * 4, '\0');
  const std::size_t produced = itksysBase64_Encode(data,
                                                   static_cast<std::size_t>(numBytes),
                                                   reinterpret_cast<unsigned char *>(&out[0]),
                                                   /*mark_end=*/0);
  out.resize(produced);
  return out;
}

VTIImageIO::IOComponentEnum
VTIImageIO::VTKTypeStringToITKComponent(const std::string & vtkType)
{
  const std::string t = ToLower(vtkType);
  if (t == "int8" || t == "char")
  {
    return IOComponentEnum::CHAR;
  }
  if (t == "uint8" || t == "unsigned_char")
  {
    return IOComponentEnum::UCHAR;
  }
  if (t == "int16" || t == "short")
  {
    return IOComponentEnum::SHORT;
  }
  if (t == "uint16" || t == "unsigned_short")
  {
    return IOComponentEnum::USHORT;
  }
  if (t == "int32" || t == "int")
  {
    return IOComponentEnum::INT;
  }
  if (t == "uint32" || t == "unsigned_int")
  {
    return IOComponentEnum::UINT;
  }
  if (t == "int64" || t == "long" || t == "vtktypeint64")
  {
    return IOComponentEnum::LONGLONG;
  }
  if (t == "uint64" || t == "unsigned_long" || t == "vtktypeuint64")
  {
    return IOComponentEnum::ULONGLONG;
  }
  if (t == "float32" || t == "float")
  {
    return IOComponentEnum::FLOAT;
  }
  if (t == "float64" || t == "double")
  {
    return IOComponentEnum::DOUBLE;
  }
  return IOComponentEnum::UNKNOWNCOMPONENTTYPE;
}

std::string
VTIImageIO::ITKComponentToVTKTypeString(IOComponentEnum t)
{
  switch (t)
  {
    case IOComponentEnum::CHAR:
      return "Int8";
    case IOComponentEnum::UCHAR:
      return "UInt8";
    case IOComponentEnum::SHORT:
      return "Int16";
    case IOComponentEnum::USHORT:
      return "UInt16";
    case IOComponentEnum::INT:
      return "Int32";
    case IOComponentEnum::UINT:
      return "UInt32";
    case IOComponentEnum::LONG:
      if (sizeof(long) == 4)
      {
        return "Int32";
      }
      else
      {
        return "Int64";
      }
    case IOComponentEnum::ULONG:
      if (sizeof(unsigned long) == 4)
      {
        return "UInt32";
      }
      else
      {
        return "UInt64";
      }
    case IOComponentEnum::LONGLONG:
      return "Int64";
    case IOComponentEnum::ULONGLONG:
      return "UInt64";
    case IOComponentEnum::FLOAT:
      return "Float32";
    case IOComponentEnum::DOUBLE:
      return "Float64";
    default:
      itkGenericExceptionMacro("Unsupported component type for VTI writing.");
  }
}

// ---------------------------------------------------------------------------
// ReadImageInformation
// ---------------------------------------------------------------------------
void
VTIImageIO::ReadImageInformation()
{
  // Reset cached parser results.
  m_AsciiDataContent.clear();
  m_Base64DataContent.clear();
  m_AppendedDataOffset = 0;
  m_DataArrayOffset = 0;
  m_HeaderTypeUInt64 = false;
  m_IsZLibCompressed = false;
  m_AppendedDataIsBase64 = false;
  m_AppendedBase64Content.clear();

  // Open the file for streaming.  We feed content to expat in chunks rather
  // than slurping the whole file.  When the <AppendedData> start element is
  // seen, VTIStartElement calls XML_StopParser so expat halts before touching
  // any binary payload.  XML_GetCurrentByteIndex then gives the file offset of
  // the tag, and we scan forward from there for the `_` data marker.  For
  // files without an <AppendedData> section (ASCII / inline base64) we parse
  // to EOF normally.  The net effect: large VTI files are read once, not twice.
  std::ifstream xmlFile(m_FileName.c_str(), std::ios::in | std::ios::binary);
  if (!xmlFile.is_open())
  {
    itkExceptionMacro("Cannot open or read file: " << m_FileName);
  }

  // Security pre-scan: DOCTYPE / ENTITY declarations must appear before any
  // element content, so they will be in the first few hundred bytes.  Reading
  // 512 bytes is sufficient and avoids loading the full file for this check.
  {
    char secBuf[512]{};
    xmlFile.read(secBuf, sizeof(secBuf) - 1);
    const std::string prefix(secBuf, static_cast<std::size_t>(xmlFile.gcount()));
    if (prefix.find("<!DOCTYPE") != std::string::npos || prefix.find("<!ENTITY") != std::string::npos)
    {
      itkExceptionMacro("Rejecting VTI file with DOCTYPE or ENTITY declaration "
                        "(billion-laughs / XXE mitigation): "
                        << m_FileName);
    }
    xmlFile.clear();
    xmlFile.seekg(0, std::ios::beg);
  }

  VTIParseState st;
  XML_Parser    parser = XML_ParserCreate(nullptr);
  if (parser == nullptr)
  {
    itkExceptionMacro("Failed to create expat XML parser.");
  }

  // Harden: disable external parameter-entity parsing (defence 1 of 2).
  // Defence 2 is the pre-scan above.
  XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_NEVER);
  st.parserHandle = parser;

  XML_SetUserData(parser, &st);
  XML_SetElementHandler(parser, &VTIStartElement, &VTIEndElement);
  XML_SetCharacterDataHandler(parser, &VTICharData);

  // Feed the file in chunks.  Stop when expat is suspended (AppendedData
  // encountered via XML_StopParser(XML_TRUE)) or when we reach EOF.
  constexpr int     kChunkSize = 65536;
  std::vector<char> chunk(kChunkSize);
  XML_Status        parseStatus = XML_STATUS_OK;
  bool              reachedEOF = false;

  while (!reachedEOF && parseStatus == XML_STATUS_OK)
  {
    xmlFile.read(chunk.data(), kChunkSize);
    const auto bytesRead = static_cast<int>(xmlFile.gcount());
    reachedEOF = (bytesRead < kChunkSize);
    parseStatus = XML_Parse(parser, chunk.data(), bytesRead, reachedEOF ? 1 : 0);
  }

  // XML_STATUS_SUSPENDED: VTIStartElement called XML_StopParser at <AppendedData>.
  // XML_STATUS_OK after reachedEOF: normal end-of-file for ASCII/inline-base64 files.
  // XML_STATUS_ERROR: genuine parse failure.
  if (parseStatus == XML_STATUS_ERROR)
  {
    const std::string err = XML_ErrorString(XML_GetErrorCode(parser));
    XML_ParserFree(parser);
    itkExceptionMacro("XML parse error in " << m_FileName << ": " << err);
  }

  // If we stopped at <AppendedData>, locate the `_` binary marker and record
  // the file offset of the first byte of payload.
  if (st.sawAppendedData)
  {
    const long tagByteIndex = XML_GetCurrentByteIndex(parser);
    xmlFile.clear();
    xmlFile.seekg(tagByteIndex, std::ios::beg);

    char c = '\0';
    while (xmlFile.get(c) && c != '_')
    {
    }
    if (c != '_')
    {
      XML_ParserFree(parser);
      itkExceptionMacro("Missing `_` marker in <AppendedData> section of: " << m_FileName);
    }
    m_AppendedDataOffset = xmlFile.tellg();

    // For base64-encoded appended data, read the text content now so
    // Read() doesn't need to re-open the file.
    if (IequalsStr(st.appendedDataEncoding, "base64"))
    {
      std::string b64content;
      std::getline(xmlFile, b64content, '<'); // read until </AppendedData>
      m_AppendedBase64Content = std::move(b64content);
    }
  }

  XML_ParserFree(parser);

  // ---- Validate captured XML ------------------------------------------
  if (!st.sawVTKFile)
  {
    itkExceptionMacro("Not a valid VTK XML file (missing <VTKFile> element): " << m_FileName);
  }
  if (!IequalsStr(st.fileType, "ImageData"))
  {
    itkExceptionMacro("VTK XML file is not of type ImageData: " << m_FileName);
  }
  if (!st.sawImageData)
  {
    itkExceptionMacro("Missing <ImageData> element in file: " << m_FileName);
  }
  // F-005 check runs before the "no DataArray" fallback so a multi-Piece
  // file with zero DataArrays still yields the precise guard diagnostic.
  if (st.pieceCount > 1)
  {
    itkExceptionMacro("F-005 Multi-Piece ImageData files not yet supported.  File '"
                      << m_FileName << "' contains " << st.pieceCount
                      << " <Piece> elements.  Deferred to the follow-up PR; see F-005 "
                         "in commit history.");
  }
  if (!st.haveDataArray)
  {
    itkExceptionMacro("No DataArray element found in file: " << m_FileName);
  }

  // Byte order
  if (ToLower(st.byteOrder) == "bigendian")
  {
    m_ByteOrder = IOByteOrderEnum::BigEndian;
  }
  else
  {
    m_ByteOrder = IOByteOrderEnum::LittleEndian;
  }

  // Header type
  m_HeaderTypeUInt64 = (ToLower(st.headerType) == "uint64");

  // Geometry
  if (st.wholeExtent.empty())
  {
    itkExceptionMacro("Missing WholeExtent attribute in <ImageData>: " << m_FileName);
  }
  int extents[6] = { 0, 0, 0, 0, 0, 0 };
  {
    std::istringstream extStream(st.wholeExtent);
    for (int & ext : extents)
    {
      extStream >> ext;
    }
  }
  double origin[3] = { 0.0, 0.0, 0.0 };
  if (!st.origin.empty())
  {
    std::istringstream os2(st.origin);
    os2 >> origin[0] >> origin[1] >> origin[2];
  }
  double spacing[3] = { 1.0, 1.0, 1.0 };
  if (!st.spacing.empty())
  {
    std::istringstream spStr(st.spacing);
    spStr >> spacing[0] >> spacing[1] >> spacing[2];
  }

  // Direction cosines (VTK 9+ optional attribute; row-major 3x3 where
  // column j is the direction vector of image axis j in world space).
  // Absent => identity, which is ITK's default, so we simply skip the
  // SetDirection calls below when the attribute is missing.
  double directionMatrix[3][3] = { { 1.0, 0.0, 0.0 }, { 0.0, 1.0, 0.0 }, { 0.0, 0.0, 1.0 } };
  bool   directionSpecified = false;
  if (!st.direction.empty())
  {
    std::istringstream dirStream(st.direction);
    for (int r = 0; r < 3; ++r)
    {
      for (int c = 0; c < 3; ++c)
      {
        dirStream >> directionMatrix[r][c];
      }
    }
    if (dirStream.fail())
    {
      itkExceptionMacro("Malformed Direction attribute '"
                        << st.direction << "' (expected 9 space-separated floats) in file: " << m_FileName);
    }
    directionSpecified = true;
  }

  const int nx = extents[1] - extents[0] + 1;
  const int ny = extents[3] - extents[2] + 1;
  const int nz = extents[5] - extents[4] + 1;

  if (nz <= 1 && ny <= 1)
  {
    this->SetNumberOfDimensions(1);
  }
  else if (nz <= 1)
  {
    this->SetNumberOfDimensions(2);
  }
  else
  {
    this->SetNumberOfDimensions(3);
  }

  this->SetDimensions(0, static_cast<unsigned int>(nx));
  if (this->GetNumberOfDimensions() > 1)
  {
    this->SetDimensions(1, static_cast<unsigned int>(ny));
  }
  if (this->GetNumberOfDimensions() > 2)
  {
    this->SetDimensions(2, static_cast<unsigned int>(nz));
  }
  for (unsigned int i = 0; i < this->GetNumberOfDimensions(); ++i)
  {
    this->SetSpacing(i, spacing[i]);
    this->SetOrigin(i, origin[i]);
  }
  if (directionSpecified)
  {
    const unsigned int nd = this->GetNumberOfDimensions();
    for (unsigned int axis = 0; axis < nd; ++axis)
    {
      std::vector<double> axisVec(nd);
      for (unsigned int r = 0; r < nd; ++r)
      {
        axisVec[r] = directionMatrix[r][axis];
      }
      this->SetDirection(axis, axisVec);
    }
  }

  // Component type
  const IOComponentEnum compType = VTKTypeStringToITKComponent(st.daType);
  if (compType == IOComponentEnum::UNKNOWNCOMPONENTTYPE)
  {
    itkExceptionMacro("Unknown VTK DataArray type '" << st.daType << "' in file: " << m_FileName);
  }
  this->SetComponentType(compType);

  // Number of components
  unsigned int numComp = 1u;
  if (!st.daNumberOfComponents.empty())
  {
    try
    {
      numComp = static_cast<unsigned int>(std::stoul(st.daNumberOfComponents));
    }
    catch (const std::exception & e)
    {
      itkExceptionMacro("Invalid NumberOfComponents '" << st.daNumberOfComponents << "' in file '" << m_FileName
                                                       << "': " << e.what());
    }
  }
  this->SetNumberOfComponents(numComp);

  // Pixel type, derived from the active PointData attribute and component count
  const bool isTensor = !st.activeTensors.empty() && st.daName == st.activeTensors;
  const bool isVector = !st.activeVectors.empty() && st.daName == st.activeVectors;
  if (isTensor)
  {
    // VTK canonical symmetric-tensor layout is 6 components per pixel
    // in [XX, YY, ZZ, XY, YZ, XZ] order.  ITK's in-memory
    // SymmetricSecondRankTensor<T,3> uses [e00, e01, e02, e11, e12, e22]
    // (upper-triangular row-major); the actual permutation is applied in
    // Read() after the encoding path populates the buffer.
    if (numComp != 6)
    {
      itkExceptionMacro("Active Tensors DataArray has NumberOfComponents=\""
                        << numComp
                        << "\"; expected 6 for VTK-canonical symmetric-tensor layout "
                           "[XX, YY, ZZ, XY, YZ, XZ] in file: "
                        << m_FileName);
    }
    this->SetPixelType(IOPixelEnum::SYMMETRICSECONDRANKTENSOR);
    this->SetNumberOfComponents(6);
  }
  else if (isVector)
  {
    this->SetPixelType(IOPixelEnum::VECTOR);
  }
  else if (numComp == 1)
  {
    this->SetPixelType(IOPixelEnum::SCALAR);
  }
  else if (numComp == 3)
  {
    this->SetPixelType(IOPixelEnum::RGB);
  }
  else if (numComp == 4)
  {
    this->SetPixelType(IOPixelEnum::RGBA);
  }
  else
  {
    this->SetPixelType(IOPixelEnum::VECTOR);
  }

  // Compression.  Only vtkZLibDataCompressor is currently supported; LZ4 /
  // LZMA / any future VTK compressor triggers a tagged exception so users
  // get a clear error instead of silently falling through to the
  // uncompressed read path.  Guard tags (F-NNN) correspond to follow-up
  // work items; `git grep F-001` etc. locates the guard + guard test +
  // code comment for each.
  const std::string compressorLower = ToLower(st.compressor);
  if (!compressorLower.empty() && compressorLower.find("zlib") == std::string::npos)
  {
    if (compressorLower.find("lz4") != std::string::npos)
    {
      itkExceptionMacro("F-001 LZ4 decompressor not yet implemented.  "
                        "Compressor attribute: '"
                        << st.compressor << "'.  File: " << m_FileName
                        << ".  Deferred to the follow-up PR; see F-001 in commit history.");
    }
    if (compressorLower.find("lzma") != std::string::npos)
    {
      itkExceptionMacro("F-002 LZMA decompressor not yet implemented.  "
                        "Compressor attribute: '"
                        << st.compressor << "'.  File: " << m_FileName
                        << ".  Deferred to the follow-up PR; see F-002 in commit history.");
    }
    itkExceptionMacro("F-010 Unknown VTK compressor '"
                      << st.compressor
                      << "'.  Only vtkZLibDataCompressor is supported today; LZ4 / LZMA "
                         "(F-001 / F-002) and any new compressor require an explicit "
                         "decoder path.  File: "
                      << m_FileName);
  }
  m_IsZLibCompressed = IequalsStr(st.compressor, "vtkZLibDataCompressor");

  // Appended data encoding (already read into m_AppendedBase64Content above
  // for the base64 case; raw-appended offset is in m_AppendedDataOffset).
  m_AppendedDataIsBase64 = IequalsStr(st.appendedDataEncoding, "base64");

  // Encoding
  if (st.daFormat == "ascii")
  {
    m_DataEncoding = DataEncoding::ASCII;
    m_FileType = IOFileEnum::ASCII;
    m_AsciiDataContent = std::move(st.asciiContent);
  }
  else if (st.daFormat == "appended")
  {
    if (!st.sawAppendedData)
    {
      itkExceptionMacro(
        "DataArray uses format=\"appended\" but no <AppendedData> element was found in: " << m_FileName);
    }
    // Select encoding based on compression and base64 encoding
    if (m_IsZLibCompressed)
    {
      m_DataEncoding = m_AppendedDataIsBase64 ? DataEncoding::ZLibBase64Appended : DataEncoding::ZLibAppended;
    }
    else
    {
      m_DataEncoding = m_AppendedDataIsBase64 ? DataEncoding::Base64Appended : DataEncoding::RawAppended;
    }
    m_FileType = IOFileEnum::Binary;
    m_DataArrayOffset = 0u;
    if (!st.daOffset.empty())
    {
      try
      {
        m_DataArrayOffset = static_cast<SizeType>(std::stoull(st.daOffset));
      }
      catch (const std::exception & e)
      {
        itkExceptionMacro("Invalid DataArray offset '" << st.daOffset << "' in file '" << m_FileName
                                                       << "': " << e.what());
      }
    }
  }
  else // "binary" (base64) or unspecified, defaulting to binary
  {
    m_DataEncoding = m_IsZLibCompressed ? DataEncoding::ZLibBase64 : DataEncoding::Base64;
    m_FileType = IOFileEnum::Binary;
    m_Base64DataContent = std::move(st.base64Content);
  }
}

void
VTIImageIO::SwapBufferForByteOrder(void *          buffer,
                                   std::size_t     componentSize,
                                   std::size_t     numComponents,
                                   IOByteOrderEnum fileByteOrder,
                                   IOByteOrderEnum targetByteOrder)
{
  if (fileByteOrder == targetByteOrder || componentSize <= 1)
  {
    return;
  }
  const IOByteOrderEnum systemByteOrder =
    ByteSwapper<uint16_t>::SystemIsBigEndian() ? IOByteOrderEnum::BigEndian : IOByteOrderEnum::LittleEndian;

  // When either the source or the target is the host's native byte order --
  // the only case that occurs in the Read/Write paths -- dispatch to
  // itk::ByteSwapper for a correctly-sized, well-tested swap.  For the
  // uncommon file-to-target swap that does not involve the host order,
  // fall back to a direct per-component byte reverse.
  if (fileByteOrder == systemByteOrder || targetByteOrder == systemByteOrder)
  {
    const IOByteOrderEnum nonSystem = (fileByteOrder == systemByteOrder) ? targetByteOrder : fileByteOrder;
    const auto            swap = [&](auto * typed) {
      using T = std::remove_pointer_t<decltype(typed)>;
      if (nonSystem == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<T>::SwapRangeFromSystemToLittleEndian(typed, numComponents);
      }
      else
      {
        ByteSwapper<T>::SwapRangeFromSystemToBigEndian(typed, numComponents);
      }
    };
    switch (componentSize)
    {
      case 2:
        swap(static_cast<uint16_t *>(buffer));
        return;
      case 4:
        swap(static_cast<uint32_t *>(buffer));
        return;
      case 8:
        swap(static_cast<uint64_t *>(buffer));
        return;
      default:
        break; // fall through to std::reverse for unusual sizes
    }
  }

  auto * bytes = static_cast<unsigned char *>(buffer);
  for (std::size_t i = 0; i < numComponents; ++i)
  {
    unsigned char * c = bytes + i * componentSize;
    std::reverse(c, c + componentSize);
  }
}

namespace
{
// Thin wrapper that defaults targetByteOrder to the host's native order.
// Kept so the existing encoding-path call sites remain untouched.
void
SwapBufferIfNeeded(void * buffer, std::size_t componentSize, std::size_t numComponents, IOByteOrderEnum fileByteOrder)
{
  const IOByteOrderEnum target =
    ByteSwapper<uint16_t>::SystemIsBigEndian() ? IOByteOrderEnum::BigEndian : IOByteOrderEnum::LittleEndian;
  VTIImageIO::SwapBufferForByteOrder(buffer, componentSize, numComponents, fileByteOrder, target);
}
} // anonymous namespace

// ---------------------------------------------------------------------------
// Read
// ---------------------------------------------------------------------------
void
VTIImageIO::DecompressZLib(const unsigned char *        compressedData,
                           std::size_t                  compressedDataSize,
                           bool                         headerUInt64,
                           std::vector<unsigned char> & uncompressed)
{
  // VTK zlib compressed block layout:
  //   [nblocks]               UInt32 or UInt64
  //   [uncompressed_blocksize] UInt32 or UInt64  (size of each full block)
  //   [last_partial_blocksize] UInt32 or UInt64  (0 means last block is full)
  //   [compressed_size_0]     UInt32 or UInt64
  //   [compressed_size_1]     UInt32 or UInt64
  //   ...
  //   [compressed_data_0][compressed_data_1]...
  const std::size_t hdrItemSize = headerUInt64 ? sizeof(uint64_t) : sizeof(uint32_t);

  auto readHeader = [&](std::size_t offset) -> uint64_t {
    if (headerUInt64)
    {
      uint64_t v;
      std::memcpy(&v, compressedData + offset, sizeof(v));
      return v;
    }
    else
    {
      uint32_t v;
      std::memcpy(&v, compressedData + offset, sizeof(v));
      return static_cast<uint64_t>(v);
    }
  };

  const uint64_t nblocks = readHeader(0);
  const uint64_t uncompBlockSize = readHeader(hdrItemSize);
  const uint64_t lastPartialSize = readHeader(2 * hdrItemSize);

  // Compute total uncompressed size
  const uint64_t lastBlockUncompSize = (lastPartialSize == 0) ? uncompBlockSize : lastPartialSize;
  const uint64_t totalUncompSize = (nblocks > 1 ? (nblocks - 1) * uncompBlockSize : 0) + lastBlockUncompSize;

  uncompressed.resize(static_cast<std::size_t>(totalUncompSize));

  // Compressed block sizes start at offset 3*hdrItemSize
  const std::size_t blockSizesOffset = 3 * hdrItemSize;

  // Compressed data starts after the header (3 + nblocks) items
  std::size_t dataOffset = static_cast<std::size_t>((3 + nblocks) * hdrItemSize);
  std::size_t uncompOffset = 0;

  for (uint64_t b = 0; b < nblocks; ++b)
  {
    const uint64_t compSize = readHeader(blockSizesOffset + b * hdrItemSize);
    const uint64_t thisUncompSize = (b == nblocks - 1) ? lastBlockUncompSize : uncompBlockSize;

    if (dataOffset + static_cast<std::size_t>(compSize) > compressedDataSize)
    {
      itkGenericExceptionMacro("ZLib compressed block extends beyond buffer.");
    }

    uLongf    destLen = static_cast<uLongf>(thisUncompSize);
    const int ret = uncompress(reinterpret_cast<Bytef *>(uncompressed.data() + uncompOffset),
                               &destLen,
                               reinterpret_cast<const Bytef *>(compressedData + dataOffset),
                               static_cast<uLong>(compSize));
    if (ret != Z_OK)
    {
      itkGenericExceptionMacro("zlib uncompress failed for VTI block (code " << ret << ").");
    }

    dataOffset += static_cast<std::size_t>(compSize);
    uncompOffset += static_cast<std::size_t>(destLen);
  }
}

namespace
{
// Compress raw bytes into the VTK multi-block zlib appended format.
//
// Output layout (all integers are UInt64, matching header_type="UInt64"):
//   [nblocks][uncompressed_blocksize][last_partial_blocksize]
//   [compressed_size_0][compressed_size_1]...
//   [compressed_data_0][compressed_data_1]...
//
// last_partial_blocksize is 0 when the last block is exactly blockSize bytes.
std::vector<unsigned char>
CompressZLibVTK(const unsigned char * data, std::size_t totalBytes, std::size_t blockSize = 65536)
{
  if (totalBytes == 0)
  {
    // Emit a valid single-block header with zero sizes.
    std::vector<unsigned char> result(4 * sizeof(uint64_t), 0);
    uint64_t                   one = 1;
    std::memcpy(result.data(), &one, sizeof(uint64_t)); // nblocks = 1
    return result;
  }

  const uint64_t nblocks = static_cast<uint64_t>((totalBytes + blockSize - 1) / blockSize);
  const uint64_t lastBlockBytes =
    (totalBytes % blockSize == 0) ? static_cast<uint64_t>(blockSize) : static_cast<uint64_t>(totalBytes % blockSize);
  const uint64_t lastPartialSize = (lastBlockBytes == static_cast<uint64_t>(blockSize)) ? 0u : lastBlockBytes;

  std::vector<std::vector<unsigned char>> compBlocks(static_cast<std::size_t>(nblocks));
  for (uint64_t b = 0; b < nblocks; ++b)
  {
    const std::size_t offset = static_cast<std::size_t>(b) * blockSize;
    const std::size_t thisBlockBytes = (b == nblocks - 1) ? static_cast<std::size_t>(lastBlockBytes) : blockSize;

    uLongf destLen = compressBound(static_cast<uLong>(thisBlockBytes));
    compBlocks[static_cast<std::size_t>(b)].resize(static_cast<std::size_t>(destLen));

    const int ret = compress2(reinterpret_cast<Bytef *>(compBlocks[static_cast<std::size_t>(b)].data()),
                              &destLen,
                              reinterpret_cast<const Bytef *>(data + offset),
                              static_cast<uLong>(thisBlockBytes),
                              Z_DEFAULT_COMPRESSION);
    if (ret != Z_OK)
    {
      itkGenericExceptionMacro("zlib compress failed for block " << b << " (code " << ret << ").");
    }
    compBlocks[static_cast<std::size_t>(b)].resize(static_cast<std::size_t>(destLen));
  }

  const std::size_t headerSize = static_cast<std::size_t>(3 + nblocks) * sizeof(uint64_t);
  std::size_t       payloadSize = 0;
  for (const auto & block : compBlocks)
  {
    payloadSize += block.size();
  }

  std::vector<unsigned char> result(headerSize + payloadSize);
  unsigned char *            p = result.data();

  auto writeU64 = [&](uint64_t v) {
    std::memcpy(p, &v, sizeof(uint64_t));
    p += sizeof(uint64_t);
  };

  writeU64(nblocks);
  writeU64(static_cast<uint64_t>(blockSize));
  writeU64(lastPartialSize);
  for (uint64_t b = 0; b < nblocks; ++b)
  {
    writeU64(static_cast<uint64_t>(compBlocks[static_cast<std::size_t>(b)].size()));
  }
  for (uint64_t b = 0; b < nblocks; ++b)
  {
    const auto & block = compBlocks[static_cast<std::size_t>(b)];
    std::memcpy(p, block.data(), block.size());
    p += block.size();
  }
  return result;
}

// Remap a buffer of symmetric-tensor pixels from VTK canonical layout
// [XX, YY, ZZ, XY, YZ, XZ] to ITK's in-memory
// SymmetricSecondRankTensor<T,3> layout [e00, e01, e02, e11, e12, e22]
// (upper-triangular row-major), in place.  Called after each encoding
// path in Read() has populated `buffer`.
void
RemapTensorVTKToITK(void * buffer, std::size_t numPixels, std::size_t componentSize)
{
  std::vector<unsigned char> tmp(6 * componentSize);
  auto *                     bufBytes = static_cast<unsigned char *>(buffer);
  for (std::size_t p = 0; p < numPixels; ++p)
  {
    unsigned char * px = bufBytes + p * 6 * componentSize;
    std::memcpy(tmp.data(), px, 6 * componentSize);
    std::memcpy(px + 0 * componentSize, tmp.data() + 0 * componentSize, componentSize); // e00 = XX
    std::memcpy(px + 1 * componentSize, tmp.data() + 3 * componentSize, componentSize); // e01 = XY
    std::memcpy(px + 2 * componentSize, tmp.data() + 5 * componentSize, componentSize); // e02 = XZ
    std::memcpy(px + 3 * componentSize, tmp.data() + 1 * componentSize, componentSize); // e11 = YY
    std::memcpy(px + 4 * componentSize, tmp.data() + 4 * componentSize, componentSize); // e12 = YZ
    std::memcpy(px + 5 * componentSize, tmp.data() + 2 * componentSize, componentSize); // e22 = ZZ
  }
}

// Scope guard that applies RemapTensorVTKToITK when destroyed, so every
// early `return` out of Read() gets the remap without having to edit each
// exit point individually.  Does nothing if the image is not a symmetric
// tensor.
struct TensorRemapGuard
{
  void *      buffer;
  std::size_t numPixels;
  std::size_t componentSize;
  bool        active;
  ~TensorRemapGuard()
  {
    if (active)
    {
      RemapTensorVTKToITK(buffer, numPixels, componentSize);
    }
  }
};
} // namespace

void
VTIImageIO::Read(void * buffer)
{
  const SizeType totalComponents = this->GetImageSizeInComponents();
  const SizeType totalBytes = this->GetImageSizeInBytes();
  const SizeType componentSize = this->GetComponentSize();

  TensorRemapGuard tensorGuard{
    buffer,
    static_cast<std::size_t>(totalComponents / 6),
    static_cast<std::size_t>(componentSize),
    this->GetPixelType() == IOPixelEnum::SYMMETRICSECONDRANKTENSOR,
  };

  if (m_DataEncoding == DataEncoding::ASCII)
  {
    if (m_AsciiDataContent.empty())
    {
      itkExceptionMacro("ASCII DataArray content is empty in file: " << m_FileName);
    }
    std::istringstream is(m_AsciiDataContent);
    this->ReadBufferAsASCII(is, buffer, this->GetComponentType(), totalComponents);
    return;
  }

  if (m_DataEncoding == DataEncoding::Base64 || m_DataEncoding == DataEncoding::ZLibBase64)
  {
    if (m_Base64DataContent.empty())
    {
      itkExceptionMacro("Base64 DataArray content is empty in file: " << m_FileName);
    }
    std::vector<unsigned char> decoded;
    DecodeBase64(m_Base64DataContent, decoded);

    if (m_DataEncoding == DataEncoding::ZLibBase64)
    {
      // Compressed: the decoded bytes ARE the compression header + compressed blocks.
      std::vector<unsigned char> uncompressed;
      DecompressZLib(decoded.data(), decoded.size(), m_HeaderTypeUInt64, uncompressed);
      if (uncompressed.size() < static_cast<std::size_t>(totalBytes))
      {
        itkExceptionMacro("Decompressed data size (" << uncompressed.size() << ") is less than expected (" << totalBytes
                                                     << ") in file: " << m_FileName);
      }
      std::memcpy(buffer, uncompressed.data(), static_cast<std::size_t>(totalBytes));
    }
    else
    {
      // Uncompressed base64: VTK binary DataArrays are prefixed with a block header
      // containing the number of bytes of data.  Header is UInt32 or UInt64.
      const std::size_t headerBytes = m_HeaderTypeUInt64 ? sizeof(uint64_t) : sizeof(uint32_t);
      if (decoded.size() <= headerBytes)
      {
        itkExceptionMacro("Decoded base64 data is too short in file: " << m_FileName);
      }
      const unsigned char * dataPtr = decoded.data() + headerBytes;
      const std::size_t     dataSize = decoded.size() - headerBytes;
      if (dataSize < static_cast<std::size_t>(totalBytes))
      {
        itkExceptionMacro("Decoded data size (" << dataSize << ") is less than expected (" << totalBytes
                                                << ") in file: " << m_FileName);
      }
      std::memcpy(buffer, dataPtr, static_cast<std::size_t>(totalBytes));
    }
    SwapBufferIfNeeded(buffer, componentSize, totalComponents, m_ByteOrder);
    return;
  }

  // Base64Appended or ZLibBase64Appended: appended data is base64-encoded
  if (m_DataEncoding == DataEncoding::Base64Appended || m_DataEncoding == DataEncoding::ZLibBase64Appended)
  {
    if (m_AppendedBase64Content.empty())
    {
      itkExceptionMacro("Base64 appended content is empty in file: " << m_FileName);
    }

    // Decode the base64 content
    std::vector<unsigned char> decoded;
    DecodeBase64(m_AppendedBase64Content, decoded);

    // The decoded buffer contains all appended data arrays; we need to extract
    // the portion at offset m_DataArrayOffset
    const std::size_t headerBytes = m_HeaderTypeUInt64 ? sizeof(uint64_t) : sizeof(uint32_t);
    const std::size_t arrayStart = static_cast<std::size_t>(m_DataArrayOffset);

    if (arrayStart + headerBytes > decoded.size())
    {
      itkExceptionMacro("Appended data offset extends beyond decoded buffer in file: " << m_FileName);
    }

    if (m_DataEncoding == DataEncoding::ZLibBase64Appended)
    {
      // Compressed: read nblocks to determine full header size
      uint64_t nblocks;
      if (m_HeaderTypeUInt64)
      {
        std::memcpy(&nblocks, decoded.data() + arrayStart, sizeof(uint64_t));
      }
      else
      {
        uint32_t nb32;
        std::memcpy(&nb32, decoded.data() + arrayStart, sizeof(uint32_t));
        nblocks = nb32;
      }

      // Calculate total header size and extract compressed block
      const std::size_t compHeaderSize = static_cast<std::size_t>((3 + nblocks) * headerBytes);
      if (arrayStart + compHeaderSize > decoded.size())
      {
        itkExceptionMacro("Compressed header extends beyond decoded buffer in file: " << m_FileName);
      }

      // Sum the compressed block sizes
      uint64_t totalCompressed = 0;
      for (uint64_t b = 0; b < nblocks; ++b)
      {
        if (m_HeaderTypeUInt64)
        {
          uint64_t cs;
          std::memcpy(&cs, decoded.data() + arrayStart + (3 + b) * sizeof(uint64_t), sizeof(uint64_t));
          totalCompressed += cs;
        }
        else
        {
          uint32_t cs;
          std::memcpy(&cs, decoded.data() + arrayStart + (3 + b) * sizeof(uint32_t), sizeof(uint32_t));
          totalCompressed += cs;
        }
      }

      const std::size_t compDataSize = static_cast<std::size_t>(compHeaderSize + totalCompressed);
      if (arrayStart + compDataSize > decoded.size())
      {
        itkExceptionMacro("Compressed data extends beyond decoded buffer in file: " << m_FileName);
      }

      // Decompress
      std::vector<unsigned char> uncompressed;
      DecompressZLib(decoded.data() + arrayStart, compDataSize, m_HeaderTypeUInt64, uncompressed);
      if (uncompressed.size() < static_cast<std::size_t>(totalBytes))
      {
        itkExceptionMacro("Decompressed data size (" << uncompressed.size() << ") is less than expected (" << totalBytes
                                                     << ") in file: " << m_FileName);
      }
      std::memcpy(buffer, uncompressed.data(), static_cast<std::size_t>(totalBytes));
    }
    else
    {
      // Uncompressed base64 appended: skip block-size header and read data
      if (arrayStart + headerBytes + static_cast<std::size_t>(totalBytes) > decoded.size())
      {
        itkExceptionMacro("Appended data extends beyond decoded buffer in file: " << m_FileName);
      }
      const unsigned char * dataPtr = decoded.data() + arrayStart + headerBytes;
      std::memcpy(buffer, dataPtr, static_cast<std::size_t>(totalBytes));
    }

    SwapBufferIfNeeded(buffer, componentSize, totalComponents, m_ByteOrder);
    return;
  }

  // RawAppended path (compressed or uncompressed): seek into the file.
  std::ifstream file(m_FileName.c_str(), std::ios::in | std::ios::binary);
  if (!file.is_open())
  {
    itkExceptionMacro("Cannot open file for reading: " << m_FileName);
  }

  const std::size_t    headerBytes = m_HeaderTypeUInt64 ? sizeof(uint64_t) : sizeof(uint32_t);
  const std::streampos readPos = m_AppendedDataOffset + static_cast<std::streamoff>(m_DataArrayOffset);

  file.seekg(readPos, std::ios::beg);
  if (file.fail())
  {
    itkExceptionMacro("Failed to seek to data position in file: " << m_FileName);
  }

  if (m_DataEncoding == DataEncoding::ZLibAppended)
  {
    // Read the full compressed block sequence into memory.
    // We need to read the nblocks field first to know how big the header is,
    // then read all the compressed blocks.
    std::vector<unsigned char> firstItem(headerBytes);
    file.read(reinterpret_cast<char *>(firstItem.data()), static_cast<std::streamsize>(headerBytes));
    if (file.fail())
    {
      itkExceptionMacro("Failed to read zlib compression header from file: " << m_FileName);
    }
    uint64_t nblocks;
    if (m_HeaderTypeUInt64)
    {
      std::memcpy(&nblocks, firstItem.data(), sizeof(uint64_t));
    }
    else
    {
      uint32_t nb32;
      std::memcpy(&nb32, firstItem.data(), sizeof(uint32_t));
      nblocks = nb32;
    }

    // Read the rest of the header: uncompressed_blocksize, last_partial_blocksize,
    // plus nblocks compressed sizes.
    const std::size_t          remainingHeaderBytes = static_cast<std::size_t>((2 + nblocks) * headerBytes);
    std::vector<unsigned char> headerBuf(headerBytes + remainingHeaderBytes);
    std::memcpy(headerBuf.data(), firstItem.data(), headerBytes);
    file.read(reinterpret_cast<char *>(headerBuf.data() + headerBytes),
              static_cast<std::streamsize>(remainingHeaderBytes));
    if (file.fail())
    {
      itkExceptionMacro("Failed to read zlib compression block sizes from file: " << m_FileName);
    }

    // Sum the compressed block sizes to know how many bytes of payload to read.
    uint64_t totalCompressed = 0;
    for (uint64_t b = 0; b < nblocks; ++b)
    {
      if (m_HeaderTypeUInt64)
      {
        uint64_t cs;
        std::memcpy(&cs, headerBuf.data() + (3 + b) * sizeof(uint64_t), sizeof(uint64_t));
        totalCompressed += cs;
      }
      else
      {
        uint32_t cs;
        std::memcpy(&cs, headerBuf.data() + (3 + b) * sizeof(uint32_t), sizeof(uint32_t));
        totalCompressed += cs;
      }
    }

    // Read compressed payload.
    std::vector<unsigned char> compressedPayload(static_cast<std::size_t>(totalCompressed));
    file.read(reinterpret_cast<char *>(compressedPayload.data()), static_cast<std::streamsize>(totalCompressed));
    if (file.fail())
    {
      itkExceptionMacro("Failed to read zlib compressed data from file: " << m_FileName);
    }

    // Build the full buffer that DecompressZLib expects: header + payload.
    std::vector<unsigned char> fullBuf(headerBuf.size() + compressedPayload.size());
    std::memcpy(fullBuf.data(), headerBuf.data(), headerBuf.size());
    std::memcpy(fullBuf.data() + headerBuf.size(), compressedPayload.data(), compressedPayload.size());

    std::vector<unsigned char> uncompressed;
    DecompressZLib(fullBuf.data(), fullBuf.size(), m_HeaderTypeUInt64, uncompressed);
    if (uncompressed.size() < static_cast<std::size_t>(totalBytes))
    {
      itkExceptionMacro("Decompressed data size (" << uncompressed.size() << ") is less than expected (" << totalBytes
                                                   << ") in file: " << m_FileName);
    }
    std::memcpy(buffer, uncompressed.data(), static_cast<std::size_t>(totalBytes));
    SwapBufferIfNeeded(buffer, componentSize, totalComponents, m_ByteOrder);
    return;
  }

  // Plain RawAppended: skip the block-size header and read directly.
  file.seekg(static_cast<std::streamoff>(headerBytes), std::ios::cur);
  if (file.fail())
  {
    itkExceptionMacro("Failed to seek past block header in file: " << m_FileName);
  }

  file.read(static_cast<char *>(buffer), static_cast<std::streamsize>(totalBytes));
  if (file.fail())
  {
    itkExceptionMacro("Failed to read raw appended data from file: " << m_FileName);
  }

  SwapBufferIfNeeded(buffer, componentSize, totalComponents, m_ByteOrder);
}

// ---------------------------------------------------------------------------
// Write
// ---------------------------------------------------------------------------
void
VTIImageIO::Write(const void * buffer)
{
  const unsigned int numDims = this->GetNumberOfDimensions();
  if (numDims < 1 || numDims > 3)
  {
    itkExceptionMacro("VTIImageIO can only write 1, 2 or 3-dimensional images");
  }

  const auto nx = static_cast<unsigned int>(this->GetDimensions(0));
  const auto ny = (numDims > 1) ? static_cast<unsigned int>(this->GetDimensions(1)) : 1u;
  const auto nz = (numDims > 2) ? static_cast<unsigned int>(this->GetDimensions(2)) : 1u;

  const double ox = this->GetOrigin(0);
  const double oy = (numDims > 1) ? this->GetOrigin(1) : 0.0;
  const double oz = (numDims > 2) ? this->GetOrigin(2) : 0.0;

  const double sx = this->GetSpacing(0);
  const double sy = (numDims > 1) ? this->GetSpacing(1) : 1.0;
  const double sz = (numDims > 2) ? this->GetSpacing(2) : 1.0;

  const SizeType totalBytes = this->GetImageSizeInBytes();
  const SizeType totalComponents = this->GetImageSizeInComponents();

  // Determine attribute name and type
  const IOPixelEnum     pixelType = this->GetPixelType();
  const unsigned int    numComp = this->GetNumberOfComponents();
  const IOComponentEnum compType = this->GetComponentType();
  const std::string     vtkType = ITKComponentToVTKTypeString(compType);

  std::string attributeElement;
  std::string dataArrayName;
  std::string pointDataAttr;
  if (pixelType == IOPixelEnum::SYMMETRICSECONDRANKTENSOR)
  {
    dataArrayName = "tensors";
    pointDataAttr = "Tensors=\"tensors\"";
    // VTK canonical symmetric-tensor layout: 6 components per pixel in
    // [XX, YY, ZZ, XY, YZ, XZ] order.  ASCII writer remaps from ITK's
    // [e00, e01, e02, e11, e12, e22] layout below.  Binary writing is
    // deferred to F-007 in the follow-up PR.
    attributeElement = "NumberOfComponents=\"6\"";
  }
  else if (pixelType == IOPixelEnum::VECTOR && numComp == 3)
  {
    dataArrayName = "vectors";
    pointDataAttr = "Vectors=\"vectors\"";
    std::ostringstream tmp;
    tmp << "NumberOfComponents=\"" << numComp << "\"";
    attributeElement = tmp.str();
  }
  else if ((pixelType == IOPixelEnum::RGB && numComp == 3) || (pixelType == IOPixelEnum::RGBA && numComp == 4))
  {
    dataArrayName = "scalars";
    pointDataAttr = "Scalars=\"scalars\"";
    std::ostringstream tmp;
    tmp << "NumberOfComponents=\"" << numComp << "\"";
    attributeElement = tmp.str();
  }
  else
  {
    dataArrayName = "scalars";
    pointDataAttr = "Scalars=\"scalars\"";
    if (numComp > 1)
    {
      std::ostringstream tmp;
      tmp << "NumberOfComponents=\"" << numComp << "\"";
      attributeElement = tmp.str();
    }
  }

  // F-007: Binary symmetric-tensor writing is deferred to the follow-up
  // PR.  The ASCII writer below already emits the VTK-canonical
  // 6-component [XX, YY, ZZ, XY, YZ, XZ] layout, so when F-007 lands the
  // binary path only needs to mirror that remap (no layout re-decision).
  if (pixelType == IOPixelEnum::SYMMETRICSECONDRANKTENSOR && m_FileType != IOFileEnum::ASCII)
  {
    itkExceptionMacro("F-007 Binary symmetric-tensor writer not yet implemented.  "
                      "The ASCII path emits VTK-canonical 6-component layout "
                      "[XX, YY, ZZ, XY, YZ, XZ]; call SetFileTypeToASCII() for "
                      "tensor output until the follow-up PR adds binary support.");
  }

  // Prepare a byte-swapped copy if the system is big-endian (we always
  // write little-endian binary).
  const char *               dataToWrite = static_cast<const char *>(buffer);
  std::vector<unsigned char> swapBuf;

  const bool needsSwap =
    ByteSwapper<uint16_t>::SystemIsBigEndian() && this->GetComponentSize() > 1 && m_FileType != IOFileEnum::ASCII;
  if (needsSwap)
  {
    swapBuf.resize(static_cast<std::size_t>(totalBytes));
    std::memcpy(swapBuf.data(), buffer, static_cast<std::size_t>(totalBytes));
    switch (this->GetComponentSize())
    {
      case 2:
        ByteSwapper<uint16_t>::SwapRangeFromSystemToBigEndian(reinterpret_cast<uint16_t *>(swapBuf.data()),
                                                              totalComponents);
        break;
      case 4:
        ByteSwapper<uint32_t>::SwapRangeFromSystemToBigEndian(reinterpret_cast<uint32_t *>(swapBuf.data()),
                                                              totalComponents);
        break;
      case 8:
        ByteSwapper<uint64_t>::SwapRangeFromSystemToBigEndian(reinterpret_cast<uint64_t *>(swapBuf.data()),
                                                              totalComponents);
        break;
      default:
        break;
    }
    dataToWrite = reinterpret_cast<const char *>(swapBuf.data());
  }

  std::ofstream file(m_FileName.c_str(), std::ios::out | std::ios::binary | std::ios::trunc);
  if (!file.is_open())
  {
    itkExceptionMacro("Cannot open file for writing: " << m_FileName);
  }

  file.precision(16);

  const std::string byteOrderStr = ByteSwapper<uint16_t>::SystemIsBigEndian() ? "BigEndian" : "LittleEndian";

  // Compose a 3x3 row-major Direction matrix from ITK's per-axis direction
  // vectors, padding with identity rows/columns when image dimensionality
  // is less than 3 (VTK always expects a full 3x3 Direction).
  const unsigned int nd = this->GetNumberOfDimensions();
  double             dm[3][3] = { { 1.0, 0.0, 0.0 }, { 0.0, 1.0, 0.0 }, { 0.0, 0.0, 1.0 } };
  for (unsigned int axis = 0; axis < nd && axis < 3; ++axis)
  {
    const std::vector<double> & v = this->GetDirection(axis);
    for (unsigned int r = 0; r < nd && r < 3; ++r)
    {
      dm[r][axis] = v[r];
    }
  }

  // Determine write mode.  When UseCompression is set and the output is not
  // ASCII, write appended raw + vtkZLibDataCompressor (smallest on-disk size,
  // Dženan's recommended next step in the follow-up discussion).
  const bool useCompressedAppended = (m_UseCompression && m_FileType != IOFileEnum::ASCII);

  // XML header -- emit the modern VTK 9 / ParaView 5.7+ attribute set so
  // round-trip through ParaView does not silently demote the version or
  // truncate block headers.  header_type="UInt64" is paired with the
  // uint64_t block-size prefix used in the binary writer below.
  file << "<?xml version=\"1.0\"?>\n";
  file << "<VTKFile type=\"ImageData\" version=\"1.0\" byte_order=\"" << byteOrderStr << "\" header_type=\"UInt64\"";
  if (useCompressedAppended)
  {
    file << " compressor=\"vtkZLibDataCompressor\"";
  }
  file << ">\n";
  file << "  <ImageData WholeExtent=\""
       << "0 " << (nx - 1) << " 0 " << (ny - 1) << " 0 " << (nz - 1) << "\""
       << " Origin=\"" << ox << " " << oy << " " << oz << "\""
       << " Spacing=\"" << sx << " " << sy << " " << sz << "\""
       << " Direction=\"" << dm[0][0] << " " << dm[0][1] << " " << dm[0][2] << " " << dm[1][0] << " " << dm[1][1] << " "
       << dm[1][2] << " " << dm[2][0] << " " << dm[2][1] << " " << dm[2][2] << "\">\n";
  file << "    <Piece Extent=\""
       << "0 " << (nx - 1) << " 0 " << (ny - 1) << " 0 " << (nz - 1) << "\">\n";
  file << "      <PointData " << pointDataAttr << ">\n";

  if (m_FileType == IOFileEnum::ASCII)
  {
    file << "        <DataArray type=\"" << vtkType << "\" Name=\"" << dataArrayName << "\" format=\"ascii\"";
    if (!attributeElement.empty())
    {
      file << " " << attributeElement;
    }
    file << ">\n";

    if (pixelType == IOPixelEnum::SYMMETRICSECONDRANKTENSOR)
    {
      // Emit VTK-canonical 6-component symmetric tensor: [XX, YY, ZZ, XY, YZ, XZ].
      // ITK's SymmetricSecondRankTensor<T,3> is stored as
      // [e00, e01, e02, e11, e12, e22] (upper-triangular row-major); remap:
      //   VTK[0] XX = ITK[0] e00    VTK[3] XY = ITK[1] e01
      //   VTK[1] YY = ITK[3] e11    VTK[4] YZ = ITK[4] e12
      //   VTK[2] ZZ = ITK[5] e22    VTK[5] XZ = ITK[2] e02
      const SizeType numPixels = totalComponents / 6;
      for (SizeType p = 0; p < numPixels; ++p)
      {
        if (compType == IOComponentEnum::FLOAT)
        {
          const float * fPtr = static_cast<const float *>(buffer) + p * 6;
          file << fPtr[0] << ' ' << fPtr[3] << ' ' << fPtr[5] << ' ' << fPtr[1] << ' ' << fPtr[4] << ' ' << fPtr[2]
               << '\n';
        }
        else
        {
          const double * dPtr = static_cast<const double *>(buffer) + p * 6;
          file << dPtr[0] << ' ' << dPtr[3] << ' ' << dPtr[5] << ' ' << dPtr[1] << ' ' << dPtr[4] << ' ' << dPtr[2]
               << '\n';
        }
      }
    }
    else
    {
      this->WriteBufferAsASCII(file, buffer, compType, totalComponents);
    }

    file << "\n        </DataArray>\n";
    file << "      </PointData>\n";
    file << "    </Piece>\n";
    file << "  </ImageData>\n";
    file << "</VTKFile>\n";
  }
  else if (useCompressedAppended)
  {
    // Appended raw + vtkZLibDataCompressor: single DataArray element
    // referencing offset 0 in the <AppendedData> block.
    file << "        <DataArray type=\"" << vtkType << "\" Name=\"" << dataArrayName
         << "\" format=\"appended\" offset=\"0\"";
    if (!attributeElement.empty())
    {
      file << " " << attributeElement;
    }
    file << "/>\n";
    file << "      </PointData>\n";
    file << "    </Piece>\n";
    file << "  </ImageData>\n";
    file << "  <AppendedData encoding=\"raw\">\n_";

    const std::vector<unsigned char> compressed =
      CompressZLibVTK(reinterpret_cast<const unsigned char *>(dataToWrite), static_cast<std::size_t>(totalBytes));
    file.write(reinterpret_cast<const char *>(compressed.data()), static_cast<std::streamsize>(compressed.size()));

    file << "\n  </AppendedData>\n";
    file << "</VTKFile>\n";
  }
  else // Binary (inline base64)
  {
    file << "        <DataArray type=\"" << vtkType << "\" Name=\"" << dataArrayName << "\" format=\"binary\"";
    if (!attributeElement.empty())
    {
      file << " " << attributeElement;
    }
    file << ">\n";

    // Prepend a UInt64 block-size header (number of raw data bytes).  The
    // <VTKFile> attribute declares header_type="UInt64" above, matching
    // ParaView 5.7+ defaults and allowing images > 4 GiB without silent
    // truncation.
    const auto                 blockSize = static_cast<uint64_t>(totalBytes);
    std::vector<unsigned char> toEncode(sizeof(blockSize) + static_cast<std::size_t>(totalBytes));
    std::memcpy(toEncode.data(), &blockSize, sizeof(blockSize));
    std::memcpy(toEncode.data() + sizeof(blockSize), dataToWrite, static_cast<std::size_t>(totalBytes));

    file << "        " << EncodeBase64(toEncode.data(), static_cast<SizeType>(toEncode.size())) << "\n";
    file << "        </DataArray>\n";
    file << "      </PointData>\n";
    file << "    </Piece>\n";
    file << "  </ImageData>\n";
    file << "</VTKFile>\n";
  }

  if (file.fail())
  {
    itkExceptionMacro("Failed to write VTI file: " << m_FileName);
  }
}

} // end namespace itk
