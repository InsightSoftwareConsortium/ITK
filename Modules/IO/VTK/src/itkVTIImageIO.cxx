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

// ---------------------------------------------------------------------------
// Base64 lookup tables
// ---------------------------------------------------------------------------
namespace
{
const char s_B64Chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
const int  s_B64Inv[256] = {
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 0-15
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 16-31
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63, // 32-47
  52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, 0,  -1, -1, // 48-63
  -1, 0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, // 64-79
  15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1, // 80-95
  -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, // 96-111
  41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1, // 112-127
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 128-143
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 144-159
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 160-175
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 176-191
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 192-207
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 208-223
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 224-239
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  // 240-255
};

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

  // <ImageData ...>
  bool        sawImageData{ false };
  std::string wholeExtent;
  std::string origin;
  std::string spacing;

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
  bool sawAppendedData{ false };
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
    }
    else if (std::strcmp(name, "ImageData") == 0)
    {
      st->sawImageData = true;
      st->wholeExtent = FindAttribute(atts, "WholeExtent");
      st->origin = FindAttribute(atts, "Origin");
      st->spacing = FindAttribute(atts, "Spacing");
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
      // We do not consume any character data from inside AppendedData via
      // expat -- the binary content following the `_` marker is XML-illegal
      // and is read directly from the file by the caller.  We don't even
      // get this far on a real raw-appended file because the binary bytes
      // would have caused a parser error before reaching this start tag.
      // The caller pre-truncates the XML at <AppendedData> to handle that.
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

// Read entire file into a string.
std::string
SlurpFile(const std::string & path)
{
  std::ifstream file(path.c_str(), std::ios::in | std::ios::binary);
  if (!file.is_open())
  {
    return {};
  }
  std::ostringstream ss;
  ss << file.rdbuf();
  return ss.str();
}

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
  decoded.clear();
  decoded.reserve((encoded.size() / 4) * 3 + 4);

  int bits = 0;
  int val = 0;
  for (unsigned char c : encoded)
  {
    if (c == '=' || std::isspace(c))
    {
      continue;
    }
    const int d = s_B64Inv[c];
    if (d == -1)
    {
      continue;
    }
    val = (val << 6) | d;
    bits += 6;
    if (bits >= 8)
    {
      bits -= 8;
      decoded.push_back(static_cast<unsigned char>((val >> bits) & 0xFF));
    }
  }
  return static_cast<SizeType>(decoded.size());
}

std::string
VTIImageIO::EncodeBase64(const unsigned char * data, SizeType numBytes)
{
  std::string out;
  out.reserve(((numBytes + 2) / 3) * 4 + 1);

  for (SizeType i = 0; i < numBytes; i += 3)
  {
    const unsigned int b0 = data[i];
    const unsigned int b1 = (i + 1 < numBytes) ? data[i + 1] : 0u;
    const unsigned int b2 = (i + 2 < numBytes) ? data[i + 2] : 0u;
    out += s_B64Chars[(b0 >> 2) & 0x3F];
    out += s_B64Chars[((b0 << 4) | (b1 >> 4)) & 0x3F];
    out += (i + 1 < numBytes) ? s_B64Chars[((b1 << 2) | (b2 >> 6)) & 0x3F] : '=';
    out += (i + 2 < numBytes) ? s_B64Chars[b2 & 0x3F] : '=';
  }
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
      return "Int64";
    case IOComponentEnum::ULONG:
      return "UInt64";
    case IOComponentEnum::LONGLONG:
      return "Int64";
    case IOComponentEnum::ULONGLONG:
      return "UInt64";
    case IOComponentEnum::FLOAT:
      return "Float32";
    case IOComponentEnum::DOUBLE:
      return "Float64";
    default:
    {
      ExceptionObject e_(__FILE__, __LINE__, "Unsupported component type for VTI writing.", ITK_LOCATION);
      throw e_;
    }
  }
}

// ---------------------------------------------------------------------------
// ReadImageInformation
// ---------------------------------------------------------------------------
void
VTIImageIO::InternalReadImageInformation()
{
  // Reset cached parser results.
  m_AsciiDataContent.clear();
  m_Base64DataContent.clear();
  m_AppendedDataOffset = 0;
  m_DataArrayOffset = 0;
  m_HeaderTypeUInt64 = false;

  // Slurp the entire file.  We need both the XML metadata (parsed by expat)
  // and, for raw-appended files, the byte offset of the binary section.
  const std::string content = SlurpFile(m_FileName);
  if (content.empty())
  {
    itkExceptionMacro("Cannot open or read file: " << m_FileName);
  }

  // The VTK XML appended-data section embeds raw binary AFTER an `_` marker
  // inside an `<AppendedData>` element.  These bytes are XML-illegal, so we
  // hand expat only the prefix of the file up to and including the
  // `<AppendedData ...>` start tag, and read the binary block directly.
  //
  // We also record the absolute byte offset (in the original file) of the
  // first byte after `_`, which is where the appended block begins.
  std::string       xmlPortion;
  const std::size_t appPos = content.find("<AppendedData");
  if (appPos == std::string::npos)
  {
    xmlPortion = content;
  }
  else
  {
    // Locate the end of the <AppendedData ...> start tag.
    const std::size_t startTagEnd = content.find('>', appPos);
    if (startTagEnd == std::string::npos)
    {
      itkExceptionMacro("Malformed <AppendedData> tag in file: " << m_FileName);
    }

    // Construct an XML view that ends with a self-closing <AppendedData/>
    // and a closing </VTKFile>, so expat sees a well-formed document.
    xmlPortion = content.substr(0, appPos) + "<AppendedData/></VTKFile>";

    // Locate the `_` marker that introduces the raw binary stream.
    const std::size_t underscorePos = content.find('_', startTagEnd);
    if (underscorePos == std::string::npos)
    {
      itkExceptionMacro("Missing `_` marker in <AppendedData> section of: " << m_FileName);
    }
    m_AppendedDataOffset = static_cast<std::streampos>(underscorePos + 1);
  }

  // Run expat over the XML portion.
  VTIParseState st;
  XML_Parser    parser = XML_ParserCreate(nullptr);
  if (parser == nullptr)
  {
    itkExceptionMacro("Failed to create expat XML parser.");
  }

  XML_SetUserData(parser, &st);
  XML_SetElementHandler(parser, &VTIStartElement, &VTIEndElement);
  XML_SetCharacterDataHandler(parser, &VTICharData);

  const auto parseResult = XML_Parse(parser, xmlPortion.c_str(), static_cast<int>(xmlPortion.size()), /*isFinal=*/1);
  if (parseResult == 0)
  {
    const std::string err = XML_ErrorString(XML_GetErrorCode(parser));
    XML_ParserFree(parser);
    itkExceptionMacro("XML parse error in " << m_FileName << ": " << err);
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

  // Component type
  const IOComponentEnum compType = VTKTypeStringToITKComponent(st.daType);
  if (compType == IOComponentEnum::UNKNOWNCOMPONENTTYPE)
  {
    itkExceptionMacro("Unknown VTK DataArray type '" << st.daType << "' in file: " << m_FileName);
  }
  this->SetComponentType(compType);

  // Number of components
  const unsigned int numComp =
    st.daNumberOfComponents.empty() ? 1u : static_cast<unsigned int>(std::stoul(st.daNumberOfComponents));
  this->SetNumberOfComponents(numComp);

  // Pixel type, derived from the active PointData attribute and component count
  const bool isTensor = !st.activeTensors.empty() && st.daName == st.activeTensors;
  const bool isVector = !st.activeVectors.empty() && st.daName == st.activeVectors;
  if (isTensor)
  {
    // VTK tensors are 3x3 = 9 components on disk; ITK uses 6 (symmetric).
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
    m_DataEncoding = DataEncoding::RawAppended;
    m_FileType = IOFileEnum::Binary;
    m_DataArrayOffset = st.daOffset.empty() ? 0u : static_cast<SizeType>(std::stoull(st.daOffset));
  }
  else // "binary" (base64) or unspecified, defaulting to binary
  {
    m_DataEncoding = DataEncoding::Base64;
    m_FileType = IOFileEnum::Binary;
    m_Base64DataContent = std::move(st.base64Content);
  }
}

void
VTIImageIO::ReadImageInformation()
{
  this->InternalReadImageInformation();
}

namespace
{
// Byte-swap a buffer in-place to/from little-endian based on the file's
// byte order and the system byte order.
void
SwapBufferIfNeeded(void * buffer, std::size_t componentSize, std::size_t numComponents, IOByteOrderEnum fileOrder)
{
  const bool fileBigEndian = (fileOrder == IOByteOrderEnum::BigEndian);
  const bool sysBigEndian = ByteSwapper<uint16_t>::SystemIsBigEndian();
  if (fileBigEndian == sysBigEndian)
  {
    return;
  }
  switch (componentSize)
  {
    case 1:
      break;
    case 2:
      ByteSwapper<uint16_t>::SwapRangeFromSystemToBigEndian(static_cast<uint16_t *>(buffer), numComponents);
      break;
    case 4:
      ByteSwapper<uint32_t>::SwapRangeFromSystemToBigEndian(static_cast<uint32_t *>(buffer), numComponents);
      break;
    case 8:
      ByteSwapper<uint64_t>::SwapRangeFromSystemToBigEndian(static_cast<uint64_t *>(buffer), numComponents);
      break;
    default:
    {
      ExceptionObject e_(__FILE__, __LINE__, "Unknown component size for byte swap.", ITK_LOCATION);
      throw e_;
    }
  }
}
} // anonymous namespace

// ---------------------------------------------------------------------------
// Read
// ---------------------------------------------------------------------------
void
VTIImageIO::Read(void * buffer)
{
  const SizeType totalComponents = this->GetImageSizeInComponents();
  const SizeType totalBytes = this->GetImageSizeInBytes();
  const SizeType componentSize = this->GetComponentSize();

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

  if (m_DataEncoding == DataEncoding::Base64)
  {
    if (m_Base64DataContent.empty())
    {
      itkExceptionMacro("Base64 DataArray content is empty in file: " << m_FileName);
    }
    std::vector<unsigned char> decoded;
    DecodeBase64(m_Base64DataContent, decoded);

    // VTK binary DataArrays are prefixed with a block header containing
    // the number of bytes of (uncompressed) data.  The header is either
    // one UInt32 or one UInt64.
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
    SwapBufferIfNeeded(buffer, componentSize, totalComponents, m_ByteOrder);
    return;
  }

  // RawAppended path: seek into the file at the cached byte offset.
  std::ifstream file(m_FileName.c_str(), std::ios::in | std::ios::binary);
  if (!file.is_open())
  {
    itkExceptionMacro("Cannot open file for reading: " << m_FileName);
  }

  const std::size_t    headerBytes = m_HeaderTypeUInt64 ? sizeof(uint64_t) : sizeof(uint32_t);
  const std::streampos readPos =
    m_AppendedDataOffset + static_cast<std::streamoff>(m_DataArrayOffset) + static_cast<std::streamoff>(headerBytes);

  file.seekg(readPos, std::ios::beg);
  if (file.fail())
  {
    itkExceptionMacro("Failed to seek to data position in file: " << m_FileName);
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
    // VTK tensors on disk are full 3x3 (9 components per pixel) but binary
    // writing currently only supports the ASCII path which expands the
    // symmetric (6-component) layout to 9.  Binary writing of tensors is
    // disallowed below.
    attributeElement = "NumberOfComponents=\"9\"";
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

  // Refuse to write tensors in binary form because the on-disk layout
  // (NumberOfComponents="9") and the in-memory ITK layout (6) disagree;
  // we'd silently produce a corrupt file.
  if (pixelType == IOPixelEnum::SYMMETRICSECONDRANKTENSOR && m_FileType != IOFileEnum::ASCII)
  {
    itkExceptionMacro("VTIImageIO does not yet support binary writing of "
                      "SymmetricSecondRankTensor pixel types; set FileType to ASCII.");
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

  // XML header
  file << "<?xml version=\"1.0\"?>\n";
  file << "<VTKFile type=\"ImageData\" version=\"0.1\" byte_order=\"" << byteOrderStr << "\">\n";
  file << "  <ImageData WholeExtent=\""
       << "0 " << (nx - 1) << " 0 " << (ny - 1) << " 0 " << (nz - 1) << "\""
       << " Origin=\"" << ox << " " << oy << " " << oz << "\""
       << " Spacing=\"" << sx << " " << sy << " " << sz << "\">\n";
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
      // Expand symmetric tensor (6 components) to full 3x3 = 9 for VTK.
      // Layout convention: e11 e12 e13 e22 e23 e33.
      const SizeType numPixels = totalComponents / 6;
      for (SizeType p = 0; p < numPixels; ++p)
      {
        if (compType == IOComponentEnum::FLOAT)
        {
          const float * fPtr = static_cast<const float *>(buffer) + p * 6;
          file << fPtr[0] << ' ' << fPtr[1] << ' ' << fPtr[2] << '\n';
          file << fPtr[1] << ' ' << fPtr[3] << ' ' << fPtr[4] << '\n';
          file << fPtr[2] << ' ' << fPtr[4] << ' ' << fPtr[5] << '\n';
        }
        else
        {
          const double * dPtr = static_cast<const double *>(buffer) + p * 6;
          file << dPtr[0] << ' ' << dPtr[1] << ' ' << dPtr[2] << '\n';
          file << dPtr[1] << ' ' << dPtr[3] << ' ' << dPtr[4] << '\n';
          file << dPtr[2] << ' ' << dPtr[4] << ' ' << dPtr[5] << '\n';
        }
      }
    }
    else
    {
      this->WriteBufferAsASCII(file, buffer, compType, totalComponents);
    }

    file << "\n        </DataArray>\n";
  }
  else // Binary (base64)
  {
    file << "        <DataArray type=\"" << vtkType << "\" Name=\"" << dataArrayName << "\" format=\"binary\"";
    if (!attributeElement.empty())
    {
      file << " " << attributeElement;
    }
    file << ">\n";

    // Prepend a UInt32 block-size header (number of raw data bytes).
    const auto                 blockSize = static_cast<uint32_t>(totalBytes);
    std::vector<unsigned char> toEncode(sizeof(blockSize) + static_cast<std::size_t>(totalBytes));
    std::memcpy(toEncode.data(), &blockSize, sizeof(blockSize));
    std::memcpy(toEncode.data() + sizeof(blockSize), dataToWrite, static_cast<std::size_t>(totalBytes));

    file << "        " << EncodeBase64(toEncode.data(), static_cast<SizeType>(toEncode.size())) << "\n";
    file << "        </DataArray>\n";
  }

  file << "      </PointData>\n";
  file << "    </Piece>\n";
  file << "  </ImageData>\n";
  file << "</VTKFile>\n";

  if (file.fail())
  {
    itkExceptionMacro("Failed to write VTI file: " << m_FileName);
  }
}

} // end namespace itk
