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
#include "itkBruker2dseqImageIO.h"
#include "itkMacro.h"
#include "itkIOCommon.h"
#include "itkByteSwapper.h"
#include "itksys/SystemTools.hxx"
#include "itkMetaDataObject.h"
#include "itkStringConvert.h"
#include "itkPrintHelper.h"

#include <cctype>

namespace itk
{

#define BRUKER_LITTLE_ENDIAN "littleEndian"
#define BRUKER_BIG_ENDIAN "bigEndian"
#define BRUKER_SIGNED_CHAR "_8BIT_SGN_INT"
#define BRUKER_UNSIGNED_CHAR "_8BIT_UNSGN_INT"
#define BRUKER_SIGNED_SHORT "_16BIT_SGN_INT"
#define BRUKER_SIGNED_INT "_32BIT_SGN_INT"
#define BRUKER_FLOAT "_32BIT_FLOAT"

namespace
{
using SizeType = ImageIOBase::SizeType;

// Internal function to throw an exception if a needed parameter does not exist
template <typename T>
T
GetParameter(const itk::MetaDataDictionary & dict, const std::string & name)
{
  T value;
  if (!ExposeMetaData(dict, name, value))
  {
    itkGenericExceptionMacro("Could not read parameter: " << name);
  }
  return value;
}

// Parameter may be absent, hold a single value for all frames, or one value per frame
std::vector<double>
ReadDoubleArray(const MetaDataDictionary & dict, const char * name, double defaultValue)
{
  std::vector<double> values;
  if (ExposeMetaData(dict, name, values) && !values.empty())
  {
    return values;
  }
  double value = defaultValue;
  ExposeMetaData(dict, name, value);
  return std::vector<double>(1, value);
}

// Internal function to rescale pixel according to slope & intercept
template <typename T>
void
Rescale(T *                         buffer,
        const std::vector<double> & slopes,
        const std::vector<double> & offsets,
        const SizeType              frameSize,
        const SizeType              frameCount)
{
  SizeType i = 0;
  for (SizeType f = 0; f < frameCount; ++f)
  {
    const double slope = slopes[slopes.size() == 1 ? 0 : f];
    const double offset = offsets[offsets.size() == 1 ? 0 : f];
    for (SizeType v = 0; v < frameSize; ++v, ++i)
    {
      const double tmp = static_cast<double>(buffer[i]) * slope + offset;
      buffer[i] = static_cast<T>(tmp);
    }
  }
}

// Internal function to swap slices and volumes
template <typename T>
void
SwapSlicesAndVolumes(T *            buffer,
                     const SizeType sizeX,
                     const SizeType sizeY,
                     const SizeType sizeZ,
                     const SizeType sizeToSwap,
                     const SizeType sizeNoSwap)
{
  const SizeType szSlice = sizeX * sizeY;
  std::vector<T> tempBuffer(szSlice * sizeZ * sizeToSwap * sizeNoSwap);
  T *            toPixel = &(tempBuffer[0]);
  T *            fromNoSwapVol = buffer;
  for (SizeType n = 0; n < sizeNoSwap; ++n)
  {
    T * fromSwapVol = fromNoSwapVol;
    for (SizeType v = 0; v < sizeToSwap; ++v)
    {
      T * fromSlice = fromSwapVol;
      for (SizeType z = 0; z < sizeZ; ++z)
      {
        T * fromPixel = fromSlice;
        for (SizeType p = 0; p < szSlice; ++p)
        {
          *toPixel = *fromPixel;
          ++toPixel;
          ++fromPixel;
        }
        fromSlice += sizeToSwap * szSlice;
      }
      fromSwapVol += szSlice;
    }
    fromNoSwapVol += szSlice * sizeZ * sizeToSwap;
  }

  // Now copy back to buffer
  toPixel = buffer;
  for (auto it = tempBuffer.begin(); it != tempBuffer.end(); ++it, ++toPixel)
  {
    *toPixel = *it;
  }
}

// Internal function to reverse slice order
template <typename T>
void
ReverseSliceOrder(T * buffer, const SizeType sizeX, const SizeType sizeY, const SizeType sz, const SizeType sizeToSwap)
{
  const SizeType ss = sizeX * sizeY;
  T *            fromVol = buffer;
  T              temp;
  for (SizeType v = 0; v < sizeToSwap; ++v)
  {
    T * fromSlice = fromVol;
    T * toSlice = fromVol + (ss * (sz - 1));
    for (SizeType z = 0; z < sz / 2; ++z)
    {
      T * fromPixel = fromSlice;
      T * toPixel = toSlice;
      for (SizeType p = 0; p < ss; ++p)
      {
        temp = *toPixel;
        *toPixel = *fromPixel;
        *fromPixel = temp;
        ++toPixel;
        ++fromPixel;
      }
      fromSlice += ss;
      toSlice -= ss;
    }
    fromVol += ss * sz;
  }
}

// Internal function to copy and cast at the same time
template <typename PixelType>
void
CastCopy(float * to, void * from, size_t pixelCount)
{
  auto * tempFrom = static_cast<PixelType *>(from);
  for (unsigned int i = 0; i < pixelCount; ++i)
  {
    to[i] = static_cast<float>(tempFrom[i]);
  }
}

// Marks every character inside a <...> string, brackets included
std::vector<bool>
MaskStrings(const std::string & s)
{
  std::vector<bool> mask(s.size(), false);
  bool              inString = false;
  for (std::string::size_type i = 0; i < s.size(); ++i)
  {
    if (s[i] == '<')
    {
      inString = true;
    }
    mask[i] = inString;
    if (s[i] == '>')
    {
      inString = false;
    }
  }
  return mask;
}

// Expands ParaVision 360 run-length encoded tokens: @N*(value) -> N copies of value
std::string
ExpandRLE(const std::string & s)
{
  if (s.find("*(") == std::string::npos)
  {
    return s;
  }
  const std::vector<bool> mask = MaskStrings(s);
  std::string             expanded;
  expanded.reserve(s.size());
  std::string::size_type i = 0;
  while (i < s.size())
  {
    if (s[i] == '@' && !mask[i])
    {
      std::string::size_type j = i + 1;
      while (j < s.size() && std::isdigit(static_cast<unsigned char>(s[j])))
      {
        ++j;
      }
      if (j > i + 1 && j + 1 < s.size() && s[j] == '*' && s[j + 1] == '(')
      {
        const std::string::size_type close = s.find(')', j + 2);
        if (close != std::string::npos)
        {
          // The repetition count comes from the file; bound it so a corrupt or
          // hostile record cannot exhaust memory
          constexpr std::string::size_type maxExpandedSize = std::string::size_type{ 1 } << 26;
          if (j - (i + 1) > 9)
          {
            itkGenericExceptionMacro("Bruker JCAMPDX RLE count out of range: " << s.substr(i, close + 1 - i));
          }
          const auto        count = static_cast<std::string::size_type>(std::stoi(s.substr(i + 1, j - (i + 1))));
          const std::string value = s.substr(j + 2, close - (j + 2));
          const std::string::size_type remaining =
            (expanded.size() < maxExpandedSize) ? maxExpandedSize - expanded.size() : 0;
          if (count > remaining / (value.size() + 1))
          {
            itkGenericExceptionMacro("Bruker JCAMPDX RLE expansion exceeds " << maxExpandedSize << " bytes");
          }
          for (std::string::size_type c = 0; c < count; ++c)
          {
            expanded += value;
            expanded += ' ';
          }
          i = close + 1;
          continue;
        }
      }
    }
    expanded += s[i];
    ++i;
  }
  return expanded;
}

std::string
Trim(const std::string & s)
{
  const std::string::size_type begin = s.find_first_not_of(" \t");
  if (begin == std::string::npos)
  {
    return {};
  }
  return s.substr(begin, s.find_last_not_of(" \t") - begin + 1);
}

// Splits a struct array into its parenthesized tuples, ignoring characters inside strings
std::vector<std::string>
SplitTuples(const std::string & s)
{
  const std::vector<bool>  mask = MaskStrings(s);
  std::vector<std::string> tuples;
  int                      depth = 0;
  std::string::size_type   start = 0;
  for (std::string::size_type i = 0; i < s.size(); ++i)
  {
    if (mask[i])
    {
      continue;
    }
    if (s[i] == '(')
    {
      if (depth == 0)
      {
        start = i + 1;
      }
      ++depth;
    }
    else if (s[i] == ')' && depth > 0)
    {
      --depth;
      if (depth == 0)
      {
        tuples.push_back(s.substr(start, i - start));
      }
    }
  }
  return tuples;
}

// Splits struct fields on commas, ignoring commas inside strings
std::vector<std::string>
SplitFields(const std::string & s)
{
  const std::vector<bool>  mask = MaskStrings(s);
  std::vector<std::string> fields;
  std::string::size_type   start = 0;
  for (std::string::size_type i = 0; i <= s.size(); ++i)
  {
    if (i == s.size() || (s[i] == ',' && !mask[i]))
    {
      fields.push_back(Trim(s.substr(start, i - start)));
      start = i + 1;
    }
  }
  return fields;
}

std::vector<double>
ParseDoubles(const std::string & s)
{
  std::istringstream  stream(s);
  std::vector<double> values;
  double              value = 0.0;
  while (stream >> value)
  {
    values.push_back(value);
    if (stream.peek() == ',')
    {
      stream.ignore();
    }
  }
  return values;
}

void
ParseJCAMPDXRecord(const std::string & record, MetaDataDictionary & dict)
{
  const std::string::size_type epos = record.find('=');
  if (epos == std::string::npos)
  {
    itkGenericExceptionMacro("Invalid Bruker JCAMPDX parameter record (Missing =): " << record);
  }
  const std::string parname = record.substr(0, epos);
  std::string       value = record.substr(epos + 1);

  // A leading "( N )" or "( N, M )" written with spaces is a dimension indicator;
  // "(v, v)" without them is a scalar struct value
  bool hasDims = false;
  if (value.compare(0, 2, "( ") == 0)
  {
    const std::string::size_type close = value.find(')');
    if (close != std::string::npos && value.find_first_not_of("0123456789, ", 2) == close)
    {
      hasDims = true;
      value = value.substr(close + 1);
    }
  }
  value = Trim(ExpandRLE(value));

  const std::vector<bool> mask = MaskStrings(value);
  bool                    hasStruct = false;
  for (std::string::size_type i = 0; i < value.size(); ++i)
  {
    if (value[i] == '(' && !mask[i])
    {
      hasStruct = true;
      break;
    }
  }

  if (hasStruct)
  {
    const std::vector<std::string> tuples = SplitTuples(value);
    if (value.find('<') != std::string::npos)
    {
      std::vector<std::vector<std::string>> stringArrayArray;
      for (const std::string & tuple : tuples)
      {
        stringArrayArray.push_back(SplitFields(tuple));
      }
      EncapsulateMetaData(dict, parname, stringArrayArray);
    }
    else
    {
      std::vector<std::vector<double>> doubleArrayArray;
      for (const std::string & tuple : tuples)
      {
        doubleArrayArray.push_back(ParseDoubles(tuple));
      }
      EncapsulateMetaData(dict, parname, doubleArrayArray);
    }
  }
  else if (hasDims && value.find('<') != std::string::npos)
  {
    std::vector<std::string> stringArray;
    std::string::size_type   left = value.find('<');
    while (left != std::string::npos)
    {
      const std::string::size_type right = value.find('>', left + 1);
      if (right == std::string::npos)
      {
        break;
      }
      stringArray.push_back(value.substr(left + 1, right - (left + 1)));
      left = value.find('<', right + 1);
    }
    EncapsulateMetaData(dict, parname, stringArray);
  }
  else if (hasDims)
  {
    const std::vector<double> values = ParseDoubles(value);
    if (values.empty() && !value.empty())
    {
      // Enum arrays hold bare symbolic names, e.g. ( 3 ) spatial spatial spatial
      std::istringstream       stream(value);
      std::vector<std::string> tokens;
      std::string              token;
      while (stream >> token)
      {
        tokens.push_back(token);
      }
      if (tokens.size() == 1)
      {
        EncapsulateMetaData(dict, parname, tokens[0]);
      }
      else
      {
        EncapsulateMetaData(dict, parname, tokens);
      }
    }
    else
    {
      EncapsulateMetaData(dict, parname, values);
    }
  }
  else
  {
    const std::vector<double> values = ParseDoubles(value);
    const bool                allNumeric = value.find_first_not_of("0123456789+-.eE, \t") == std::string::npos;
    if (allNumeric && values.size() == 1)
    {
      EncapsulateMetaData(dict, parname, values[0]);
    }
    else if (allNumeric && values.size() > 1)
    {
      // Fixed-length array with omitted dimension indicator (legacy datasets)
      EncapsulateMetaData(dict, parname, values);
    }
    else
    {
      EncapsulateMetaData(dict, parname, value);
    }
  }
}

// Internal function to read a JCAMPDX parameter file
void
ReadJCAMPDX(const std::string & filename, MetaDataDictionary & dict)
{
  std::ifstream paramsStream(filename.c_str());

  std::string line;
  std::string record;
  while (std::getline(paramsStream, line))
  {
    if (!line.empty() && line.back() == '\r')
    {
      line.pop_back();
    }
    if (line.compare(0, 2, "$$") == 0)
    {
      // Comment lines may appear anywhere, including inside a wrapped value block
      continue;
    }
    if (line.compare(0, 2, "##") == 0)
    {
      if (!record.empty())
      {
        ParseJCAMPDXRecord(record, dict);
        record.clear();
      }
      if (line.compare(0, 5, "##END") == 0)
      {
        // The file may continue after ##END= with a "$$ File finished" trailer
        break;
      }
      if (line.compare(0, 3, "##$") == 0)
      {
        record = line.substr(3);
      }
      // Standard JCAMP labels (##TITLE= etc.) carry no image information
    }
    else if (!record.empty())
    {
      // Values wrap near column 80; wrapped lines keep their trailing space
      if (record.back() != ' ')
      {
        record += ' ';
      }
      record += line;
    }
  }
  if (!record.empty())
  {
    ParseJCAMPDXRecord(record, dict);
  }
}
} // namespace

Bruker2dseqImageIO::Bruker2dseqImageIO()
{
  // By default, only have 3 dimensions
  this->SetNumberOfDimensions(3);
  this->m_PixelType = IOPixelEnum::SCALAR;
  this->m_ComponentType = IOComponentEnum::CHAR;
  this->SetNumberOfComponents(1);

  // Set m_MachineByteOrder to the IOByteOrderEnum of the machine
  // Start out with file byte order == system byte order
  // this will be changed if we're reading a file to whatever
  // the file actually contains.
  if constexpr (ByteSwapper<int>::SystemIsBigEndian())
  {
    this->m_MachineByteOrder = this->m_ByteOrder = IOByteOrderEnum::BigEndian;
  }
  else
  {
    this->m_MachineByteOrder = this->m_ByteOrder = IOByteOrderEnum::LittleEndian;
  }
}

Bruker2dseqImageIO::~Bruker2dseqImageIO() = default;

void
Bruker2dseqImageIO::SwapBytesIfNecessary(void * buff, SizeValueType components)
{
  if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
  {
#define BYTE_SWAP(T) ByteSwapper<T>::SwapRangeFromSystemToLittleEndian((T *)buff, components)
    switch (this->m_OnDiskComponentType)
    {
      case IOComponentEnum::SCHAR:
      case IOComponentEnum::UCHAR:
        // For CHAR and UCHAR, it is not necessary to swap bytes.
        break;
      case IOComponentEnum::SHORT:
        BYTE_SWAP(short);
        break;
      case IOComponentEnum::USHORT:
        BYTE_SWAP(unsigned short);
        break;
      case IOComponentEnum::INT:
        BYTE_SWAP(int);
        break;
      case IOComponentEnum::UINT:
        BYTE_SWAP(unsigned int);
        break;
      case IOComponentEnum::LONG:
        BYTE_SWAP(long);
        break;
      case IOComponentEnum::ULONG:
        BYTE_SWAP(unsigned long);
        break;
      case IOComponentEnum::FLOAT:
        BYTE_SWAP(float);
        break;
      case IOComponentEnum::DOUBLE:
        BYTE_SWAP(double);
        break;
      default:
        itkExceptionStringMacro("Component Type Unknown");
    }
#undef BYTE_SWAP
  }
  else
  {
#define BYTE_SWAP(T) ByteSwapper<T>::SwapRangeFromSystemToBigEndian((T *)buff, components)
    switch (this->m_OnDiskComponentType)
    {
      case IOComponentEnum::SCHAR:
      case IOComponentEnum::UCHAR:
        // For CHAR and UCHAR, it is not necessary to swap bytes.
        break;
      case IOComponentEnum::SHORT:
        BYTE_SWAP(short);
        break;
      case IOComponentEnum::USHORT:
        BYTE_SWAP(unsigned short);
        break;
      case IOComponentEnum::INT:
        BYTE_SWAP(int);
        break;
      case IOComponentEnum::UINT:
        BYTE_SWAP(unsigned int);
        break;
      case IOComponentEnum::LONG:
        BYTE_SWAP(long);
        break;
      case IOComponentEnum::ULONG:
        BYTE_SWAP(unsigned long);
        break;
      case IOComponentEnum::FLOAT:
        BYTE_SWAP(float);
        break;
      case IOComponentEnum::DOUBLE:
        BYTE_SWAP(double);
        break;
      default:
        itkExceptionStringMacro("Component Type Unknown");
    }
#undef BYTE_SWAP
  }
}

void
Bruker2dseqImageIO::Read(void * buffer)
{
  const auto numberOfComponents = this->GetImageSizeInComponents();

  std::string path2Dseq = itksys::SystemTools::CollapseFullPath(this->m_FileName);
  itksys::SystemTools::ConvertToUnixSlashes(path2Dseq);
  std::ifstream stream2Dseq;
  this->OpenFileForReading(stream2Dseq, path2Dseq);

  if (m_ComponentType != m_OnDiskComponentType)
  {
    SizeType numberOfBytesOnDisk = numberOfComponents;
    switch (m_OnDiskComponentType)
    {
      case IOComponentEnum::UCHAR:
        numberOfBytesOnDisk *= sizeof(unsigned char);
        break;
      case IOComponentEnum::SCHAR:
        numberOfBytesOnDisk *= sizeof(char);
        break;
      case IOComponentEnum::USHORT:
        numberOfBytesOnDisk *= sizeof(unsigned short);
        break;
      case IOComponentEnum::SHORT:
        numberOfBytesOnDisk *= sizeof(short);
        break;
      case IOComponentEnum::UINT:
        numberOfBytesOnDisk *= sizeof(unsigned int);
        break;
      case IOComponentEnum::INT:
        numberOfBytesOnDisk *= sizeof(int);
        break;
      case IOComponentEnum::ULONG:
        numberOfBytesOnDisk *= sizeof(unsigned long);
        break;
      case IOComponentEnum::LONG:
        numberOfBytesOnDisk *= sizeof(long);
        break;
      case IOComponentEnum::FLOAT:
        numberOfBytesOnDisk *= sizeof(float);
        break;
      case IOComponentEnum::DOUBLE:
        numberOfBytesOnDisk *= sizeof(double);
        break;
      case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      default:
        itkExceptionMacro("Unknown component type: " << m_ComponentType);
    }

    std::vector<char> dataFromDisk(numberOfBytesOnDisk);
    char *            dataFromDiskBuffer = &(dataFromDisk[0]);
    stream2Dseq.read(dataFromDiskBuffer, numberOfBytesOnDisk);
    if (stream2Dseq.fail())
    {
      itkExceptionMacro("Failed to read file: " << path2Dseq);
    }

    this->SwapBytesIfNecessary(dataFromDiskBuffer, numberOfComponents);

    auto * floatBuffer = static_cast<float *>(buffer);
    switch (m_OnDiskComponentType)
    {
      case IOComponentEnum::SCHAR:
        CastCopy<char>(floatBuffer, dataFromDiskBuffer, numberOfComponents);
        break;
      case IOComponentEnum::UCHAR:
        CastCopy<unsigned char>(floatBuffer, dataFromDiskBuffer, numberOfComponents);
        break;
      case IOComponentEnum::SHORT:
        CastCopy<short>(floatBuffer, dataFromDiskBuffer, numberOfComponents);
        break;
      case IOComponentEnum::USHORT:
        CastCopy<unsigned short>(floatBuffer, dataFromDiskBuffer, numberOfComponents);
        break;
      case IOComponentEnum::INT:
        CastCopy<int>(floatBuffer, dataFromDiskBuffer, numberOfComponents);
        break;
      case IOComponentEnum::UINT:
        CastCopy<unsigned int>(floatBuffer, dataFromDiskBuffer, numberOfComponents);
        break;
      case IOComponentEnum::LONG:
        CastCopy<long>(floatBuffer, dataFromDiskBuffer, numberOfComponents);
        break;
      case IOComponentEnum::ULONG:
        CastCopy<unsigned long>(floatBuffer, dataFromDiskBuffer, numberOfComponents);
        break;
      case IOComponentEnum::FLOAT:
        itkExceptionStringMacro("FLOAT pixels do not need Casting to float");
      case IOComponentEnum::DOUBLE:
        itkExceptionStringMacro("DOUBLE pixels do not need Casting to float");
      case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      default:
        itkExceptionStringMacro("Bad OnDiskComponentType UNKNOWNCOMPONENTTYPE");
    }
  }
  else
  {
    const auto numberOfBytesOnDisk = this->GetImageSizeInBytes();
    auto *     charBuffer = static_cast<char *>(buffer);
    stream2Dseq.read(charBuffer, numberOfBytesOnDisk);
    if (stream2Dseq.fail())
    {
      itkExceptionMacro("Failed to read file: " << path2Dseq);
    }
    this->SwapBytesIfNecessary(charBuffer, numberOfComponents);
  }

  const MetaDataDictionary & dict = this->GetMetaDataDictionary();
  const std::vector<double>  slopes = ReadDoubleArray(dict, "VisuCoreDataSlope", 1.0);
  const std::vector<double>  offsets = ReadDoubleArray(dict, "VisuCoreDataOffs", 0.0);
  const SizeType             frameCount = static_cast<SizeType>(GetParameter<double>(dict, "VisuCoreFrameCount"));
  const SizeType             frameDim = static_cast<SizeType>(GetParameter<double>(dict, "VisuCoreDim"));
  SizeType                   frameSize = this->GetDimensions(0) * this->GetDimensions(1);

  if (slopes.size() != 1 && slopes.size() != frameCount)
  {
    itkExceptionMacro("VisuCoreDataSlope has " << slopes.size() << " values, expected 1 or " << frameCount);
  }
  if (offsets.size() != 1 && offsets.size() != frameCount)
  {
    itkExceptionMacro("VisuCoreDataOffs has " << offsets.size() << " values, expected 1 or " << frameCount);
  }

  if (frameDim == 3)
  {
    frameSize *= this->GetDimensions(2);
  }

  switch (this->m_ComponentType)
  {
    case IOComponentEnum::SCHAR:
      [[fallthrough]];
    case IOComponentEnum::UCHAR:
      [[fallthrough]];
    case IOComponentEnum::SHORT:
      [[fallthrough]];
    case IOComponentEnum::USHORT:
      [[fallthrough]];
    case IOComponentEnum::INT:
      [[fallthrough]];
    case IOComponentEnum::UINT:
      [[fallthrough]];
    case IOComponentEnum::LONG:
      [[fallthrough]];
    case IOComponentEnum::ULONG:
      itkExceptionStringMacro("Must have float pixels to rescale");
    case IOComponentEnum::FLOAT:
      Rescale(static_cast<float *>(buffer), slopes, offsets, frameSize, frameCount);
      break;
    case IOComponentEnum::DOUBLE:
      Rescale(static_cast<double *>(buffer), slopes, offsets, frameSize, frameCount);
      break;
    default:
      itkExceptionMacro("Datatype not supported: " << ImageIOBase::GetComponentTypeAsString(this->m_ComponentType));
  }

  //
  // 2D Multi-echo or calculated maps (e.g. DTI) may be stored echo/image first, then slice
  // Look at the Order Description field to check if they need re-ordering
  //
  if (frameDim == 2 && dict.HasKey("VisuFGOrderDesc"))
  {
    size_t sizeToSwap = 1;
    for (auto & i : GetParameter<std::vector<std::vector<std::string>>>(dict, "VisuFGOrderDesc"))
    {
      // Anything before the SLICE order needs to be re-ordered
      if (i[1] == "<FG_SLICE>")
      {
        break;
      }

      sizeToSwap *= itk::StringToInt32(i[0], "Bruker 2dseq VisuFGOrderDesc size");
    }
    if (sizeToSwap > 1)
    {
      const SizeValueType x = this->GetDimensions(0);
      const SizeValueType y = this->GetDimensions(1);
      const SizeValueType z = this->GetDimensions(2);
      const SizeValueType noswap = this->GetDimensions(3) / sizeToSwap;
      switch (this->m_ComponentType)
      {
        case IOComponentEnum::SCHAR:
          SwapSlicesAndVolumes(static_cast<char *>(buffer), x, y, z, sizeToSwap, noswap);
          break;
        case IOComponentEnum::UCHAR:
          SwapSlicesAndVolumes(static_cast<unsigned char *>(buffer), x, y, z, sizeToSwap, noswap);
          break;
        case IOComponentEnum::SHORT:
          SwapSlicesAndVolumes(static_cast<short *>(buffer), x, y, z, sizeToSwap, noswap);
          break;
        case IOComponentEnum::USHORT:
          SwapSlicesAndVolumes(static_cast<unsigned short *>(buffer), x, y, z, sizeToSwap, noswap);
          break;
        case IOComponentEnum::INT:
          SwapSlicesAndVolumes(static_cast<int *>(buffer), x, y, z, sizeToSwap, noswap);
          break;
        case IOComponentEnum::UINT:
          SwapSlicesAndVolumes(static_cast<unsigned int *>(buffer), x, y, z, sizeToSwap, noswap);
          break;
        case IOComponentEnum::LONG:
          SwapSlicesAndVolumes(static_cast<long *>(buffer), x, y, z, sizeToSwap, noswap);
          break;
        case IOComponentEnum::ULONG:
          SwapSlicesAndVolumes(static_cast<unsigned long *>(buffer), x, y, z, sizeToSwap, noswap);
          break;
        case IOComponentEnum::FLOAT:
          SwapSlicesAndVolumes(static_cast<float *>(buffer), x, y, z, sizeToSwap, noswap);
          break;
        case IOComponentEnum::DOUBLE:
          SwapSlicesAndVolumes(static_cast<double *>(buffer), x, y, z, sizeToSwap, noswap);
          break;
        default:
          itkExceptionMacro("Datatype not supported: " << ImageIOBase::GetComponentTypeAsString(this->m_ComponentType));
      }
    }
  }

  if (dict.HasKey("VisuCoreDiskSliceOrder") &&
      (GetParameter<std::string>(dict, "VisuCoreDiskSliceOrder") == "disk_reverse_slice_order"))
  {
    const SizeValueType x = this->GetDimensions(0);
    const SizeValueType y = this->GetDimensions(1);
    const SizeValueType z = this->GetDimensions(2);
    const SizeValueType v = (this->GetNumberOfDimensions() > 3) ? this->GetDimensions(3) : 1;
    switch (this->m_ComponentType)
    {
      case IOComponentEnum::SCHAR:
        ReverseSliceOrder(static_cast<char *>(buffer), x, y, z, v);
        break;
      case IOComponentEnum::UCHAR:
        ReverseSliceOrder(static_cast<unsigned char *>(buffer), x, y, z, v);
        break;
      case IOComponentEnum::SHORT:
        ReverseSliceOrder(static_cast<short *>(buffer), x, y, z, v);
        break;
      case IOComponentEnum::USHORT:
        ReverseSliceOrder(static_cast<unsigned short *>(buffer), x, y, z, v);
        break;
      case IOComponentEnum::INT:
        ReverseSliceOrder(static_cast<int *>(buffer), x, y, z, v);
        break;
      case IOComponentEnum::UINT:
        ReverseSliceOrder(static_cast<unsigned int *>(buffer), x, y, z, v);
        break;
      case IOComponentEnum::LONG:
        ReverseSliceOrder(static_cast<long *>(buffer), x, y, z, v);
        break;
      case IOComponentEnum::ULONG:
        ReverseSliceOrder(static_cast<unsigned long *>(buffer), x, y, z, v);
        break;
      case IOComponentEnum::FLOAT:
        ReverseSliceOrder(static_cast<float *>(buffer), x, y, z, v);
        break;
      case IOComponentEnum::DOUBLE:
        ReverseSliceOrder(static_cast<double *>(buffer), x, y, z, v);
        break;
      default:
        itkExceptionMacro("Datatype not supported: " << ImageIOBase::GetComponentTypeAsString(this->m_ComponentType));
    }
  }
}

bool
Bruker2dseqImageIO::CanReadFile(const char * FileNameToRead)
{
  std::string file2Dseq = itksys::SystemTools::CollapseFullPath(FileNameToRead);
  itksys::SystemTools::ConvertToUnixSlashes(file2Dseq);
  const std::string fileVisu = itksys::SystemTools::GetFilenamePath(file2Dseq) + "/visu_pars";

  if (!itksys::SystemTools::FileExists(file2Dseq))
  {
    return false;
  }
  if (!itksys::SystemTools::FileExists(fileVisu))
  {
    return false;
  }
  return true;
}

void
Bruker2dseqImageIO::ReadImageInformation()
{
  // Get the meta dictionary for this object.
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  EncapsulateMetaData<std::string>(dict, ITK_InputFilterName, this->GetNameOfClass());

  std::string path2Dseq = itksys::SystemTools::CollapseFullPath(this->m_FileName);
  itksys::SystemTools::ConvertToUnixSlashes(path2Dseq);
  const std::string pathVisu = itksys::SystemTools::GetFilenamePath(path2Dseq) + "/visu_pars";
  ReadJCAMPDX(pathVisu, dict);

  // If the method file exists, read it in case user wants the meta-data
  // However, visu_pars contains everything needed to read so make this optional
  const std::string methodFilename = itksys::SystemTools::GetFilenamePath(path2Dseq) + "/../../method";
  if (itksys::SystemTools::FileExists(methodFilename))
  {
    ReadJCAMPDX(methodFilename, dict);
  }

  const auto wordType = GetParameter<std::string>(dict, "VisuCoreWordType");
  if (wordType == BRUKER_SIGNED_CHAR)
  {
    this->m_ComponentType = IOComponentEnum::CHAR;
    this->m_PixelType = IOPixelEnum::SCALAR;
  }
  else if (wordType == BRUKER_UNSIGNED_CHAR)
  {
    this->m_ComponentType = IOComponentEnum::UCHAR;
    this->m_PixelType = IOPixelEnum::SCALAR;
  }
  else if (wordType == BRUKER_SIGNED_SHORT)
  {
    this->m_ComponentType = IOComponentEnum::SHORT;
    this->m_PixelType = IOPixelEnum::SCALAR;
  }
  else if (wordType == BRUKER_SIGNED_INT)
  {
    this->m_ComponentType = IOComponentEnum::INT;
    this->m_PixelType = IOPixelEnum::SCALAR;
  }
  else if (wordType == BRUKER_FLOAT)
  {
    this->m_ComponentType = IOComponentEnum::FLOAT;
    this->m_PixelType = IOPixelEnum::SCALAR;
  }
  else
  {
    itkExceptionMacro("VisuCoreWordType parameter is invalid: " << wordType);
  }

  // Similar to NIFTI - promote to at least float for rescaling
  this->m_OnDiskComponentType = this->m_ComponentType;
  if (this->m_ComponentType == IOComponentEnum::CHAR || this->m_ComponentType == IOComponentEnum::UCHAR ||
      this->m_ComponentType == IOComponentEnum::SHORT || this->m_ComponentType == IOComponentEnum::USHORT ||
      this->m_ComponentType == IOComponentEnum::INT || this->m_ComponentType == IOComponentEnum::UINT ||
      this->m_ComponentType == IOComponentEnum::LONG || this->m_ComponentType == IOComponentEnum::ULONG)
  {
    this->m_ComponentType = IOComponentEnum::FLOAT;
  }

  const auto byteOrder = GetParameter<std::string>(dict, "VisuCoreByteOrder");
  if (byteOrder == BRUKER_LITTLE_ENDIAN)
  {
    this->m_ByteOrder = IOByteOrderEnum::LittleEndian;
  }
  else if (byteOrder == BRUKER_BIG_ENDIAN)
  {
    this->m_ByteOrder = IOByteOrderEnum::BigEndian;
  }
  else
  {
    itkExceptionMacro("VisuCoreByteOrder parameter is invalid: " << byteOrder);
  }

  const SizeType brukerDim = static_cast<SizeType>(GetParameter<double>(dict, "VisuCoreDim"));
  const SizeType frames = static_cast<SizeType>(GetParameter<double>(dict, "VisuCoreFrameCount"));
  const auto     size = GetParameter<std::vector<double>>(dict, "VisuCoreSize");
  const auto     FoV = GetParameter<std::vector<double>>(dict, "VisuCoreExtent");

  if (brukerDim == 1)
  {
    // Spectroscopy Data. Should probably ignore this, but we've got this far
    // so attempt to convert
    //
    this->SetNumberOfDimensions(1);
    this->SetDimensions(0, size[0]);
    this->SetSpacing(0, FoV[0] / size[0]);
    this->SetOrigin(0, 0);
  }
  else
  {
    const auto position = GetParameter<std::vector<double>>(dict, "VisuCorePosition");
    // Bruker 'origin' is corner of slice/volume. Needs shifting by half-voxel to be ITK origin
    // But for 2D images, the slice position is correct (center of slice)
    vnl_vector<double> halfStep(3);
    halfStep[0] = FoV[0] / (2 * size[0]);
    halfStep[1] = FoV[1] / (2 * size[1]);
    SizeType           sizeZ = 1;
    SizeType           sizeT = 1;
    double             spacingZ = 0;
    vnl_vector<double> sliceDiff(3, 0.0);
    if (brukerDim == 2)
    {
      SizeType sliceLength = 0;
      SizeType framesPerSlice = 1;
      if (dict.HasKey("VisuFGOrderDesc"))
      {
        for (auto & i : GetParameter<std::vector<std::vector<std::string>>>(dict, "VisuFGOrderDesc"))
        {
          const auto length = static_cast<SizeType>(StringToInt32(i[0], "Bruker 2dseq VisuFGOrderDesc size"));
          if (i[1] == "<FG_SLICE>")
          {
            sliceLength = length;
          }
          else
          {
            // Any dimension that isn't a slice is collapsed into the 4th dimension
            sizeT *= length;
            if (sliceLength == 0)
            {
              framesPerSlice *= length;
            }
          }
        }
      }
      // Frame groups without FG_SLICE (e.g. FG_ISA maps) describe one slice;
      // without frame groups fall back to the position count (3 coordinates each)
      const SizeType positionCount = position.size() / 3;
      if (sliceLength > 0)
      {
        sizeZ = sliceLength;
      }
      else
      {
        sizeZ = dict.HasKey("VisuFGOrderDesc") ? 1 : positionCount;
      }
      if (sizeZ > 1 && positionCount > 1)
      {
        // FrameThickness does not include the slice gap and ParaVision sometimes
        // writes SliceDist as 0, so measure the step between slice positions.
        // Positions may be stored per-slice or per-frame (frame groups before
        // FG_SLICE vary faster), so step by the frame count per slice increment.
        const SizeType positionStride = (positionCount > sizeZ) ? framesPerSlice : 1;
        if (3 * (positionStride + 1) <= static_cast<SizeType>(position.size()))
        {
          const vnl_vector<double> slice1(&position[0], 3);
          const vnl_vector<double> slice2(&position[3 * positionStride], 3);
          sliceDiff = slice2 - slice1;
          spacingZ = sliceDiff.magnitude();
        }
      }
      if (spacingZ == 0)
      {
        spacingZ = GetParameter<std::vector<double>>(dict, "VisuCoreFrameThickness")[0];
      }
      halfStep[2] = 0; // Slice position will be correct
    }
    else
    {
      sizeZ = size[2];
      spacingZ = FoV[2] / sizeZ;
      sizeT = frames; // Each volume is a 'frame'
      halfStep[2] = FoV[2] / (2 * size[2]);
    }

    if (sizeT > 1)
    {
      this->SetNumberOfDimensions(4);
      this->SetDimensions(3, sizeT);
      if (dict.HasKey("VisuAcqRepetitionTime"))
      {
        const double TR = GetParameter<std::vector<double>>(dict, "VisuAcqRepetitionTime")[0];
        this->SetSpacing(3, TR / 1e3); // TR is in milliseconds, convert to seconds
      }
      else
      {
        // Map images from Bruker X-Tip don't have a TR
        this->SetSpacing(3, 1);
      }
      this->SetOrigin(3, 0);
    }

    // It is possible for every slice to have a different orientation,
    // but ITK doesn't support this so concatenate all slices as if they
    // had the same orientation
    const auto orient = GetParameter<std::vector<double>>(dict, "VisuCoreOrientation");

    // The Bruker orient field is scanner-to-image. ITK is image-to-scanner.
    // However, ITK stores column-wise, Bruker row-wise. So the below is
    // equivalent to a matrix transpose, which because these are direction
    // matrices with determinant +/-1, is equivalent to an inverse. So this
    // gives the correct orientations.
    const vnl_matrix<double> dirMatrix(&orient[0], 3, 3);
    this->SetDirection(0, dirMatrix.get_row(0));
    this->SetDirection(1, dirMatrix.get_row(1));
    // 2D slices are sometimes stored against the orientation's slice axis; flip to match
    const double reverseZ = (dot_product(sliceDiff, dirMatrix.get_row(2)) < 0) ? -1 : 1;
    this->SetDirection(2, reverseZ * dirMatrix.get_row(2));

    // Now work out the correct ITK origin including the half-voxel offset
    const vnl_vector<double> corner(&position[0], 3);
    vnl_vector<double>       origin = corner + dirMatrix * halfStep;

    this->SetOrigin(0, origin[0]);
    this->SetOrigin(1, origin[1]);
    this->SetOrigin(2, origin[2]);

    // Finally set matrix size and voxel spacing
    this->SetDimensions(0, size[0]);
    this->SetDimensions(1, size[1]);
    this->SetDimensions(2, sizeZ);

    this->SetSpacing(0, FoV[0] / size[0]);
    this->SetSpacing(1, FoV[1] / size[1]);
    this->SetSpacing(2, spacingZ);
  }
}

void
Bruker2dseqImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  print_helper::PrintNumericTrait(os, indent, "OnDiskComponentType", m_OnDiskComponentType);
  print_helper::PrintNumericTrait(os, indent, "MachineByteOrder", m_MachineByteOrder);
}
} // end namespace itk
