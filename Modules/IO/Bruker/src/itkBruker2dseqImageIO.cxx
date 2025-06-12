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
    for (SizeType v = 0; v < frameSize; ++v, ++i)
    {
      const double tmp = static_cast<double>(buffer[i]) * slopes[f] + offsets[f];
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

// Internal function to read a JCAMPDX parameter file
void
ReadJCAMPDX(const std::string & filename, MetaDataDictionary & dict)
{
  std::ifstream paramsStream(filename.c_str());

  std::string line;
  // First five lines are 'comments' starting with ##
  std::getline(paramsStream, line);
  std::getline(paramsStream, line);
  std::getline(paramsStream, line);
  std::getline(paramsStream, line);
  std::getline(paramsStream, line);

  // Then three lines starting with $$
  std::getline(paramsStream, line);
  std::getline(paramsStream, line);
  std::getline(paramsStream, line);

  // Now process parameters starting with ##$
  while (std::getline(paramsStream, line))
  {
    // Check start of line
    if (line.substr(0, 2) == "$$")
    {
      // Comment line
      continue;
    }
    if (line.substr(0, 5) == "##END")
    {
      // There should be one comment line after this line in the file
      continue;
    }
    else if (line.substr(0, 3) != "##$")
    {
      itkGenericExceptionMacro("Failed to parse Bruker JCAMPDX: " + line);
    }

    const std::string::size_type epos = line.find('=', 3);
    if (epos == std::string::npos)
    {
      itkGenericExceptionMacro("Invalid Bruker JCAMPDX parameter line (Missing =): " << line);
    }

    const std::string parname = line.substr(3, epos - 3);
    std::string       par = line.substr(epos + 1);
    if (par[0] == '(')
    {
      // Array value
      // The array sizes appear to be entirely meaningless. 65 means a string, except when it doesn't and actually gives
      // the length of the string Skip to next line, process lines until we hit a comment or new parameter Then process
      // all lines together and look for strings or numbers
      par.clear();
      std::string lines;
      while (paramsStream.peek() != '#' && paramsStream.peek() != '$')
      {
        std::getline(paramsStream, line);
        lines.append(line);
      }

      std::string::size_type leftBracket = lines.find('(');
      if (leftBracket == std::string::npos)
      {
        // Now check for array of strings marked with <>
        std::string::size_type left = lines.find('<');
        if (left != std::string::npos)
        {
          std::vector<std::string> stringArray;
          while (left != std::string::npos)
          {
            const std::string::size_type right = lines.find('>', left + 1);
            stringArray.push_back(lines.substr(left + 1, right - (left + 1)));
            left = lines.find('<', right + 1);
          }
          EncapsulateMetaData(dict, parname, stringArray);
        }
        else
        {
          // An array of numbers
          std::stringstream   lineStream(lines);
          double              doubleValue;
          std::vector<double> doubleArray;
          while (lineStream >> doubleValue)
          {
            doubleArray.push_back(doubleValue);
            if (lineStream.peek() == ',')
            {
              // Ignore commas
              lineStream.ignore();
            }
          }
          EncapsulateMetaData(dict, parname, doubleArray);
        }
      }
      else
      {
        // An array of arrays
        std::string::size_type rightBracket = lines.find(')', leftBracket);

        if (lines.find('<') != std::string::npos)
        {
          // Array of array of strings (and maybe doubles, but let's keep it sane)
          std::vector<std::vector<std::string>> stringArrayArray;
          while (leftBracket != std::string::npos)
          {
            std::string::size_type   stringStart = leftBracket + 1;
            std::string::size_type   stringEnd = lines.find(',', stringStart);
            std::vector<std::string> stringArray;
            while (stringStart < rightBracket)
            {
              stringArray.push_back(lines.substr(stringStart, stringEnd - stringStart));
              stringStart = stringEnd + 2; // Eat comma + space character
              stringEnd = lines.find(',', stringStart + 1);
              if (stringEnd > rightBracket)
              {
                stringEnd = rightBracket;
              }
            }
            stringArrayArray.push_back(stringArray);
            leftBracket = lines.find('(', rightBracket);
            rightBracket = lines.find(')', leftBracket);
          }
          EncapsulateMetaData(dict, parname, stringArrayArray);
        }
        else
        {
          // Array of array of numbers
          std::vector<std::vector<double>> doubleArrayArray;
          while (leftBracket != std::string::npos)
          {
            std::istringstream  arrayStream(lines.substr(leftBracket, rightBracket - leftBracket));
            std::vector<double> doubleArray;
            double              doubleValue;
            while (arrayStream >> doubleValue)
            {
              doubleArray.push_back(doubleValue);
              if (arrayStream && (arrayStream.peek() == ','))
              {
                // Ignore commas. Some arrays have them, others don't
                arrayStream.ignore();
              }
            }
            doubleArrayArray.push_back(doubleArray);
            leftBracket = lines.find('(', rightBracket);
            rightBracket = lines.find(')', leftBracket);
          }
          EncapsulateMetaData(dict, parname, doubleArrayArray);
        }
      }
    }
    else
    {
      // A single value
      std::istringstream streamPar(par);
      double             value;
      streamPar >> value;
      if (streamPar.fail())
      {
        // Didn't read a valid number, so it's a string
        EncapsulateMetaData(dict, parname, par);
      }
      else
      {
        EncapsulateMetaData(dict, parname, value);
      }
    }
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
      case IOComponentEnum::CHAR:
        BYTE_SWAP(char);
        break;
      case IOComponentEnum::UCHAR:
        BYTE_SWAP(unsigned char);
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
        itkExceptionMacro("Component Type Unknown");
    }
#undef BYTE_SWAP
  }
  else
  {
#define BYTE_SWAP(T) ByteSwapper<T>::SwapRangeFromSystemToBigEndian((T *)buff, components)
    switch (this->m_OnDiskComponentType)
    {
      case IOComponentEnum::CHAR:
        BYTE_SWAP(char);
        break;
      case IOComponentEnum::UCHAR:
        BYTE_SWAP(unsigned char);
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
        itkExceptionMacro("Component Type Unknown");
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
      case IOComponentEnum::CHAR:
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
      case IOComponentEnum::CHAR:
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
        itkExceptionMacro("FLOAT pixels do not need Casting to float");
      case IOComponentEnum::DOUBLE:
        itkExceptionMacro("DOUBLE pixels do not need Casting to float");
      case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      default:
        itkExceptionMacro("Bad OnDiskComponentType UNKNOWNCOMPONENTTYPE");
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
  const auto                 slopes = GetParameter<std::vector<double>>(dict, "VisuCoreDataSlope");
  const auto                 offsets = GetParameter<std::vector<double>>(dict, "VisuCoreDataOffs");
  const SizeType             frameCount = static_cast<SizeType>(GetParameter<double>(dict, "VisuCoreFrameCount"));
  const SizeType             frameDim = static_cast<SizeType>(GetParameter<double>(dict, "VisuCoreDim"));
  SizeType                   frameSize = this->GetDimensions(0) * this->GetDimensions(1);

  if (frameDim == 3)
  {
    frameSize *= this->GetDimensions(2);
  }

  switch (this->m_ComponentType)
  {
    case IOComponentEnum::CHAR:
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
      itkExceptionMacro("Must have float pixels to rescale");
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

      sizeToSwap *= std::stoi(i[0].c_str());
    }
    if (sizeToSwap > 1)
    {
      const SizeValueType x = this->GetDimensions(0);
      const SizeValueType y = this->GetDimensions(1);
      const SizeValueType z = this->GetDimensions(2);
      const SizeValueType noswap = this->GetDimensions(3) / sizeToSwap;
      switch (this->m_ComponentType)
      {
        case IOComponentEnum::CHAR:
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
      case IOComponentEnum::CHAR:
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
    SizeType sizeZ = 1;
    SizeType sizeT = 1;
    double   spacingZ = 1;
    double   reverseZ = 1;
    if (brukerDim == 2)
    {
      // The obvious way to get number of slices is sum of SlicePacksSlices - but single-slice images do not store this!
      // The easiest way is divide the length of Position by 3 (3 co-ordinates per slice position)
      sizeZ = position.size() / 3;
      if (sizeZ == 1)
      { // Special case for single-slice, because that doesn't store SliceDist
        spacingZ = GetParameter<std::vector<double>>(dict, "VisuCoreFrameThickness")[0];
      }
      else
      {
        // FrameThickness does not include slice gap
        // You would think that we could use the SliceDist field for multi-slice, but ParaVision
        // has a bug that sometimes sets SliceDist to 0
        // So - calculate this manually from the SlicePosition field
        const vnl_vector<double> slice1(&position[0], 3);
        const vnl_vector<double> slice2(&position[3], 3);
        const vnl_vector<double> diff = slice2 - slice1;
        spacingZ = diff.magnitude();
      }
      if (dict.HasKey("VisuFGOrderDesc"))
      {
        // Find the FG_CYCLE field
        sizeT = 1;
        for (auto & i : GetParameter<std::vector<std::vector<std::string>>>(dict, "VisuFGOrderDesc"))
        {
          // Anything dimension that isn't a slice needs to be collapsed into the 4th dimension
          if (i[1] != "<FG_SLICE>")
          {
            sizeT *= std::stoi(i[0].c_str());
          }
        }
      }
      else
      {
        itkGenericExceptionMacro("Could not find order description field");
      }
      halfStep[2] = 0; // Slice position will be correct

      if (sizeZ > 1)
      { // There appears to be a bug in 2dseq orientations for Coronal 2D slices.
        // This code checks if we have coronal slices and reverses the Z-direction
        // further down, which makes the images appear correct in FSL View.
        // The acquisition orientation is not stored in visu_pars so work out if
        // this is coronal by checking if the Y component of the slice positions is
        // changing.
        const vnl_vector<double> corner1(&position[0], 3);
        const vnl_vector<double> corner2(&position[3], 3);
        vnl_vector<double>       diff = corner2 - corner1;
        if (diff[1] != 0)
        {
          reverseZ = -1;
        }
      }
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
    // See note above for apparent bug in 2D coronal acquisitions
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

  os << indent << "OnDiskComponentType" << static_cast<NumericTraits<IOComponentEnum>::PrintType>(m_OnDiskComponentType)
     << std::endl;
  os << indent << "MachineByteOrder" << static_cast<NumericTraits<IOByteOrderEnum>::PrintType>(m_MachineByteOrder)
     << std::endl;
}
} // end namespace itk
