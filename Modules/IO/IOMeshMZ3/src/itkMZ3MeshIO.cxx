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

#include "itkMZ3MeshIO.h"

#include "itkMakeUniqueForOverwrite.h"
#include "itksys/SystemTools.hxx"
#include "itkCommonEnums.h"

namespace itk
{

MZ3MeshIO::MZ3MeshIO()
  : m_Internal(std::make_unique<MZ3MeshIOInternals>())
{
  this->m_PointDimension = 3;
  this->m_PointComponentType = IOComponentEnum::FLOAT;
  this->m_CellComponentType = IOComponentEnum::UINT;
  this->m_FileType = IOFileEnum::BINARY;
  this->m_ByteOrder = IOByteOrderEnum::LittleEndian;

  this->AddSupportedReadExtension(".mz3");
  this->AddSupportedWriteExtension(".mz3");
  this->m_UseCompression = true;
  this->m_IsCompressed = true;
}

MZ3MeshIO::~MZ3MeshIO()
{
  if (m_IsCompressed)
  {
    if (m_Internal->m_GzFile != nullptr)
    {
      gzclose(m_Internal->m_GzFile);
      m_Internal->m_GzFile = nullptr;
    }
  }
  else
  {
    m_Ifstream.close();
  }
}

bool
MZ3MeshIO::CanReadFile(const char * fileName)
{
  if (!itksys::SystemTools::FileExists(fileName, true))
  {
    return false;
  }

  if (itksys::SystemTools::GetFilenameLastExtension(fileName) != ".mz3")
  {
    return false;
  }

  std::ifstream file(fileName, std::ios::binary);
  if (!file)
  {
    return false;
  }

  // Read magic number (first 2 bytes)
  uint8_t magic1;
  uint8_t magic2;
  file.read((char *)&magic1, static_cast<std::streamsize>(sizeof(uint8_t)));
  file.read((char *)&magic2, static_cast<std::streamsize>(sizeof(uint8_t)));

  // Check for MZ3 signature
  if (magic1 == 0x4D && magic2 == 0x5A)
  {
    return true;
  }

  // Check for GZip signature
  if (magic1 == 0x1F && magic2 == 0x8B)
  {
    return true;
  }

  return false;
}

bool
MZ3MeshIO::CanWriteFile(const char * fileName)
{
  if (itksys::SystemTools::GetFilenameLastExtension(fileName) != ".mz3")
  {
    return false;
  }

  return true;
}

void
MZ3MeshIO::ReadMeshInformation()
{
  // Check if file is gzip compressed
  std::ifstream file(this->m_FileName.c_str(), std::ios::binary);
  // Read magic number (first 2 bytes)
  uint8_t magic1;
  uint8_t magic2;
  file.read((char *)&magic1, static_cast<std::streamsize>(sizeof(uint8_t)));
  file.read((char *)&magic2, static_cast<std::streamsize>(sizeof(uint8_t)));
  file.close();

  // GZip signature (0x1F8B)
  if (magic1 == 0x1F && magic2 == 0x8B)
  {
    m_IsCompressed = true;
  }
  else
  {
    m_IsCompressed = false;
  }

  if (m_IsCompressed)
  {
    if (m_Internal->m_GzFile != nullptr)
    {
      gzclose(m_Internal->m_GzFile);
      m_Internal->m_GzFile = nullptr;
    }
    m_Internal->m_GzFile = gzopen(m_FileName.c_str(), "rb");
    if (m_Internal->m_GzFile == nullptr)
    {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("File cannot be read");
      throw exception;
    }
  }
  else
  {
    if (m_Ifstream.is_open())
    {
      m_Ifstream.close();
    }
    m_Ifstream.open(m_FileName.c_str(), std::ios::binary);
  }

  // Read 16-byte header
  uint16_t magic, attr;
  uint32_t nface, nvert, nskip;

  if (m_IsCompressed)
  {
    gzread(m_Internal->m_GzFile, (char *)&magic, static_cast<unsigned int>(sizeof(magic)));
    gzread(m_Internal->m_GzFile, (char *)&attr, static_cast<unsigned int>(sizeof(attr)));
    gzread(m_Internal->m_GzFile, (char *)&nface, static_cast<unsigned int>(sizeof(nface)));
    gzread(m_Internal->m_GzFile, (char *)&nvert, static_cast<unsigned int>(sizeof(nvert)));
    gzread(m_Internal->m_GzFile, (char *)&nskip, static_cast<unsigned int>(sizeof(nskip)));
  }
  else
  {
    m_Ifstream.read((char *)&magic, static_cast<std::streamsize>(sizeof(magic)));
    m_Ifstream.read((char *)&attr, static_cast<std::streamsize>(sizeof(attr)));
    m_Ifstream.read((char *)&nface, static_cast<std::streamsize>(sizeof(nface)));
    m_Ifstream.read((char *)&nvert, static_cast<std::streamsize>(sizeof(nvert)));
    m_Ifstream.read((char *)&nskip, static_cast<std::streamsize>(sizeof(nskip)));
  }

  const auto isFace = (attr & 1) != 0;
  const auto isVert = (attr & 2) != 0;
  const auto isRGBA = (attr & 4) != 0;
  const auto isScalar = (attr & 8) != 0;
  const auto isDouble = (attr & 16) != 0;

  // Set mesh information
  if (isVert)
  {
    this->m_NumberOfPoints = nvert;
  }
  else
  {
    this->m_NumberOfPoints = 0;
  }
  this->m_NumberOfCells = nface;
  this->m_PointDimension = 3;
  if (this->m_NumberOfPoints == 0)
  {
    this->m_UpdatePoints = false;
  }
  else
  {
    this->m_UpdatePoints = true;
  }
  if (this->m_NumberOfCells == 0)
  {
    this->m_UpdateCells = false;
  }
  else
  {
    this->m_UpdateCells = true;
  }
  this->m_CellBufferSize = this->m_NumberOfCells * (3 + 2);

  // Set point and cell component types
  this->m_PointComponentType = IOComponentEnum::FLOAT;
  this->m_CellComponentType = IOComponentEnum::UINT;
  this->m_FileType = IOFileEnum::BINARY;
  this->m_ByteOrder = IOByteOrderEnum::LittleEndian;

  if (isScalar)
  {
    this->m_PointPixelType = IOPixelEnum::SCALAR;
    this->m_PointPixelComponentType = IOComponentEnum::FLOAT;
    this->m_NumberOfPointPixelComponents = 1;
    this->m_NumberOfPointPixels = nvert;
    this->m_UpdatePointData = true;
  }
  else if (isDouble)
  {
    this->m_PointPixelType = IOPixelEnum::SCALAR;
    this->m_PointPixelComponentType = IOComponentEnum::DOUBLE;
    this->m_NumberOfPointPixelComponents = 1;
    this->m_NumberOfPointPixels = nvert;
    this->m_UpdatePointData = true;
  }
  else if (isRGBA)
  {
    this->m_PointPixelType = IOPixelEnum::RGBA;
    this->m_PointPixelComponentType = IOComponentEnum::UCHAR;
    this->m_NumberOfPointPixelComponents = 4;
    this->m_NumberOfPointPixels = nvert;
    this->m_UpdatePointData = true;
  }

  this->m_Internal->m_Attributes = attr;
  this->m_Internal->m_Skip = nskip;
}

void
MZ3MeshIO::ReadPoints(void * buffer)
{
  if (m_IsCompressed)
  {
    // Skip header and optional skip bytes
    gzseek(m_Internal->m_GzFile, 16 + m_Internal->m_Skip, SEEK_SET);
    // Skip faces if present
    if (m_Internal->m_Attributes & 1)
    {
      gzseek(m_Internal->m_GzFile, m_NumberOfCells * 12, SEEK_CUR);
    }
    // Read vertex coordinates
    gzread(m_Internal->m_GzFile, buffer, m_NumberOfPoints * 3 * sizeof(float));
  }
  else
  {
    // Skip header and optional skip bytes
    m_Ifstream.seekg(16 + m_Internal->m_Skip);
    // Skip faces if present
    if (m_Internal->m_Attributes & 1)
    {
      m_Ifstream.seekg(m_NumberOfCells * 12, std::ios::cur);
    }
    // Read vertex coordinates
    m_Ifstream.read(static_cast<char *>(buffer), m_NumberOfPoints * 3 * sizeof(float));
  }
}

void
MZ3MeshIO::ReadCells(void * buffer)
{
  const auto cellSize = m_Internal->m_Attributes & 1 ? 12 : 0;
  if (!cellSize)
  {
    return;
  }
  const auto faceBuffer = make_unique_for_overwrite<uint32_t[]>(m_NumberOfCells * 3);
  if (m_IsCompressed)
  {
    // Skip header and optional skip bytes
    gzseek(m_Internal->m_GzFile, 16 + m_Internal->m_Skip, SEEK_SET);
    // Read face indices
    gzread(m_Internal->m_GzFile, faceBuffer.get(), m_NumberOfCells * cellSize);
  }
  else
  {
    // Skip header and optional skip bytes
    m_Ifstream.seekg(16 + m_Internal->m_Skip);
    // Read face indices
    m_Ifstream.read(reinterpret_cast<char *>(faceBuffer.get()), m_NumberOfCells * cellSize);
  }

  SizeValueType index = 0;
  const auto    bufferAsUint = static_cast<uint32_t *>(buffer);
  for (SizeValueType i = 0; i < m_NumberOfCells; ++i)
  {
    bufferAsUint[index++] = static_cast<unsigned int>(CellGeometryEnum::TRIANGLE_CELL);
    bufferAsUint[index++] = 3;
    for (unsigned int j = 0; j < 3; ++j)
    {
      bufferAsUint[index++] = faceBuffer[i * 3 + j];
    }
  }
}

void
MZ3MeshIO::ReadPointData(void * buffer)
{
  const auto isScalar = (m_Internal->m_Attributes & 8) != 0;
  const auto isDouble = (m_Internal->m_Attributes & 16) != 0;
  const auto isRGBA = (m_Internal->m_Attributes & 4) != 0;

  if (!isScalar && !isDouble && !isRGBA)
  {
    return;
  }

  if (m_IsCompressed)
  {
    // Skip header and optional skip bytes
    gzseek(m_Internal->m_GzFile, 16 + m_Internal->m_Skip, SEEK_SET);
    // Skip faces if present
    if (m_Internal->m_Attributes & 1)
    {
      gzseek(m_Internal->m_GzFile, m_NumberOfCells * 12, SEEK_CUR);
    }
    // Skip vertices if present
    if (m_Internal->m_Attributes & 2)
    {
      gzseek(m_Internal->m_GzFile, m_NumberOfPoints * 12, SEEK_CUR);
    }
    // Read point data
    if (isRGBA)
    {
      gzread(m_Internal->m_GzFile, buffer, m_NumberOfPointPixels * 4);
    }
    else if (isScalar)
    {
      gzread(m_Internal->m_GzFile, buffer, m_NumberOfPointPixels * 4);
    }
    else if (isDouble)
    {
      gzread(m_Internal->m_GzFile, buffer, m_NumberOfPointPixels * 8);
    }
  }
  else
  {
    // Skip header and optional skip bytes
    m_Ifstream.seekg(16 + m_Internal->m_Skip);
    // Skip faces if present
    if (m_Internal->m_Attributes & 1)
    {
      m_Ifstream.seekg(m_NumberOfCells * 12, std::ios::cur);
    }
    // Skip vertices if present
    if (m_Internal->m_Attributes & 2)
    {
      m_Ifstream.seekg(m_NumberOfPoints * 12, std::ios::cur);
    }
    // Read point data
    if (isRGBA)
    {
      m_Ifstream.read(static_cast<char *>(buffer), m_NumberOfPointPixels * 4);
    }
    else if (isScalar)
    {
      m_Ifstream.read(static_cast<char *>(buffer), m_NumberOfPointPixels * 4);
    }
    else if (isDouble)
    {
      m_Ifstream.read(static_cast<char *>(buffer), m_NumberOfPointPixels * 8);
    }
  }
}

void
MZ3MeshIO::ReadCellData(void * itkNotUsed(buffer))
{
  // No cell data
}

void
MZ3MeshIO::WriteMeshInformation()
{
  if (this->m_UseCompression)
  {
    m_IsCompressed = true;
  }
  else
  {
    m_IsCompressed = false;
  }

  if (m_IsCompressed)
  {
    m_Internal->m_GzFile = gzopen(m_FileName.c_str(), "wb");
    if (m_Internal->m_GzFile == nullptr)
    {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("File cannot be written");
      throw exception;
    }
  }
  else
  {
    m_Ofstream.open(m_FileName.c_str(), std::ios::binary);
  }

  // Write header
  uint8_t  magic1 = 0x4D;
  uint8_t  magic2 = 0x5A;
  uint16_t attr = 0;
  if (this->m_NumberOfCells > 0)
  {
    attr |= 1;
  }
  if (this->m_NumberOfPoints > 0)
  {
    attr |= 2;
  }
  uint32_t nskip = 0;

  if (this->m_PointPixelType == IOPixelEnum::SCALAR && this->m_PointPixelComponentType == IOComponentEnum::FLOAT)
  {
    attr |= 8;
  }
  else if (this->m_PointPixelType == IOPixelEnum::SCALAR && this->m_PointPixelComponentType == IOComponentEnum::DOUBLE)
  {
    attr |= 16;
  }
  else if (this->m_PointPixelType == IOPixelEnum::RGBA)
  {
    attr |= 4;
  }
  else if (this->m_PointPixelType == IOPixelEnum::SCALAR)
  {
    switch (this->m_PointPixelComponentType)
    {
      case IOComponentEnum::UCHAR:
        // Write as float
        attr |= 8;
        break;
      case IOComponentEnum::CHAR:
        attr |= 8;
        break;
      case IOComponentEnum::USHORT:
        attr |= 8;
        break;
      case IOComponentEnum::SHORT:
        attr |= 8;
        break;
      default:
        if (this->m_PointPixelComponentType != IOComponentEnum::UNKNOWNCOMPONENTTYPE)
        {
          itkExceptionMacro("Unsupported point pixel component type");
        }
    }
  }
  else if (this->m_PointPixelComponentType != IOComponentEnum::UNKNOWNCOMPONENTTYPE)
  {
    itkExceptionMacro("Unsupported point pixel type");
  }

  uint32_t nface = this->m_NumberOfCells;
  uint32_t nvert = this->m_NumberOfPoints;
  if (this->m_NumberOfPoints == 0)
  {
    nvert = this->m_NumberOfPointPixels;
  }
  m_Internal->m_Attributes = attr;
  m_Internal->m_Skip = nskip;
  if (m_IsCompressed)
  {
    m_Internal->m_VertexBuffer.resize(nvert * 3);
  }

  if (m_IsCompressed)
  {
    gzwrite(m_Internal->m_GzFile, (char *)&magic1, sizeof(magic1));
    gzwrite(m_Internal->m_GzFile, (char *)&magic2, sizeof(magic2));
    gzwrite(m_Internal->m_GzFile, (char *)&attr, sizeof(attr));
    gzwrite(m_Internal->m_GzFile, (char *)&nface, sizeof(nface));
    gzwrite(m_Internal->m_GzFile, (char *)&nvert, sizeof(nvert));
    gzwrite(m_Internal->m_GzFile, (char *)&nskip, sizeof(nskip));
  }
  else
  {
    m_Ofstream.write(reinterpret_cast<char *>(&magic1), sizeof(magic1));
    m_Ofstream.write(reinterpret_cast<char *>(&magic2), sizeof(magic2));
    m_Ofstream.write(reinterpret_cast<char *>(&attr), sizeof(attr));
    m_Ofstream.write(reinterpret_cast<char *>(&nface), sizeof(nface));
    m_Ofstream.write(reinterpret_cast<char *>(&nvert), sizeof(nvert));
    m_Ofstream.write(reinterpret_cast<char *>(&nskip), sizeof(nskip));
  }
}

void
MZ3MeshIO::WritePoints(void * buffer)
{
  switch (this->m_PointComponentType)
  {
    case IOComponentEnum::FLOAT:
    {
      if (m_IsCompressed)
      {
        // Copy for deferred writing
        std::memcpy(m_Internal->m_VertexBuffer.data(), buffer, m_NumberOfPoints * 3 * sizeof(float));
      }
      else
      {
        // Skip header and optional skip bytes
        m_Ofstream.seekp(16 + m_Internal->m_Skip);
        // Skip faces if present
        if (m_Internal->m_Attributes & 1)
        {
          m_Ofstream.seekp(m_NumberOfCells * 12, std::ios::cur);
        }
        // Write vertex coordinates
        m_Ofstream.write(static_cast<char *>(buffer), m_NumberOfPoints * 3 * sizeof(float));
      }
      break;
    }
    case IOComponentEnum::DOUBLE:
    {
      WritePoints(static_cast<double *>(buffer));
      break;
    }
    case IOComponentEnum::LDOUBLE:
    {
      WritePoints(static_cast<long double *>(buffer));
      break;
    }
    default:
    {
      itkExceptionMacro("Unsupported point component type");
    }
  }
}

void
MZ3MeshIO::WriteCells(void * buffer)
{
  switch (this->m_CellComponentType)
  {
    case IOComponentEnum::UCHAR:
    {
      WriteCells(static_cast<unsigned char *>(buffer));
      break;
    }
    case IOComponentEnum::CHAR:
    {
      WriteCells(static_cast<unsigned char *>(buffer));
      break;
    }
    case IOComponentEnum::USHORT:
    {
      WriteCells(static_cast<unsigned short *>(buffer));
      break;
    }
    case IOComponentEnum::SHORT:
    {
      WriteCells(static_cast<short *>(buffer));
      break;
    }
    case IOComponentEnum::UINT:
    {
      WriteCells(static_cast<unsigned int *>(buffer));
      break;
    }
    case IOComponentEnum::INT:
    {
      WriteCells(static_cast<int *>(buffer));
      break;
    }
    case IOComponentEnum::ULONG:
    {
      WriteCells(static_cast<unsigned long *>(buffer));
      break;
    }
    case IOComponentEnum::LONG:
    {
      WriteCells(static_cast<long *>(buffer));
      break;
    }
    case IOComponentEnum::ULONGLONG:
    {
      WriteCells(static_cast<unsigned long long *>(buffer));
      break;
    }
    case IOComponentEnum::LONGLONG:
    {
      WriteCells(static_cast<long long *>(buffer));
      break;
    }
    default:
    {
      itkExceptionMacro("Unsupported cell component type" << std::endl);
    }
  }
}

void
MZ3MeshIO::WritePointData(void * buffer)
{
  if (this->m_PointPixelComponentType == IOComponentEnum::UNKNOWNCOMPONENTTYPE)
  {
    std::cerr << "Unknown point pixel component type****" << std::endl;
    return;
  }
  if (m_IsCompressed)
  {
    if (this->m_PointPixelType == IOPixelEnum::RGBA && this->m_PointPixelComponentType == IOComponentEnum::UCHAR)
    {
      gzwrite(m_Internal->m_GzFile, buffer, m_NumberOfPointPixels * 4);
    }
    else if (this->m_PointPixelType == IOPixelEnum::SCALAR &&
             this->m_PointPixelComponentType == IOComponentEnum::DOUBLE)
    {
      gzwrite(m_Internal->m_GzFile, buffer, m_NumberOfPointPixels * 8);
    }
    else if (this->m_PointPixelType == IOPixelEnum::SCALAR && this->m_PointPixelComponentType == IOComponentEnum::FLOAT)
    {
      gzwrite(m_Internal->m_GzFile, buffer, m_NumberOfPointPixels * 4);
    }
    else
    {
      if (this->m_PointPixelType == IOPixelEnum::SCALAR)
      {
        switch (this->m_PointPixelComponentType)
        {
          case IOComponentEnum::UCHAR:
            WritePointData(static_cast<unsigned char *>(buffer));
            break;
          case IOComponentEnum::CHAR:
            WritePointData(static_cast<char *>(buffer));
            break;
          case IOComponentEnum::USHORT:
            WritePointData(static_cast<unsigned short *>(buffer));
            break;
          case IOComponentEnum::SHORT:
            WritePointData(static_cast<short *>(buffer));
            break;
          default:
            itkExceptionMacro("Unsupported point pixel component type");
        }
      }
      else
      {
        itkExceptionMacro("Unsupported point pixel type");
      }
    }
  }
  else
  {
    // Skip header and optional skip bytes
    m_Ofstream.seekp(16 + m_Internal->m_Skip);
    // Skip faces if present
    if (m_Internal->m_Attributes & 1)
    {
      m_Ofstream.seekp(m_NumberOfCells * 12, std::ios::cur);
    }
    // Skip vertices if present
    if (m_Internal->m_Attributes & 2)
    {
      m_Ofstream.seekp(m_NumberOfPoints * 12, std::ios::cur);
    }
    if (this->m_PointPixelType == IOPixelEnum::RGBA && this->m_PointPixelComponentType == IOComponentEnum::UCHAR)
    {
      m_Ofstream.write(static_cast<char *>(buffer), m_NumberOfPointPixels * 4);
    }
    else if (this->m_PointPixelType == IOPixelEnum::SCALAR &&
             this->m_PointPixelComponentType == IOComponentEnum::DOUBLE)
    {
      m_Ofstream.write(static_cast<char *>(buffer), m_NumberOfPointPixels * 8);
    }
    else if (this->m_PointPixelType == IOPixelEnum::SCALAR && this->m_PointPixelComponentType == IOComponentEnum::FLOAT)
    {
      m_Ofstream.write(static_cast<char *>(buffer), m_NumberOfPointPixels * 4);
    }
    else
    {
      if (this->m_PointPixelType == IOPixelEnum::SCALAR)
      {
        switch (this->m_PointPixelComponentType)
        {
          case IOComponentEnum::UCHAR:
            WritePointData(static_cast<unsigned char *>(buffer));
            break;
          case IOComponentEnum::CHAR:
            WritePointData(static_cast<char *>(buffer));
            break;
          case IOComponentEnum::USHORT:
            WritePointData(static_cast<unsigned short *>(buffer));
            break;
          case IOComponentEnum::SHORT:
            WritePointData(static_cast<short *>(buffer));
            break;
          default:
            itkExceptionMacro("Unsupported point pixel component type");
        }
      }
      else
      {
        itkExceptionMacro("Unsupported point pixel type");
      }
    }
  }
}

void
MZ3MeshIO::WriteCellData(void * itkNotUsed(buffer))
{
  // No cell data
}

void
MZ3MeshIO::Write()
{
  if (m_IsCompressed)
  {
    if (m_Internal->m_GzFile != nullptr)
    {
      gzclose(m_Internal->m_GzFile);
      m_Internal->m_GzFile = nullptr;
    }
  }
  else
  {
    m_Ofstream.close();
  }
}

void
MZ3MeshIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // namespace itk
