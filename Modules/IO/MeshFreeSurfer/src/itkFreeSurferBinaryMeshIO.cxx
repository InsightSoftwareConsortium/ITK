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

#include "itkFreeSurferBinaryMeshIO.h"

#include <itksys/SystemTools.hxx>

namespace itk
{
FreeSurferBinaryMeshIO ::FreeSurferBinaryMeshIO()

{
  this->AddSupportedWriteExtension(".fsb");
  this->AddSupportedWriteExtension(".fcv");
}

FreeSurferBinaryMeshIO ::~FreeSurferBinaryMeshIO() = default;

bool
FreeSurferBinaryMeshIO ::CanReadFile(const char * fileName)
{
  if (!itksys::SystemTools::FileExists(fileName, true))
  {
    return false;
  }

  if (itksys::SystemTools::GetFilenameLastExtension(fileName) != ".fsb" &&
      itksys::SystemTools::GetFilenameLastExtension(fileName) != ".fcv")
  {
    return false;
  }

  return true;
}

bool
FreeSurferBinaryMeshIO ::CanWriteFile(const char * fileName)
{
  if (itksys::SystemTools::GetFilenameLastExtension(fileName) != ".fsb" &&
      itksys::SystemTools::GetFilenameLastExtension(fileName) != ".fcv")
  {
    return false;
  }

  return true;
}

void
FreeSurferBinaryMeshIO ::OpenFile()
{
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No input FileName");
  }

  if (!itksys::SystemTools::FileExists(m_FileName.c_str()))
  {
    itkExceptionMacro("File " << this->m_FileName << " does not exist");
  }

  m_InputFile.open(this->m_FileName.c_str(), std::ios::binary);

  if (!this->m_InputFile.is_open())
  {
    itkExceptionMacro("Unable to open file inputFile " << this->m_FileName);
  }
}

void
FreeSurferBinaryMeshIO ::CloseFile()
{
  if (m_InputFile.is_open())
  {
    m_InputFile.close();
  }
}

void
FreeSurferBinaryMeshIO ::ReadMeshInformation()
{
  // Define input file stream and attach it to input file
  OpenFile();

  // Define required variables
  constexpr unsigned int fileTypeIdLength = 3;
  unsigned char          fileTypeId[fileTypeIdLength];
  this->m_FileType = IOFileEnum::BINARY;

  // Read file type
  m_InputFile.read(reinterpret_cast<char *>(fileTypeId), fileTypeIdLength);
  m_FileTypeIdentifier = 0;
  m_FileTypeIdentifier <<= 8;

  m_FileTypeIdentifier |= fileTypeId[0];
  m_FileTypeIdentifier <<= 8;

  m_FileTypeIdentifier |= fileTypeId[1];
  m_FileTypeIdentifier <<= 8;

  m_FileTypeIdentifier |= fileTypeId[2];

  // If input file is freesurfer binary surface file
  if (m_FileTypeIdentifier == (-2 & 0x00ffffff))
  {
    constexpr unsigned int numberOfCellPoints = 3;
    // Read input commend
    int byte;

    //  Extract Comment, and ignore it.
    byte = m_InputFile.get();

    std::string comment = "";

    while (byte != '\n')
    {
      comment += byte;
      byte = m_InputFile.get();
      if (byte == EOF)
      {
        itkExceptionMacro(<< "Unexpected EOF");
      }
    }
    // Try to get the second '\n', but if the '\n' is not there, we put the byte
    // back.
    byte = m_InputFile.get();
    if (byte != '\n')
    {
      if (byte == EOF)
      {
        itkExceptionMacro(<< "Unexpected EOF");
      }
      m_InputFile.unget();
    }

    // Read the number of points and number of cells
    itk::uint32_t numberOfPoints;
    m_InputFile.read((char *)(&numberOfPoints), sizeof(numberOfPoints));
    itk::ByteSwapper<itk::uint32_t>::SwapFromSystemToBigEndian(&numberOfPoints);
    this->m_NumberOfPoints = static_cast<SizeValueType>(numberOfPoints);

    itk::uint32_t numberOfCells;
    m_InputFile.read((char *)(&numberOfCells), sizeof(numberOfCells));
    itk::ByteSwapper<itk::uint32_t>::SwapFromSystemToBigEndian(&numberOfCells);
    this->m_NumberOfCells = static_cast<SizeValueType>(numberOfCells);

    this->m_PointDimension = 3;

    // If number of points is not equal zero, update points
    if (this->m_NumberOfPoints)
    {
      this->m_UpdatePoints = true;
    }

    // If number of cells is not equal zero, update points
    if (this->m_NumberOfCells)
    {
      this->m_UpdateCells = true;
    }

    // Set default point component type
    this->m_PointComponentType = IOComponentEnum::FLOAT;

    // Set default cell component type
    this->m_CellComponentType = IOComponentEnum::UINT;
    this->m_CellBufferSize = this->m_NumberOfCells * (numberOfCellPoints + 2);

    m_FilePosition = m_InputFile.tellg();
  }
  // If input file is curvature file
  else if (m_FileTypeIdentifier == (-1 & 0x00ffffff))
  {
    // Set corresponding flags
    this->m_UpdatePoints = false;
    this->m_UpdateCells = false;
    this->m_UpdatePointData = true;
    this->m_UpdateCellData = false;

    // Read numberOfValuesPerPoint and numberOfPoints and numberOfCells
    itk::uint32_t numberOfPoints;
    m_InputFile.read((char *)(&numberOfPoints), sizeof(numberOfPoints));
    itk::ByteSwapper<itk::uint32_t>::SwapFromSystemToBigEndian(&numberOfPoints);
    this->m_NumberOfPoints = static_cast<SizeValueType>(numberOfPoints);
    this->m_NumberOfPointPixels = this->m_NumberOfPoints;

    itk::uint32_t numberOfCells;
    m_InputFile.read((char *)(&numberOfCells), sizeof(numberOfCells));
    itk::ByteSwapper<itk::uint32_t>::SwapFromSystemToBigEndian(&numberOfCells);
    this->m_NumberOfCells = static_cast<SizeValueType>(numberOfCells);

    itk::uint32_t numberOfValuesPerPoint;
    m_InputFile.read((char *)(&numberOfValuesPerPoint), sizeof(numberOfValuesPerPoint));
    itk::ByteSwapper<itk::uint32_t>::SwapFromSystemToBigEndian(&numberOfValuesPerPoint);

    m_FilePosition = m_InputFile.tellg();
  }
  else
  {
    itkExceptionMacro(<< "Unvalid file type " << m_FileTypeIdentifier);
  }

  // Set default point pixel component and point pixel type
  this->m_PointPixelComponentType = IOComponentEnum::FLOAT;
  this->m_NumberOfPointPixelComponents = itk::NumericTraits<unsigned int>::OneValue();
  this->m_PointPixelType = IOPixelEnum::SCALAR;

  // Set default cell pixel component and point pixel type
  this->m_CellPixelComponentType = IOComponentEnum::FLOAT;
  this->m_NumberOfCellPixelComponents = itk::NumericTraits<unsigned int>::OneValue();
  this->m_CellPixelType = IOPixelEnum::SCALAR;

  CloseFile();
}

void
FreeSurferBinaryMeshIO ::ReadPoints(void * buffer)
{
  OpenFile();
  m_InputFile.seekg(m_FilePosition, std::ios::beg);

  // Number of data array
  auto * data = static_cast<float *>(buffer);

  m_InputFile.read(static_cast<char *>(buffer), this->m_NumberOfPoints * this->m_PointDimension * sizeof(float));
  itk::ByteSwapper<float>::SwapRangeFromSystemToBigEndian(data, this->m_NumberOfPoints * this->m_PointDimension);
}

void
FreeSurferBinaryMeshIO ::ReadCells(void * buffer)
{
  constexpr unsigned int numberOfCellPoints = 3;
  auto *                 data = new itk::uint32_t[this->m_NumberOfCells * numberOfCellPoints];

  m_InputFile.read(reinterpret_cast<char *>(data), this->m_NumberOfCells * numberOfCellPoints * sizeof(itk::uint32_t));
  itk::ByteSwapper<itk::uint32_t>::SwapRangeFromSystemToBigEndian(data, this->m_NumberOfCells * numberOfCellPoints);

  this->WriteCellsBuffer(
    data, static_cast<unsigned int *>(buffer), CellGeometryEnum::TRIANGLE_CELL, 3, this->m_NumberOfCells);
  delete[] data;

  CloseFile();
}

void
FreeSurferBinaryMeshIO ::ReadPointData(void * buffer)
{
  OpenFile();
  m_InputFile.seekg(m_FilePosition, std::ios::beg);

  auto * data = static_cast<float *>(buffer);

  m_InputFile.read(reinterpret_cast<char *>(data), this->m_NumberOfPointPixels * sizeof(float));
  itk::ByteSwapper<float>::SwapRangeFromSystemToBigEndian(data, this->m_NumberOfPointPixels);

  CloseFile();
}

void
FreeSurferBinaryMeshIO ::ReadCellData(void * itkNotUsed(buffer))
{}

void
FreeSurferBinaryMeshIO ::WriteMeshInformation()
{
  // Check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str(), std::ios::out | std::ios::binary);

  // Whether output stream opened successfully
  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }

  if (this->m_UpdatePoints && this->m_UpdateCells)
  {
    // MAGIC_NUMBER = 16777214 ( little endian )
    const char buffer[3] = { static_cast<char>(255), static_cast<char>(255), static_cast<char>(254) };
    outputFile.write(buffer, 3);

    std::string creator = "Created by ITK  \n\n";
    outputFile.write(const_cast<char *>(creator.c_str()), creator.size());

    auto numberOfPoints = static_cast<itk::uint32_t>(this->m_NumberOfPoints);
    auto numberOfCells = static_cast<itk::uint32_t>(this->m_NumberOfCells);
    itk::ByteSwapper<itk::uint32_t>::SwapWriteRangeFromSystemToBigEndian(&numberOfPoints, 1, &outputFile);
    itk::ByteSwapper<itk::uint32_t>::SwapWriteRangeFromSystemToBigEndian(&numberOfCells, 1, &outputFile);
  }
  else if (this->m_UpdatePointData && (!this->m_UpdatePoints && !this->m_UpdateCells))
  {
    // MAGIC_NUMBER = 16777215 ( little endian )
    const char buffer[3] = { static_cast<char>(255), static_cast<char>(255), static_cast<char>(255) };
    outputFile.write(buffer, 3);

    auto          numberOfPoints = static_cast<itk::uint32_t>(this->m_NumberOfPointPixels);
    auto          numberOfCells = static_cast<itk::uint32_t>(this->m_NumberOfCells);
    itk::uint32_t numberOfValuesPerPoint = 1;
    itk::ByteSwapper<itk::uint32_t>::SwapWriteRangeFromSystemToBigEndian(&numberOfPoints, 1, &outputFile);
    itk::ByteSwapper<itk::uint32_t>::SwapWriteRangeFromSystemToBigEndian(&numberOfCells, 1, &outputFile);
    itk::ByteSwapper<itk::uint32_t>::SwapWriteRangeFromSystemToBigEndian(&numberOfValuesPerPoint, 1, &outputFile);
  }

  outputFile.close();
}

void
FreeSurferBinaryMeshIO ::WritePoints(void * buffer)
{
  // check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str(), std::ios_base::app | std::ios::binary);

  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }

  // Write points
  switch (this->m_PointComponentType)
  {
    case IOComponentEnum::UCHAR:
    {
      WritePoints(static_cast<unsigned char *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::CHAR:
    {
      WritePoints(static_cast<char *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::USHORT:
    {
      WritePoints(static_cast<unsigned short *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::SHORT:
    {
      WritePoints(static_cast<short *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::UINT:
    {
      WritePoints(static_cast<unsigned int *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::INT:
    {
      WritePoints(static_cast<int *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::ULONG:
    {
      WritePoints(static_cast<unsigned long *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::LONG:
    {
      WritePoints(static_cast<long *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::ULONGLONG:
    {
      WritePoints(static_cast<unsigned long long *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::LONGLONG:
    {
      WritePoints(static_cast<long long *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::FLOAT:
    {
      WritePoints(static_cast<float *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::DOUBLE:
    {
      WritePoints(static_cast<double *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::LDOUBLE:
    {
      WritePoints(static_cast<long double *>(buffer), outputFile);

      break;
    }
    default:
    {
      itkExceptionMacro(<< "Unknown point pixel component type" << std::endl);
    }
  }

  outputFile.close();
}

void
FreeSurferBinaryMeshIO ::WriteCells(void * buffer)
{
  // Check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str(), std::ios_base::app | std::ios::binary);

  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }

  // Write triangles
  switch (this->m_CellComponentType)
  {
    case IOComponentEnum::UCHAR:
    {
      WriteCells(static_cast<unsigned char *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::CHAR:
    {
      WriteCells(static_cast<char *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::USHORT:
    {
      WriteCells(static_cast<unsigned short *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::SHORT:
    {
      WriteCells(static_cast<short *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::UINT:
    {
      WriteCells(static_cast<unsigned int *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::INT:
    {
      WriteCells(static_cast<int *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::ULONG:
    {
      WriteCells(static_cast<unsigned long *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::LONG:
    {
      WriteCells(static_cast<long *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::ULONGLONG:
    {
      WriteCells(static_cast<unsigned long long *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::LONGLONG:
    {
      WriteCells(static_cast<long long *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::FLOAT:
    {
      WriteCells(static_cast<float *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::DOUBLE:
    {
      WriteCells(static_cast<double *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::LDOUBLE:
    {
      WriteCells(static_cast<long double *>(buffer), outputFile);
      break;
    }
    default:
    {
      itkExceptionMacro(<< "Unknown cell component type" << std::endl);
    }
  }

  outputFile.close();
}

void
FreeSurferBinaryMeshIO ::WritePointData(void * buffer)
{
  // check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str(), std::ios_base::app | std::ios::binary);

  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }

  // Write point data
  switch (this->m_PointPixelComponentType)
  {
    case IOComponentEnum::UCHAR:
    {
      WritePointData(static_cast<unsigned char *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::CHAR:
    {
      WritePointData(static_cast<char *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::USHORT:
    {
      WritePointData(static_cast<unsigned short *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::SHORT:
    {
      WritePointData(static_cast<short *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::UINT:
    {
      WritePointData(static_cast<unsigned int *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::INT:
    {
      WritePointData(static_cast<int *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::ULONG:
    {
      WritePointData(static_cast<unsigned long *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::LONG:
    {
      WritePointData(static_cast<long *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::ULONGLONG:
    {
      WritePointData(static_cast<unsigned long long *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::LONGLONG:
    {
      WritePointData(static_cast<long long *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::FLOAT:
    {
      WritePointData(static_cast<float *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::DOUBLE:
    {
      WritePointData(static_cast<double *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::LDOUBLE:
    {
      WritePointData(static_cast<long double *>(buffer), outputFile);

      break;
    }
    default:
    {
      itkExceptionMacro(<< "Unknown point data pixel component type" << std::endl);
    }
  }

  outputFile.close();
}

void
FreeSurferBinaryMeshIO ::WriteCellData(void * itkNotUsed(buffer))
{}

void
FreeSurferBinaryMeshIO ::Write()
{}

void
FreeSurferBinaryMeshIO ::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // namespace itk
