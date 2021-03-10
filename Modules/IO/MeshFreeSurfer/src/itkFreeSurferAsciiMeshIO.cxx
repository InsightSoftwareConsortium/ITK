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

#include "itkFreeSurferAsciiMeshIO.h"

#include <itksys/SystemTools.hxx>

namespace itk
{
FreeSurferAsciiMeshIO ::FreeSurferAsciiMeshIO()
{
  this->AddSupportedWriteExtension(".fsa");
}

FreeSurferAsciiMeshIO::~FreeSurferAsciiMeshIO() = default;

bool
FreeSurferAsciiMeshIO ::CanReadFile(const char * fileName)
{
  if (!itksys::SystemTools::FileExists(fileName, true))
  {
    return false;
  }

  if (itksys::SystemTools::GetFilenameLastExtension(fileName) != ".fsa")
  {
    return false;
  }

  return true;
}

bool
FreeSurferAsciiMeshIO ::CanWriteFile(const char * fileName)
{
  if (itksys::SystemTools::GetFilenameLastExtension(fileName) != ".fsa")
  {
    return false;
  }

  return true;
}

void
FreeSurferAsciiMeshIO ::OpenFile()
{
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No input FileName");
  }

  if (!itksys::SystemTools::FileExists(m_FileName.c_str()))
  {
    itkExceptionMacro("File " << this->m_FileName << " does not exist");
  }

  this->m_InputFile.open(this->m_FileName.c_str(), std::ios::in);

  if (!m_InputFile.is_open())
  {
    itkExceptionMacro("Unable to open file " << this->m_FileName);
  }
}

void
FreeSurferAsciiMeshIO ::CloseFile()
{
  if (m_InputFile.is_open())
  {
    m_InputFile.close();
  }
}

void
FreeSurferAsciiMeshIO ::ReadMeshInformation()
{
  // Define input file stream and attach it to input file
  OpenFile();

  // Read the ASCII file information
  constexpr int          numberOfInfoLines = 1;
  constexpr unsigned int numberOfCellPoints = 3;
  std::string            line;

  for (int ii = 0; ii < numberOfInfoLines; ii++)
  {
    std::getline(m_InputFile, line);
  }
  this->m_FileType = IOFileEnum::ASCII;

  // Read the number of points and number of cells
  m_InputFile >> this->m_NumberOfPoints;
  m_InputFile >> this->m_NumberOfCells;
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

  // Set default point pixel component and point pixel type
  this->m_PointPixelComponentType = IOComponentEnum::FLOAT;
  this->m_PointPixelType = IOPixelEnum::SCALAR;
  this->m_NumberOfPointPixelComponents = itk::NumericTraits<unsigned int>::OneValue();

  // Set default cell pixel component and point pixel type
  this->m_CellPixelComponentType = IOComponentEnum::FLOAT;
  this->m_CellPixelType = IOPixelEnum::SCALAR;
  this->m_NumberOfCellPixelComponents = itk::NumericTraits<unsigned int>::OneValue();
}

void
FreeSurferAsciiMeshIO ::ReadPoints(void * buffer)
{
  // Number of data array
  auto * data = static_cast<float *>(buffer);

  // Read points
  m_InputFile.precision(12);
  float value;

  SizeValueType index = 0;
  for (SizeValueType id = 0; id < this->m_NumberOfPoints; id++)
  {
    for (unsigned int ii = 0; ii < this->m_PointDimension; ii++)
    {
      m_InputFile >> data[index++];
    }
    m_InputFile >> value;
  }
}

void
FreeSurferAsciiMeshIO ::ReadCells(void * buffer)
{
  // Get cell buffer
  m_InputFile.precision(12);
  SizeValueType          index = 0;
  constexpr unsigned int numberOfCellPoints = 3;
  auto *                 data = new unsigned int[this->m_NumberOfCells * numberOfCellPoints];
  float                  value;

  for (SizeValueType id = 0; id < this->m_NumberOfCells; id++)
  {
    for (unsigned int ii = 0; ii < numberOfCellPoints; ii++)
    {
      m_InputFile >> data[index++];
    }
    m_InputFile >> value;
  }

  this->WriteCellsBuffer(
    data, static_cast<unsigned int *>(buffer), CellGeometryEnum::TRIANGLE_CELL, 3, this->m_NumberOfCells);
  delete[] data;

  CloseFile();
}

void
FreeSurferAsciiMeshIO ::ReadPointData(void * itkNotUsed(buffer))
{}

void
FreeSurferAsciiMeshIO ::ReadCellData(void * itkNotUsed(buffer))
{}

void
FreeSurferAsciiMeshIO ::WriteMeshInformation()
{
  // Check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str());

  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }

  // Write FreeSurfer Surface file header
  outputFile << "#!ascii version of " << this->m_FileName << std::endl;

  // Write the number of points and number of cells
  outputFile << this->m_NumberOfPoints << "    " << this->m_NumberOfCells << std::endl;

  outputFile.close();
}

void
FreeSurferAsciiMeshIO ::WritePoints(void * buffer)
{
  // check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str(), std::ios_base::app);

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
      WritePoints(static_cast<unsigned char *>(buffer), outputFile, itk::NumericTraits<unsigned char>::ZeroValue());
      break;
    }
    case IOComponentEnum::CHAR:
    {
      WritePoints(static_cast<char *>(buffer), outputFile, itk::NumericTraits<char>::ZeroValue());

      break;
    }
    case IOComponentEnum::USHORT:
    {
      WritePoints(static_cast<unsigned short *>(buffer), outputFile, itk::NumericTraits<unsigned short>::ZeroValue());

      break;
    }
    case IOComponentEnum::SHORT:
    {
      WritePoints(static_cast<short *>(buffer), outputFile, itk::NumericTraits<short>::ZeroValue());

      break;
    }
    case IOComponentEnum::UINT:
    {
      WritePoints(static_cast<unsigned int *>(buffer), outputFile, itk::NumericTraits<unsigned int>::ZeroValue());

      break;
    }
    case IOComponentEnum::INT:
    {
      WritePoints(static_cast<int *>(buffer), outputFile, itk::NumericTraits<int>::ZeroValue());

      break;
    }
    case IOComponentEnum::ULONG:
    {
      WritePoints(static_cast<unsigned long *>(buffer), outputFile, itk::NumericTraits<unsigned long>::ZeroValue());

      break;
    }
    case IOComponentEnum::LONG:
    {
      WritePoints(static_cast<long *>(buffer), outputFile, itk::NumericTraits<long>::ZeroValue());

      break;
    }
    case IOComponentEnum::ULONGLONG:
    {
      WritePoints(static_cast<unsigned long long *>(buffer),
                  outputFile,
                  static_cast<unsigned long long>(itk::NumericTraits<unsigned long>::ZeroValue()));

      break;
    }
    case IOComponentEnum::LONGLONG:
    {
      WritePoints(
        static_cast<long long *>(buffer), outputFile, static_cast<long long>(itk::NumericTraits<long>::ZeroValue()));

      break;
    }
    case IOComponentEnum::FLOAT:
    {
      WritePoints(static_cast<float *>(buffer), outputFile, itk::NumericTraits<float>::ZeroValue());

      break;
    }
    case IOComponentEnum::DOUBLE:
    {
      WritePoints(static_cast<double *>(buffer), outputFile, itk::NumericTraits<double>::ZeroValue());

      break;
    }
    case IOComponentEnum::LDOUBLE:
    {
      WritePoints(static_cast<long double *>(buffer), outputFile, itk::NumericTraits<long double>::ZeroValue());

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
FreeSurferAsciiMeshIO ::WriteCells(void * buffer)
{
  // Check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str(), std::ios_base::app);

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
      WriteCells(static_cast<unsigned char *>(buffer), outputFile);
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
      WriteCells(static_cast<long *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::LONG:
    {
      WriteCells(static_cast<long *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::ULONGLONG:
    {
      WriteCells(static_cast<unsigned long long *>(buffer),
                 outputFile,
                 static_cast<unsigned long long>(itk::NumericTraits<unsigned long>::ZeroValue()));
      break;
    }
    case IOComponentEnum::LONGLONG:
    {
      WriteCells(
        static_cast<long long *>(buffer), outputFile, static_cast<long long>(itk::NumericTraits<long>::ZeroValue()));
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
      itkExceptionMacro(<< "Unknown cell pixel component type" << std::endl);
    }
  }

  outputFile.close();
}

void
FreeSurferAsciiMeshIO ::WritePointData(void * itkNotUsed(buffer))
{}

void
FreeSurferAsciiMeshIO ::WriteCellData(void * itkNotUsed(buffer))
{}

void
FreeSurferAsciiMeshIO ::Write()
{}

void
FreeSurferAsciiMeshIO ::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // namespace itk
