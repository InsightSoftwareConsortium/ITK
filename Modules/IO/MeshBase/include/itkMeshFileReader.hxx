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
#ifndef itkMeshFileReader_hxx
#define itkMeshFileReader_hxx

#include "itkConvertPixelBuffer.h"
#include "itkConvertArrayPixelBuffer.h"
#include "itkConvertVariableLengthVectorPixelBuffer.h"
#include "itkMeshIOFactory.h"
#include "itkMeshFileReader.h"
#include "itkMeshRegion.h"
#include "itkObjectFactory.h"
#include "itkPixelTraits.h"

#include <itksys/SystemTools.hxx>
#include <fstream>

namespace itk
{
template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::MeshFileReader()
{
  m_MeshIO = nullptr;
  m_FileName = "";
  m_UserSpecifiedMeshIO = false;
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::PrintSelf(std::ostream & os,
                                                                                        Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  if (m_MeshIO)
  {
    os << indent << "MeshIO: \n";
    m_MeshIO->Print(os, indent.GetNextIndent());
  }
  else
  {
    os << indent << "MeshIO: (null)"
       << "\n";
  }

  os << indent << "UserSpecifiedMeshIO flag: " << m_UserSpecifiedMeshIO << "\n";
  os << indent << "FileName: " << m_FileName << "\n";
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::SetMeshIO(MeshIOBase * meshIO)
{
  itkDebugMacro("setting MeshIO to " << meshIO);

  if (this->m_MeshIO != meshIO)
  {
    this->m_MeshIO = meshIO;
    this->Modified();
  }

  m_UserSpecifiedMeshIO = true;
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::TestFileExistanceAndReadability()
{
  // Test if the file exists.
  if (!itksys::SystemTools::FileExists(m_FileName.c_str()))
  {
    MeshFileReaderException e(__FILE__, __LINE__);
    std::ostringstream      msg;
    msg << "The file doesn't exist. " << std::endl << "Filename = " << m_FileName << std::endl;
    e.SetDescription(msg.str().c_str());
    throw e;
    return;
  }

  // Test if the file can be open for reading access.
  std::ifstream readTester;
  readTester.open(m_FileName.c_str());
  if (readTester.fail())
  {
    readTester.close();
    std::ostringstream msg;
    msg << "The file couldn't be opened for reading. " << std::endl << "Filename: " << m_FileName << std::endl;
    MeshFileReaderException e(__FILE__, __LINE__, msg.str().c_str(), ITK_LOCATION);
    throw e;
    return;
  }
  readTester.close();
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
template <typename T>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::ReadPoints(T * buffer)
{
  typename TOutputMesh::Pointer output = this->GetOutput();
  output->GetPoints()->Reserve(m_MeshIO->GetNumberOfPoints());
  OutputPointType point;

  for (OutputPointIdentifier id = 0; id < output->GetNumberOfPoints(); id++)
  {
    for (OutputPointIdentifier ii = 0; ii < OutputPointDimension; ii++)
    {
      point[ii] = static_cast<typename OutputPointType::ValueType>(buffer[id * OutputPointDimension + ii]);
    }

    output->SetPoint(id, point);
  }
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
template <typename T>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::ReadCells(T * buffer)
{
  typename TOutputMesh::Pointer output = this->GetOutput();

  SizeValueType        index = NumericTraits<SizeValueType>::ZeroValue();
  OutputCellIdentifier id = NumericTraits<OutputCellIdentifier>::ZeroValue();
  while (index < m_MeshIO->GetCellBufferSize())
  {
    auto type = static_cast<CellGeometryEnum>(static_cast<int>(buffer[index++]));
    switch (type)
    {
      case CellGeometryEnum::VERTEX_CELL:
      {
        auto numberOfPoints = static_cast<unsigned int>(buffer[index++]);
        if (numberOfPoints != OutputVertexCellType::NumberOfPoints)
        {
          itkExceptionMacro(<< "Invalid Vertex Cell with number of points = " << numberOfPoints);
        }
        OutputCellAutoPointer cell;
        auto *                vertexCell = new OutputVertexCellType;
        for (unsigned int jj = 0; jj < OutputVertexCellType::NumberOfPoints; jj++)
        {
          vertexCell->SetPointId(jj, static_cast<OutputPointIdentifier>(buffer[index++]));
        }

        cell.TakeOwnership(vertexCell);
        output->SetCell(id++, cell);
        break;
      }
      case CellGeometryEnum::LINE_CELL:
      {
        // for polylines will be loaded as individual edges.
        auto numberOfPoints = static_cast<unsigned int>(buffer[index++]);
        if (numberOfPoints < 2)
        {
          itkExceptionMacro(<< "Invalid Line Cell with number of points = " << numberOfPoints);
        }
        auto pointIDBuffer = static_cast<OutputPointIdentifier>(buffer[index++]);
        for (unsigned int jj = 1; jj < numberOfPoints; ++jj)
        {
          OutputCellAutoPointer cell;
          auto *                lineCell = new OutputLineCellType;
          lineCell->SetPointId(0, pointIDBuffer);
          pointIDBuffer = static_cast<OutputPointIdentifier>(buffer[index++]);
          lineCell->SetPointId(1, pointIDBuffer);
          cell.TakeOwnership(lineCell);
          output->SetCell(id++, cell);
        }
        break;
      }
      case CellGeometryEnum::TRIANGLE_CELL:
      {
        auto numberOfPoints = static_cast<unsigned int>(buffer[index++]);
        if (numberOfPoints != OutputTriangleCellType::NumberOfPoints)
        {
          itkExceptionMacro(<< "Invalid Triangle Cell with number of points = " << numberOfPoints);
        }

        OutputCellAutoPointer cell;
        auto *                triangleCell = new OutputTriangleCellType;
        for (unsigned int jj = 0; jj < OutputTriangleCellType::NumberOfPoints; jj++)
        {
          triangleCell->SetPointId(jj, static_cast<OutputPointIdentifier>(buffer[index++]));
        }

        cell.TakeOwnership(triangleCell);
        output->SetCell(id++, cell);
        break;
      }
      case CellGeometryEnum::QUADRILATERAL_CELL:
      {
        auto numberOfPoints = static_cast<unsigned int>(buffer[index++]);
        if (numberOfPoints != OutputQuadrilateralCellType::NumberOfPoints)
        {
          itkExceptionMacro(<< "Invalid Quadrilateral Cell with number of points = " << numberOfPoints);
        }

        OutputCellAutoPointer cell;
        auto *                quadrilateralCell = new OutputQuadrilateralCellType;
        for (unsigned int jj = 0; jj < OutputQuadrilateralCellType::NumberOfPoints; jj++)
        {
          quadrilateralCell->SetPointId(jj, static_cast<OutputPointIdentifier>(buffer[index++]));
        }

        cell.TakeOwnership(quadrilateralCell);
        output->SetCell(id++, cell);
        break;
      }
      case CellGeometryEnum::POLYGON_CELL:
      {
        // For polyhedron, if the number of points is 3, then we treat it as
        // triangle cell
        OutputCellAutoPointer cell;
        auto                  numberOfPoints = static_cast<unsigned int>(buffer[index++]);
        if (numberOfPoints == OutputTriangleCellType::NumberOfPoints)
        {
          auto * triangleCell = new OutputTriangleCellType;
          for (unsigned int jj = 0; jj < OutputTriangleCellType::NumberOfPoints; jj++)
          {
            triangleCell->SetPointId(jj, static_cast<OutputPointIdentifier>(buffer[index++]));
          }
          cell.TakeOwnership(triangleCell);
        }
        else
        {
          auto * polygonCell = new OutputPolygonCellType;
          for (unsigned int jj = 0; jj < numberOfPoints; jj++)
          {
            polygonCell->SetPointId(jj, static_cast<OutputPointIdentifier>(buffer[index++]));
          }
          cell.TakeOwnership(polygonCell);
        }

        output->SetCell(id++, cell);
        break;
      }
      case CellGeometryEnum::TETRAHEDRON_CELL:
      {
        auto numberOfPoints = static_cast<unsigned int>(buffer[index++]);
        if (numberOfPoints != OutputTetrahedronCellType::NumberOfPoints)
        {
          itkExceptionMacro(<< "Invalid Tetrahedron Cell with number of points = " << numberOfPoints);
        }

        OutputCellAutoPointer cell;
        auto *                tetrahedronCell = new OutputTetrahedronCellType;
        for (unsigned int jj = 0; jj < OutputTetrahedronCellType::NumberOfPoints; jj++)
        {
          tetrahedronCell->SetPointId(jj, static_cast<OutputPointIdentifier>(buffer[index++]));
        }

        cell.TakeOwnership(tetrahedronCell);
        output->SetCell(id++, cell);
        break;
      }
      case CellGeometryEnum::HEXAHEDRON_CELL:
      {
        auto numberOfPoints = static_cast<unsigned int>(buffer[index++]);
        if (numberOfPoints != OutputHexahedronCellType::NumberOfPoints)
        {
          itkExceptionMacro(<< "Invalid Hexahedron Cell with number of points = " << numberOfPoints);
        }

        OutputCellAutoPointer cell;
        auto *                hexahedronCell = new OutputHexahedronCellType;
        for (unsigned int jj = 0; jj < OutputHexahedronCellType::NumberOfPoints; jj++)
        {
          hexahedronCell->SetPointId(jj, static_cast<OutputPointIdentifier>(buffer[index++]));
        }

        cell.TakeOwnership(hexahedronCell);
        output->SetCell(id++, cell);
        break;
      }
      case CellGeometryEnum::QUADRATIC_EDGE_CELL:
      {
        auto numberOfPoints = static_cast<unsigned int>(buffer[index++]);
        if (numberOfPoints != OutputQuadraticEdgeCellType::NumberOfPoints)
        {
          itkExceptionMacro(<< "Invalid Quadratic edge Cell with number of points = " << numberOfPoints);
        }

        OutputCellAutoPointer cell;
        auto *                quadraticEdgeCell = new OutputQuadraticEdgeCellType;
        for (unsigned int jj = 0; jj < OutputQuadraticEdgeCellType::NumberOfPoints; jj++)
        {
          quadraticEdgeCell->SetPointId(jj, static_cast<OutputPointIdentifier>(buffer[index++]));
        }

        cell.TakeOwnership(quadraticEdgeCell);
        output->SetCell(id++, cell);
        break;
      }
      case CellGeometryEnum::QUADRATIC_TRIANGLE_CELL:
      {
        auto numberOfPoints = static_cast<unsigned int>(buffer[index++]);
        if (numberOfPoints != OutputQuadraticTriangleCellType::NumberOfPoints)
        {
          itkExceptionMacro(<< "Invalid Quadratic triangle Cell with number of points = " << numberOfPoints);
        }

        OutputCellAutoPointer cell;
        auto *                quadraticTriangleCell = new OutputQuadraticTriangleCellType;
        for (unsigned int jj = 0; jj < OutputQuadraticTriangleCellType::NumberOfPoints; jj++)
        {
          quadraticTriangleCell->SetPointId(jj, static_cast<OutputPointIdentifier>(buffer[index++]));
        }

        cell.TakeOwnership(quadraticTriangleCell);
        output->SetCell(id++, cell);
        break;
      }
      default:
      {
        itkExceptionMacro(<< "Unknown cell type");
      }
    }
  }
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::ReadPointData()
{
  typename TOutputMesh::Pointer output = this->GetOutput();

  char * inputPointDataBuffer = nullptr;
  auto * outputPointDataBuffer = new OutputPointPixelType[m_MeshIO->GetNumberOfPointPixels()];

  try
  {
    if ((m_MeshIO->GetPointPixelComponentType() !=
         MeshIOBase::MapComponentType<typename ConvertPointPixelTraits::ComponentType>::CType) ||
        (m_MeshIO->GetNumberOfPointPixelComponents() != ConvertPointPixelTraits::GetNumberOfComponents()))
    {
      // the point pixel types don't match a type conversion needs to be
      // performed
      itkDebugMacro(<< "Buffer conversion required from: "
                    << m_MeshIO->GetComponentTypeAsString(m_MeshIO->GetPointPixelComponentType()) << " to: "
                    << m_MeshIO->GetComponentTypeAsString(
                         MeshIOBase::MapComponentType<typename ConvertPointPixelTraits::ComponentType>::CType)
                    << "ConvertPointPixelTraits::NumberOfComponents "
                    << ConvertPointPixelTraits::GetNumberOfComponents() << " m_MeshIO->NumberOfComponents "
                    << m_MeshIO->GetNumberOfPointPixelComponents());

      inputPointDataBuffer = new char[m_MeshIO->GetNumberOfPointPixelComponents() *
                                      m_MeshIO->GetComponentSize(m_MeshIO->GetPointPixelComponentType()) *
                                      m_MeshIO->GetNumberOfPointPixels()];
      m_MeshIO->ReadPointData(static_cast<void *>(inputPointDataBuffer));

      this->ConvertPointPixelBuffer(
        static_cast<void *>(inputPointDataBuffer), outputPointDataBuffer, m_MeshIO->GetNumberOfPointPixels());
    }
    else
    {
      itkDebugMacro(<< "No buffer conversion required.");
      m_MeshIO->ReadPointData(static_cast<void *>(outputPointDataBuffer));
    }
  }
  catch (...)
  {
    // if an exception is thrown catch it

    // clean up
    delete[] inputPointDataBuffer;
    inputPointDataBuffer = nullptr;

    delete[] outputPointDataBuffer;
    outputPointDataBuffer = nullptr;

    // then rethrow
    throw;
  }

  // clean up
  delete[] inputPointDataBuffer;
  inputPointDataBuffer = nullptr;

  for (OutputPointIdentifier id = 0; id < m_MeshIO->GetNumberOfPointPixels(); id++)
  {
    output->SetPointData(id, outputPointDataBuffer[id]);
  }

  delete[] outputPointDataBuffer;
  outputPointDataBuffer = nullptr;
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::ReadCellData()
{
  typename TOutputMesh::Pointer output = this->GetOutput();

  char * inputCellDataBuffer = nullptr;
  auto * outputCellDataBuffer = new OutputCellPixelType[m_MeshIO->GetNumberOfCellPixels()];

  try
  {
    if ((m_MeshIO->GetCellPixelComponentType() !=
         MeshIOBase::MapComponentType<typename ConvertCellPixelTraits::ComponentType>::CType) ||
        (m_MeshIO->GetNumberOfCellPixelComponents() != ConvertCellPixelTraits::GetNumberOfComponents()))
    {
      // the cell pixel types don't match a type conversion needs to be
      // performed
      itkDebugMacro(<< "Buffer conversion required from: "
                    << m_MeshIO->GetComponentTypeAsString(m_MeshIO->GetCellPixelComponentType()) << " to: "
                    << m_MeshIO->GetComponentTypeAsString(
                         MeshIOBase::MapComponentType<typename ConvertCellPixelTraits::ComponentType>::CType)
                    << "ConvertCellPixelTraits::NumberOfComponents " << ConvertCellPixelTraits::GetNumberOfComponents()
                    << " m_MeshIO->NumberOfComponents " << m_MeshIO->GetNumberOfCellPixelComponents());

      inputCellDataBuffer =
        new char[m_MeshIO->GetNumberOfCellPixelComponents() *
                 m_MeshIO->GetComponentSize(m_MeshIO->GetCellPixelComponentType()) * m_MeshIO->GetNumberOfCellPixels()];
      m_MeshIO->ReadCellData(static_cast<void *>(inputCellDataBuffer));

      this->ConvertCellPixelBuffer(
        static_cast<void *>(inputCellDataBuffer), outputCellDataBuffer, m_MeshIO->GetNumberOfCellPixels());
    }
    else
    {
      itkDebugMacro(<< "No buffer conversion required.");
      m_MeshIO->ReadCellData(static_cast<void *>(outputCellDataBuffer));
    }
  }
  catch (...)
  {
    // if an exception is thrown catch it

    // clean up
    delete[] inputCellDataBuffer;
    inputCellDataBuffer = nullptr;

    delete[] outputCellDataBuffer;
    outputCellDataBuffer = nullptr;

    // then rethrow
    throw;
  }

  // clean up
  delete[] inputCellDataBuffer;
  inputCellDataBuffer = nullptr;

  for (OutputCellIdentifier id = 0; id < m_MeshIO->GetNumberOfCellPixels(); id++)
  {
    output->SetCellData(id, outputCellDataBuffer[id]);
  }

  delete[] outputCellDataBuffer;
  outputCellDataBuffer = nullptr;
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::GenerateOutputInformation()
{
  if (m_FileName.empty())
  {
    throw MeshFileReaderException(__FILE__, __LINE__, "FileName must be specified", ITK_LOCATION);
  }

  try
  {
    m_ExceptionMessage = "";
    this->TestFileExistanceAndReadability();
  }
  catch (const itk::ExceptionObject & err)
  {
    m_ExceptionMessage = err.GetDescription();
  }

  if (m_UserSpecifiedMeshIO == false) // try creating via factory
  {
    m_MeshIO = MeshIOFactory::CreateMeshIO(m_FileName.c_str(), MeshIOFactory::IOFileModeEnum::ReadMode);
  }

  if (m_MeshIO.IsNull())
  {
    std::ostringstream msg;
    msg << " Could not create IO object for file " << m_FileName.c_str() << std::endl;
    if (!m_ExceptionMessage.empty())
    {
      msg << m_ExceptionMessage;
    }
    else
    {
      msg << "  Tried to create one of the following:" << std::endl;
      for (auto & allobject : ObjectFactoryBase::CreateAllInstance("itkMeshIOBase"))
      {
        auto * io = dynamic_cast<MeshIOBase *>(allobject.GetPointer());
        msg << "    " << io->GetNameOfClass() << std::endl;
      }
      msg << "  You probably failed to set a file suffix, or" << std::endl;
      msg << "  set the suffix to an unsupported type." << std::endl;
    }

    MeshFileReaderException e(__FILE__, __LINE__, msg.str().c_str(), ITK_LOCATION);
    throw e;
    return;
  }
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::GenerateData()
{
  typename TOutputMesh::Pointer output = this->GetOutput();

  // Test if the file exists and if it can be opened.
  // An exception will be thrown otherwise, since we can't
  // successfully read the file. We catch the exception because some
  // MeshIO's may not actually open a file. Still
  // reports file error if no MeshIO is loaded.

  output->Initialize();
  output->SetBufferedRegion(output->GetRequestedRegion());

  // Test existence and readability of input file
  try
  {
    m_ExceptionMessage = "";
    this->TestFileExistanceAndReadability();
  }
  catch (const itk::ExceptionObject & err)
  {
    m_ExceptionMessage = err.GetDescription();
  }

  // Tell the MeshIO to read the file
  m_MeshIO->SetFileName(m_FileName.c_str());

  // Get mesh information
  m_MeshIO->ReadMeshInformation();

  // Read points
  if (m_MeshIO->GetUpdatePoints())
  {
    switch (m_MeshIO->GetPointComponentType())
    {
      case IOComponentEnum::CHAR:
      {
        auto * pointsBuffer = new char[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::UCHAR:
      {
        auto * pointsBuffer = new unsigned char[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::SHORT:
      {
        auto * pointsBuffer = new short[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::USHORT:
      {
        auto * pointsBuffer = new unsigned short[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::INT:
      {
        auto * pointsBuffer = new int[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::UINT:
      {
        auto * pointsBuffer = new unsigned int[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::LONG:
      {
        auto * pointsBuffer = new long[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::ULONG:
      {
        auto * pointsBuffer = new unsigned long[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::LONGLONG:
      {
        auto * pointsBuffer = new long long[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::ULONGLONG:
      {
        auto * pointsBuffer = new unsigned long long[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::FLOAT:
      {
        auto * pointsBuffer = new float[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::DOUBLE:
      {
        auto * pointsBuffer = new double[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::LDOUBLE:
      {
        auto * pointsBuffer = new long double[m_MeshIO->GetNumberOfPoints() * OutputPointDimension];
        m_MeshIO->ReadPoints(static_cast<void *>(pointsBuffer));
        ReadPoints(pointsBuffer);
        delete[] pointsBuffer;
        break;
      }
      case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      default:
      {
        itkExceptionMacro(<< "Unknown point component type");
      }
    }
  }

  // Read cells
  if (m_MeshIO->GetUpdateCells())
  {
    switch (m_MeshIO->GetCellComponentType())
    {
      case IOComponentEnum::CHAR:
      {
        auto * cellsBuffer = new char[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::UCHAR:
      {
        auto * cellsBuffer = new unsigned char[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::SHORT:
      {
        auto * cellsBuffer = new short[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::USHORT:
      {
        auto * cellsBuffer = new unsigned short[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::INT:
      {
        auto * cellsBuffer = new int[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::UINT:
      {
        auto * cellsBuffer = new unsigned int[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::LONG:
      {
        auto * cellsBuffer = new long[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::ULONG:
      {
        auto * cellsBuffer = new unsigned long[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::LONGLONG:
      {
        auto * cellsBuffer = new long long[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::ULONGLONG:
      {
        auto * cellsBuffer = new unsigned long long[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::FLOAT:
      {
        auto * cellsBuffer = new float[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::DOUBLE:
      {
        auto * cellsBuffer = new double[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::LDOUBLE:
      {
        auto * cellsBuffer = new long double[m_MeshIO->GetCellBufferSize()];
        m_MeshIO->ReadCells(static_cast<void *>(cellsBuffer));
        ReadCells(cellsBuffer);
        delete[] cellsBuffer;
        break;
      }
      case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      default:
      {
        itkExceptionMacro(<< "Unknown cell component type");
      }
    }
  }

  // Read Point Data
  if (m_MeshIO->GetUpdatePointData())
  {
    ReadPointData();
  }

  // Read Cell Data
  if (m_MeshIO->GetUpdateCellData())
  {
    ReadCellData();
  }
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
template <typename T>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::ConvertPointPixelBuffer(
  void * inputData,
  T *    outputData,
  size_t numberOfPixels)
{
  // TODO:
  // Pass down the PixelType (RGB, VECTOR, etc.) so that any vector to
  // scalar conversion be type specific. i.e. RGB to scalar would use
  // a formula to convert to luminance, VECTOR to scalar would use
  // vector magnitude.

  // Create a macro as this code is a bit lengthy and repetitive
  // if the MeshIO pixel type is typeid(type) then use the ConvertPixelBuffer
  // class to convert the data block to TOutputMesh's pixel type
  // see DefaultConvertPixelTraits and ConvertPixelBuffer

#define ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(CType, type)                                                           \
  else if (m_MeshIO->GetPointPixelComponentType() == CType)                                                            \
  {                                                                                                                    \
    ConvertPixelBuffer<type, OutputPointPixelType, ConvertPointPixelTraits>::Convert(                                  \
      static_cast<type *>(inputData), m_MeshIO->GetNumberOfPointPixelComponents(), outputData, numberOfPixels);        \
  }

  if (false)
  {
  }
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::UCHAR, unsigned char)
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::CHAR, char)
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::USHORT, unsigned short)
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::SHORT, short)
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::UINT, unsigned int)
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::INT, int)
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::ULONG, unsigned long)
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::LONG, long)
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::ULONGLONG, unsigned long long)
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::LONGLONG, long long)
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::FLOAT, float)
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::DOUBLE, double)
  ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::LDOUBLE, long double)
  else
  {
    MeshFileReaderException e(__FILE__, __LINE__);
    std::ostringstream      msg;
    msg << "Couldn't convert component type: " << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(m_MeshIO->GetPointPixelComponentType()) << std::endl
        << "to one of: " << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned char>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<char>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned short>::CType)
        << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<short>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned int>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<int>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned long>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<long>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned long long>::CType)
        << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<long long>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<float>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<double>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<long double>::CType) << std::endl;
    e.SetDescription(msg.str().c_str());
    e.SetLocation(ITK_LOCATION);
    throw e;
    return;
  }
#undef ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
template <typename T>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::ConvertCellPixelBuffer(
  void * inputData,
  T *    outputData,
  size_t numberOfPixels)
{
  // TODO:
  // Pass down the PixelType (RGB, VECTOR, etc.) so that any vector to
  // scalar conversion be type specific. i.e. RGB to scalar would use
  // a formula to convert to luminance, VECTOR to scalar would use
  // vector magnitude.

  // Create a macro as this code is a bit lengthy and repetitive
  // if the MeshIO pixel type is typeid(type) then use the ConvertPixelBuffer
  // class to convert the data block to TOutputMesh's pixel type
  // see DefaultConvertPixelTraits and ConvertPixelBuffer

#define ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(CType, type)                                                            \
  else if (m_MeshIO->GetCellPixelComponentType() == CType)                                                             \
  {                                                                                                                    \
    ConvertPixelBuffer<type, OutputCellPixelType, ConvertCellPixelTraits>::Convert(                                    \
      static_cast<type *>(inputData), m_MeshIO->GetNumberOfCellPixelComponents(), outputData, numberOfPixels);         \
  }

  if (false)
  {
  }
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::UCHAR, unsigned char)
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::CHAR, char)
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::USHORT, unsigned short)
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::SHORT, short)
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::UINT, unsigned int)
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::INT, int)
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::ULONG, unsigned long)
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::LONG, long)
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::ULONGLONG, unsigned long long)
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::LONGLONG, long long)
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::FLOAT, float)
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::DOUBLE, double)
  ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(IOComponentEnum::LDOUBLE, long double)
  else
  {
    MeshFileReaderException e(__FILE__, __LINE__);
    std::ostringstream      msg;
    msg << "Couldn't convert component type: " << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(m_MeshIO->GetCellPixelComponentType()) << std::endl
        << "to one of: " << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned char>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<char>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned short>::CType)
        << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<short>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned int>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<int>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned long>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<long>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned long long>::CType)
        << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<long long>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<float>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<double>::CType) << std::endl
        << "    " << m_MeshIO->GetComponentTypeAsString(MeshIOBase::MapComponentType<long double>::CType) << std::endl;
    e.SetDescription(msg.str().c_str());
    e.SetLocation(ITK_LOCATION);
    throw e;
    return;
  }
#undef ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK
}
} // namespace itk
#endif
