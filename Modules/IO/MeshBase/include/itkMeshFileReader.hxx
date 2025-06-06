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
#ifndef itkMeshFileReader_hxx
#define itkMeshFileReader_hxx

#include "itkConvertPixelBuffer.h"
#include "itkConvertArrayPixelBuffer.h"
#include "itkConvertVariableLengthVectorPixelBuffer.h"
#include "itkMeshIOFactory.h"
#include "itkMeshRegion.h"
#include "itkObjectFactory.h"
#include "itkPixelTraits.h"

#include "itksys/SystemTools.hxx"
#include "itkMakeUniqueForOverwrite.h"

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

  itkPrintSelfObjectMacro(MeshIO);

  os << indent << "UserSpecifiedMeshIO flag: " << m_UserSpecifiedMeshIO << '\n';
  os << indent << "FileName: " << m_FileName << '\n';
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
  }
  readTester.close();
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
template <typename T>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::ReadPoints(T * buffer)
{
  const typename TOutputMesh::Pointer output = this->GetOutput();
  output->GetPoints()->Reserve(m_MeshIO->GetNumberOfPoints());
  OutputPointType point;

  for (OutputPointIdentifier id = 0; id < output->GetNumberOfPoints(); ++id)
  {
    for (OutputPointIdentifier ii = 0; ii < OutputPointDimension; ++ii)
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
  const typename TOutputMesh::Pointer output = this->GetOutput();

  SizeValueType        index{};
  OutputCellIdentifier id{};
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
          itkExceptionMacro("Invalid Vertex Cell with number of points = " << numberOfPoints);
        }
        OutputCellAutoPointer cell;
        auto *                vertexCell = new OutputVertexCellType;
        for (unsigned int jj = 0; jj < OutputVertexCellType::NumberOfPoints; ++jj)
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
          itkExceptionMacro("Invalid Line Cell with number of points = " << numberOfPoints);
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
      case CellGeometryEnum::POLYLINE_CELL:
      {
        auto numberOfPoints = static_cast<unsigned int>(buffer[index++]);
        if (numberOfPoints < 2)
        {
          itkExceptionMacro("Invalid Line Cell with number of points = " << numberOfPoints);
        }
        auto *                polyLineCell = new OutputPolyLineCellType;
        OutputCellAutoPointer cell;

        for (unsigned int jj = 0; jj < numberOfPoints; ++jj)
        {
          auto pointIDBuffer = static_cast<OutputPointIdentifier>(buffer[index++]);
          polyLineCell->SetPointId(jj, pointIDBuffer);
        }

        cell.TakeOwnership(polyLineCell);
        output->SetCell(id++, cell);
        break;
      }
      case CellGeometryEnum::TRIANGLE_CELL:
      {
        auto numberOfPoints = static_cast<unsigned int>(buffer[index++]);
        if (numberOfPoints != OutputTriangleCellType::NumberOfPoints)
        {
          itkExceptionMacro("Invalid Triangle Cell with number of points = " << numberOfPoints);
        }

        OutputCellAutoPointer cell;
        auto *                triangleCell = new OutputTriangleCellType;
        for (unsigned int jj = 0; jj < OutputTriangleCellType::NumberOfPoints; ++jj)
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
          itkExceptionMacro("Invalid Quadrilateral Cell with number of points = " << numberOfPoints);
        }

        OutputCellAutoPointer cell;
        auto *                quadrilateralCell = new OutputQuadrilateralCellType;
        for (unsigned int jj = 0; jj < OutputQuadrilateralCellType::NumberOfPoints; ++jj)
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
          for (unsigned int jj = 0; jj < OutputTriangleCellType::NumberOfPoints; ++jj)
          {
            triangleCell->SetPointId(jj, static_cast<OutputPointIdentifier>(buffer[index++]));
          }
          cell.TakeOwnership(triangleCell);
        }
        else
        {
          auto * polygonCell = new OutputPolygonCellType;
          for (unsigned int jj = 0; jj < numberOfPoints; ++jj)
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
          itkExceptionMacro("Invalid Tetrahedron Cell with number of points = " << numberOfPoints);
        }

        OutputCellAutoPointer cell;
        auto *                tetrahedronCell = new OutputTetrahedronCellType;
        for (unsigned int jj = 0; jj < OutputTetrahedronCellType::NumberOfPoints; ++jj)
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
          itkExceptionMacro("Invalid Hexahedron Cell with number of points = " << numberOfPoints);
        }

        OutputCellAutoPointer cell;
        auto *                hexahedronCell = new OutputHexahedronCellType;
        for (unsigned int jj = 0; jj < OutputHexahedronCellType::NumberOfPoints; ++jj)
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
          itkExceptionMacro("Invalid Quadratic edge Cell with number of points = " << numberOfPoints);
        }

        OutputCellAutoPointer cell;
        auto *                quadraticEdgeCell = new OutputQuadraticEdgeCellType;
        for (unsigned int jj = 0; jj < OutputQuadraticEdgeCellType::NumberOfPoints; ++jj)
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
          itkExceptionMacro("Invalid Quadratic triangle Cell with number of points = " << numberOfPoints);
        }

        OutputCellAutoPointer cell;
        auto *                quadraticTriangleCell = new OutputQuadraticTriangleCellType;
        for (unsigned int jj = 0; jj < OutputQuadraticTriangleCellType::NumberOfPoints; ++jj)
        {
          quadraticTriangleCell->SetPointId(jj, static_cast<OutputPointIdentifier>(buffer[index++]));
        }

        cell.TakeOwnership(quadraticTriangleCell);
        output->SetCell(id++, cell);
        break;
      }
      default:
      {
        itkExceptionMacro("Unknown cell type");
      }
    }
  }
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::ReadPointData()
{
  const typename TOutputMesh::Pointer output = this->GetOutput();

  const auto outputPointDataBuffer =
    make_unique_for_overwrite<OutputPointPixelType[]>(m_MeshIO->GetNumberOfPointPixels());

  if ((m_MeshIO->GetPointPixelComponentType() !=
       MeshIOBase::MapComponentType<typename ConvertPointPixelTraits::ComponentType>::CType) ||
      (m_MeshIO->GetNumberOfPointPixelComponents() != ConvertPointPixelTraits::GetNumberOfComponents()))
  {
    // the point pixel types don't match a type conversion needs to be
    // performed
    itkDebugMacro("Buffer conversion required from: "
                  << MeshIOBase::GetComponentTypeAsString(m_MeshIO->GetPointPixelComponentType()) << " to: "
                  << MeshIOBase::GetComponentTypeAsString(
                       MeshIOBase::MapComponentType<typename ConvertPointPixelTraits::ComponentType>::CType)
                  << "ConvertPointPixelTraits::NumberOfComponents " << ConvertPointPixelTraits::GetNumberOfComponents()
                  << " m_MeshIO->NumberOfComponents " << m_MeshIO->GetNumberOfPointPixelComponents());

    const auto inputPointDataBuffer = make_unique_for_overwrite<char[]>(
      m_MeshIO->GetNumberOfPointPixelComponents() * m_MeshIO->GetComponentSize(m_MeshIO->GetPointPixelComponentType()) *
      m_MeshIO->GetNumberOfPointPixels());
    m_MeshIO->ReadPointData(static_cast<void *>(inputPointDataBuffer.get()));

    this->ConvertPointPixelBuffer(
      static_cast<void *>(inputPointDataBuffer.get()), outputPointDataBuffer.get(), m_MeshIO->GetNumberOfPointPixels());
  }
  else
  {
    itkDebugMacro("No buffer conversion required.");
    m_MeshIO->ReadPointData(static_cast<void *>(outputPointDataBuffer.get()));
  }

  for (OutputPointIdentifier id = 0; id < m_MeshIO->GetNumberOfPointPixels(); ++id)
  {
    output->SetPointData(id, outputPointDataBuffer[id]);
  }
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::ReadCellData()
{
  const typename TOutputMesh::Pointer output = this->GetOutput();

  const auto outputCellDataBuffer = make_unique_for_overwrite<OutputCellPixelType[]>(m_MeshIO->GetNumberOfCellPixels());

  if ((m_MeshIO->GetCellPixelComponentType() !=
       MeshIOBase::MapComponentType<typename ConvertCellPixelTraits::ComponentType>::CType) ||
      (m_MeshIO->GetNumberOfCellPixelComponents() != ConvertCellPixelTraits::GetNumberOfComponents()))
  {
    // the cell pixel types don't match a type conversion needs to be
    // performed
    itkDebugMacro("Buffer conversion required from: "
                  << MeshIOBase::GetComponentTypeAsString(m_MeshIO->GetCellPixelComponentType()) << " to: "
                  << MeshIOBase::GetComponentTypeAsString(
                       MeshIOBase::MapComponentType<typename ConvertCellPixelTraits::ComponentType>::CType)
                  << "ConvertCellPixelTraits::NumberOfComponents " << ConvertCellPixelTraits::GetNumberOfComponents()
                  << " m_MeshIO->NumberOfComponents " << m_MeshIO->GetNumberOfCellPixelComponents());

    const auto inputCellDataBuffer = make_unique_for_overwrite<char[]>(
      m_MeshIO->GetNumberOfCellPixelComponents() * m_MeshIO->GetComponentSize(m_MeshIO->GetCellPixelComponentType()) *
      m_MeshIO->GetNumberOfCellPixels());
    m_MeshIO->ReadCellData(static_cast<void *>(inputCellDataBuffer.get()));

    this->ConvertCellPixelBuffer(
      static_cast<void *>(inputCellDataBuffer.get()), outputCellDataBuffer.get(), m_MeshIO->GetNumberOfCellPixels());
  }
  else
  {
    itkDebugMacro("No buffer conversion required.");
    m_MeshIO->ReadCellData(static_cast<void *>(outputCellDataBuffer.get()));
  }

  for (OutputCellIdentifier id = 0; id < m_MeshIO->GetNumberOfCellPixels(); ++id)
  {
    output->SetCellData(id, outputCellDataBuffer[id]);
  }
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
    msg << " Could not create IO object for file " << m_FileName << std::endl;
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
      msg << "  You probably failed to set a file suffix, or" << std::endl
          << "  set the suffix to an unsupported type." << std::endl;
    }

    MeshFileReaderException e(__FILE__, __LINE__, msg.str().c_str(), ITK_LOCATION);
    throw e;
  }
}

template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::GenerateData()
{
  m_MeshIO->SetPointDimension(OutputPointDimension);

  const typename TOutputMesh::Pointer output = this->GetOutput();

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
        Self::ReadPointsUsingMeshIO<char>();
        break;
      }
      case IOComponentEnum::UCHAR:
      {
        Self::ReadPointsUsingMeshIO<unsigned char>();
        break;
      }
      case IOComponentEnum::SHORT:
      {
        Self::ReadPointsUsingMeshIO<short>();
        break;
      }
      case IOComponentEnum::USHORT:
      {
        Self::ReadPointsUsingMeshIO<unsigned short>();
        break;
      }
      case IOComponentEnum::INT:
      {
        Self::ReadPointsUsingMeshIO<int>();
        break;
      }
      case IOComponentEnum::UINT:
      {
        Self::ReadPointsUsingMeshIO<unsigned int>();
        break;
      }
      case IOComponentEnum::LONG:
      {
        Self::ReadPointsUsingMeshIO<long>();
        break;
      }
      case IOComponentEnum::ULONG:
      {
        Self::ReadPointsUsingMeshIO<unsigned long>();
        break;
      }
      case IOComponentEnum::LONGLONG:
      {
        Self::ReadPointsUsingMeshIO<long long>();
        break;
      }
      case IOComponentEnum::ULONGLONG:
      {
        Self::ReadPointsUsingMeshIO<unsigned long long>();
        break;
      }
      case IOComponentEnum::FLOAT:
      {
        Self::ReadPointsUsingMeshIO<float>();
        break;
      }
      case IOComponentEnum::DOUBLE:
      {
        Self::ReadPointsUsingMeshIO<double>();
        break;
      }
      case IOComponentEnum::LDOUBLE:
      {
        Self::ReadPointsUsingMeshIO<long double>();
        break;
      }
      case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      default:
      {
        itkExceptionMacro("Unknown point component type");
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
        Self::ReadCellsUsingMeshIO<char>();
        break;
      }
      case IOComponentEnum::UCHAR:
      {
        Self::ReadCellsUsingMeshIO<unsigned char>();
        break;
      }
      case IOComponentEnum::SHORT:
      {
        Self::ReadCellsUsingMeshIO<short>();
        break;
      }
      case IOComponentEnum::USHORT:
      {
        Self::ReadCellsUsingMeshIO<unsigned short>();
        break;
      }
      case IOComponentEnum::INT:
      {
        Self::ReadCellsUsingMeshIO<int>();
        break;
      }
      case IOComponentEnum::UINT:
      {
        Self::ReadCellsUsingMeshIO<unsigned int>();
        break;
      }
      case IOComponentEnum::LONG:
      {
        Self::ReadCellsUsingMeshIO<long>();
        break;
      }
      case IOComponentEnum::ULONG:
      {
        Self::ReadCellsUsingMeshIO<unsigned long>();
        break;
      }
      case IOComponentEnum::LONGLONG:
      {
        Self::ReadCellsUsingMeshIO<long long>();
        break;
      }
      case IOComponentEnum::ULONGLONG:
      {
        Self::ReadCellsUsingMeshIO<unsigned long long>();
        break;
      }
      case IOComponentEnum::FLOAT:
      {
        Self::ReadCellsUsingMeshIO<float>();
        break;
      }
      case IOComponentEnum::DOUBLE:
      {
        Self::ReadCellsUsingMeshIO<double>();
        break;
      }
      case IOComponentEnum::LDOUBLE:
      {
        Self::ReadCellsUsingMeshIO<long double>();
        break;
      }
      case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      default:
      {
        itkExceptionMacro("Unknown cell component type");
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

#define ITK_CONVERT_POINT_PIXEL_BUFFER_IF_BLOCK(CType, type)                                                    \
  else if (m_MeshIO->GetPointPixelComponentType() == CType)                                                     \
  {                                                                                                             \
    ConvertPixelBuffer<type, OutputPointPixelType, ConvertPointPixelTraits>::Convert(                           \
      static_cast<type *>(inputData), m_MeshIO->GetNumberOfPointPixelComponents(), outputData, numberOfPixels); \
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
        << "    " << MeshIOBase::GetComponentTypeAsString(m_MeshIO->GetPointPixelComponentType()) << std::endl
        << "to one of: " << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned char>::CType)
        << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<char>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned short>::CType)
        << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<short>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned int>::CType)
        << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<int>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned long>::CType)
        << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<long>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned long long>::CType)
        << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<long long>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<float>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<double>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<long double>::CType)
        << std::endl;
    e.SetDescription(msg.str().c_str());
    e.SetLocation(ITK_LOCATION);
    throw e;
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

#define ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK(CType, type)                                                    \
  else if (m_MeshIO->GetCellPixelComponentType() == CType)                                                     \
  {                                                                                                            \
    ConvertPixelBuffer<type, OutputCellPixelType, ConvertCellPixelTraits>::Convert(                            \
      static_cast<type *>(inputData), m_MeshIO->GetNumberOfCellPixelComponents(), outputData, numberOfPixels); \
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
        << "    " << MeshIOBase::GetComponentTypeAsString(m_MeshIO->GetCellPixelComponentType()) << std::endl
        << "to one of: " << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned char>::CType)
        << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<char>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned short>::CType)
        << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<short>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned int>::CType)
        << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<int>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned long>::CType)
        << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<long>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<unsigned long long>::CType)
        << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<long long>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<float>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<double>::CType) << std::endl
        << "    " << MeshIOBase::GetComponentTypeAsString(MeshIOBase::MapComponentType<long double>::CType)
        << std::endl;
    e.SetDescription(msg.str().c_str());
    e.SetLocation(ITK_LOCATION);
    throw e;
  }
#undef ITK_CONVERT_CELL_PIXEL_BUFFER_IF_BLOCK
}


template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
template <typename T>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::ReadPointsUsingMeshIO()
{
  const auto buffer = make_unique_for_overwrite<T[]>(m_MeshIO->GetNumberOfPoints() * OutputPointDimension);
  m_MeshIO->ReadPoints(buffer.get());
  Self::ReadPoints(buffer.get());
}


template <typename TOutputMesh, typename ConvertPointPixelTraits, typename ConvertCellPixelTraits>
template <typename T>
void
MeshFileReader<TOutputMesh, ConvertPointPixelTraits, ConvertCellPixelTraits>::ReadCellsUsingMeshIO()
{
  const auto buffer = make_unique_for_overwrite<T[]>(m_MeshIO->GetCellBufferSize());
  m_MeshIO->ReadCells(buffer.get());
  Self::ReadCells(buffer.get());
}


} // namespace itk
#endif
