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

#include "itkSTLMeshIO.h"
#include "itkMetaDataObject.h"
#include "itkByteSwapper.h"

#include <itksys/SystemTools.hxx>
#include <fstream>
#include <iomanip>

namespace itk
{
// Constructor
STLMeshIO ::STLMeshIO()
{
  this->AddSupportedWriteExtension(".stl");
  this->AddSupportedWriteExtension(".STL");

  // STL uses float type by default to store point data
  this->SetPointComponentType(IOComponentEnum::FLOAT);

  // STL uses UINT32 to store the number of points,
  // hence the point Ids are of the same UINT32 type.
  this->SetCellComponentType(IOComponentEnum::UINT);

  this->SetPointDimension(3);
}

bool
STLMeshIO ::CanReadFile(const char * fileName)
{
  if (!itksys::SystemTools::FileExists(fileName, true))
  {
    return false;
  }

  const std::string extension = itksys::SystemTools::GetFilenameLastExtension(fileName);

  if (extension != ".stl" && extension != ".STL")
  {
    return false;
  }

  return true;
}

bool
STLMeshIO ::CanWriteFile(const char * fileName)
{
  const std::string extension = itksys::SystemTools::GetFilenameLastExtension(fileName);

  if (extension != ".stl" && extension != ".STL")
  {
    return false;
  }

  return true;
}

void
STLMeshIO ::Read()
{}

void
STLMeshIO ::ReadMeshInformation()
{

  // Use default filetype
  if (this->GetFileType() == IOFileEnum::ASCII)
  {
    this->m_InputStream.open(this->m_FileName.c_str(), std::ios::in);
  }
  else if (this->GetFileType() == IOFileEnum::BINARY)
  {
    this->m_InputStream.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
  }

  if (!this->m_InputStream.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "inputFilename= "
                      << this->m_FileName);
    return;
  }


  // Read STL header file(the first line)
  char headerBuffer[6];
  this->m_InputStream.read(headerBuffer, 6);
  this->m_InputStream.seekg(0); // Reset to the beginning of the file.

  headerBuffer[5] = '\0';
  std::string header(headerBuffer);

  bool inputFileIsASCII = false;

  // Based on the header line define whether the file is ASCII or BINARY
  if (header.find("solid") != std::string::npos)
  {
    inputFileIsASCII = true;
  }

  // Determine file type
  if (inputFileIsASCII)
  {
    if (this->GetFileType() != IOFileEnum::ASCII)
    {
      this->SetFileType(IOFileEnum::ASCII);
#ifdef _WIN32
      this->m_InputStream.close();
      this->m_InputStream.open(this->m_FileName.c_str(), std::ios::in);
      if (!this->m_InputStream.is_open())
      {
        itkExceptionMacro("Unable to open file\n"
                          "inputFilename= "
                          << this->m_FileName);
        return;
      }
#endif
    }

    this->ReadMeshInternalFromAscii();
  }
  else
  {
    if (this->GetFileType() != IOFileEnum::BINARY)
    {
      this->SetFileType(IOFileEnum::BINARY);
#ifdef _WIN32
      this->m_InputStream.close();
      this->m_InputStream.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
      if (!this->m_InputStream.is_open())
      {
        itkExceptionMacro("Unable to open file\n"
                          "inputFilename= "
                          << this->m_FileName);
        return;
      }
#endif
    }

    this->ReadMeshInternalFromBinary();
  }

  this->m_InputStream.close();
}


void
STLMeshIO ::ReadMeshInternalFromAscii()
{
  // Read all the points, and reduce them to unique ones
  PointType p0;
  PointType p1;
  PointType p2;

  this->m_InputLineNumber = 2;

  this->m_LatestPointId = NumericTraits<IdentifierType>::Zero;

  // read header line
  std::getline(this->m_InputStream, this->m_InputLine, '\n');

  while (!this->CheckStringFromAscii("endsolid"))
  {
    //
    // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
    //
    //  facet normal ni nj nk
    //      outer loop
    //          vertex v1x v1y v1z
    //          vertex v2x v2y v2z
    //          vertex v3x v3y v3z
    //      endloop
    //  endfacet
    //
    this->m_PointInTriangleCounter = 0;

    this->ReadStringFromAscii("facet normal");
    this->ReadStringFromAscii("outer loop");
    this->ReadPointAsAscii(p0);
    this->ReadPointAsAscii(p1);
    this->ReadPointAsAscii(p2);
    this->ReadStringFromAscii("endloop");
    this->ReadStringFromAscii("endfacet");

    this->m_CellsVector.push_back(this->m_TrianglePointIds);
  }

  this->SetNumberOfPoints(this->m_PointsMap.size());
  this->SetNumberOfCells(this->m_CellsVector.size());

  //
  // The factor 5 accounts for five integers
  //  1. cell type id
  //  2. number of points in cell
  //  3. point id of point 0
  //  4. point id of point 1
  //  5. point id of point 2
  //
  this->SetCellBufferSize(5 * this->m_CellsVector.size());
}


void
STLMeshIO ::ReadStringFromAscii(const std::string & expected)
{
  if (this->m_InputLine.empty())
  {
    std::getline(this->m_InputStream, this->m_InputLine, '\n');
  }

  if (this->m_InputLine.find(expected) == std::string::npos)
  {
    itkExceptionMacro("Parsing error: missed " << expected << " in line " << this->m_InputLineNumber
                                               << " found: " << this->m_InputLine);
  }

  this->m_InputLine.clear();

  this->m_InputLineNumber++;
}


bool
STLMeshIO ::CheckStringFromAscii(const std::string & expected)
{
  std::getline(this->m_InputStream, this->m_InputLine, '\n');

  if (this->m_InputLine.find(expected) != std::string::npos)
  {
    this->m_InputLineNumber++;
    return true;
  }

  return false;
}


void
STLMeshIO ::ReadMeshInternalFromBinary()
{
  //
  // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
  //
  // UINT8[80] header
  //
  char header[80];
  this->m_InputStream.read(header, 80);

  header[79] = '\0'; // insert string terminator


  this->m_LatestPointId = NumericTraits<IdentifierType>::Zero;

  //
  // UINT32 -- Number of Triangles
  //
  int32_t numberOfTriangles;
  this->m_InputStream.read(reinterpret_cast<char *>(&numberOfTriangles), sizeof(numberOfTriangles));

  //
  // Binary values in STL files are expected to be in little endian
  // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
  //
  ByteSwapper<int32_t>::SwapFromSystemToLittleEndian(&numberOfTriangles);

  this->SetNumberOfCells(numberOfTriangles);

  //
  // foreach triangle
  //
  NormalType normal;
  PointType  p0;
  PointType  p1;
  PointType  p2;
  int16_t    bytecount;

  while (numberOfTriangles--)
  {
    //
    //    REAL32[3] – Normal vector
    //    REAL32[3] – Vertex 1
    //    REAL32[3] – Vertex 2
    //    REAL32[3] – Vertex 3
    //    UINT16 – Attribute byte count
    //
    this->m_PointInTriangleCounter = 0;

    this->ReadNormalAsBinary(normal);
    this->ReadPointAsBinary(p0);
    this->ReadPointAsBinary(p1);
    this->ReadPointAsBinary(p2);
    this->ReadInt16AsBinary(bytecount);

    this->m_CellsVector.push_back(this->m_TrianglePointIds);
  }

  this->SetNumberOfPoints(this->m_PointsMap.size());

  //
  // The factor 5 accounts for five integers
  //  1. cell type id
  //  2. number of points in cell
  //  3. point id of point 0
  //  4. point id of point 1
  //  5. point id of point 2
  //
  this->SetCellBufferSize(5 * this->m_CellsVector.size());
}


void
STLMeshIO ::ReadPoints(void * buffer)
{
  //
  // The Point and Cell data were read in the ReadMeshInformation() method.
  // Here, we can focus on packaging the point data into the return buffer.
  //
  PointsMapType::const_iterator pointItr = this->m_PointsMap.begin();
  PointsMapType::const_iterator pointEnd = this->m_PointsMap.end();

  auto * pointsBuffer = reinterpret_cast<float *>(buffer);

  while (pointItr != pointEnd)
  {
    // Get the reference to that PointType object.
    const PointType & point = pointItr->first;

    // Get the location in the buffer where the point should be stored
    float * pointCoordinates = pointsBuffer + 3 * pointItr->second;

    //
    // Store the Point coordinates in the buffer.
    //
    *pointCoordinates++ = point[0];
    *pointCoordinates++ = point[1];
    *pointCoordinates++ = point[2];

    ++pointItr;
  }
}


void
STLMeshIO ::ReadCells(void * buffer)
{
  //
  // The Point and Cell data were read in the ReadMeshInformation() method.
  // Here, we can focus on packaging the cell data into the return buffer.
  //
  CellsVectorType::const_iterator cellItr = this->m_CellsVector.begin();
  CellsVectorType::const_iterator cellEnd = this->m_CellsVector.end();

  using CellIDType = unsigned int;
  auto * cellPointIds = reinterpret_cast<CellIDType *>(buffer);

  constexpr unsigned int numberOfPointsInCell = 3;

  while (cellItr != cellEnd)
  {

    *cellPointIds++ = static_cast<CellIDType>(CellGeometryEnum::TRIANGLE_CELL);
    *cellPointIds++ = numberOfPointsInCell;

    //
    // Store the Point Ids for this cell, in the buffer.
    //
    *cellPointIds++ = cellItr->p0;
    *cellPointIds++ = cellItr->p1;
    *cellPointIds++ = cellItr->p2;

    ++cellItr;
  }
}


void
STLMeshIO ::WriteMeshInformation()
{

  // Use default filetype
  if (this->GetFileType() == IOFileEnum::ASCII)
  {
    this->m_OutputStream.open(this->m_FileName.c_str(), std::ios::out);
  }
  else if (this->GetFileType() == IOFileEnum::BINARY)
  {
    this->m_OutputStream.open(this->m_FileName.c_str(), std::ios::out | std::ios::binary);
  }

  if (!this->m_OutputStream.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "inputFilename= "
                      << this->m_FileName);
    return;
  }

  if (this->GetFileType() == IOFileEnum::ASCII)
  {
    this->m_OutputStream << "solid ascii" << std::endl;
  }
  else if (this->GetFileType() == IOFileEnum::BINARY)
  {
    //
    // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
    //
    // UINT8[80] header
    //
    this->m_OutputStream << std::setfill(' ') << std::setw(80) << "binary STL generated from ITK";
  }
}


void
STLMeshIO ::Write()
{
  // All has been done in the WriteCells() method.

  // Here we only need to close the output stream.
  m_OutputStream.close();
}

void
STLMeshIO ::WritePoints(void * buffer)
{

  const IOComponentEnum pointValueType = this->GetPointComponentType();

  switch (pointValueType)
  {
    case IOComponentEnum::UCHAR:
    {
      using CoordType = unsigned char;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    case IOComponentEnum::CHAR:
    {
      using CoordType = char;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    case IOComponentEnum::USHORT:
    {
      using CoordType = unsigned short;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    case IOComponentEnum::SHORT:
    {
      using CoordType = short;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    case IOComponentEnum::UINT:
    {
      using CoordType = unsigned int;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    case IOComponentEnum::INT:
    {
      using CoordType = int;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    case IOComponentEnum::ULONG:
    {
      using CoordType = unsigned long;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    case IOComponentEnum::LONG:
    {
      using CoordType = long;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    case IOComponentEnum::ULONGLONG:
    {
      using CoordType = unsigned long long;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    case IOComponentEnum::LONGLONG:
    {
      using CoordType = long long;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    case IOComponentEnum::FLOAT:
    {
      using CoordType = float;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    case IOComponentEnum::DOUBLE:
    {
      using CoordType = double;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    case IOComponentEnum::LDOUBLE:
    {
      using CoordType = long double;
      this->WritePointsTyped<CoordType>(reinterpret_cast<CoordType *>(buffer));
      break;
    }
    default:
      itkExceptionMacro(<< "Unknonwn point component type");
  }
}


void
STLMeshIO ::WriteCells(void * buffer)
{
  if (this->GetFileType() == IOFileEnum::BINARY)
  {
    this->WriteCellsAsBinary(buffer);
  }
  else
  {
    this->WriteCellsAsAscii(buffer);
  }
}

void
STLMeshIO ::WriteCellsAsBinary(void * buffer)
{

  const IdentifierType numberOfPolygons = this->GetNumberOfCells();

  const auto * cellsBuffer = reinterpret_cast<const IdentifierType *>(buffer);

  SizeValueType index = 0;

  NormalType normal;

  //
  // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
  //
  // UINT32 -- Number of Triangles
  //
  int32_t numberOfTriangles = 0;

  SizeValueType index2 = 0;

  for (SizeValueType polygonItr = 0; polygonItr < numberOfPolygons; polygonItr++)
  {
    const auto cellType = static_cast<CellGeometryEnum>(cellsBuffer[index2++]);
    const auto numberOfVerticesInCell = static_cast<IdentifierType>(cellsBuffer[index2++]);

    const bool isTriangle = (cellType == CellGeometryEnum::TRIANGLE_CELL) ||
                            (cellType == CellGeometryEnum::POLYGON_CELL && numberOfVerticesInCell == 3);

    if (isTriangle)
    {
      numberOfTriangles++;
    }
    else
    {
      itkExceptionMacro("Found Non-Triangular Cell.");
    }

    index2 += numberOfVerticesInCell;
  }

  this->WriteInt32AsBinary(numberOfTriangles);

  for (SizeValueType polygonItr = 0; polygonItr < numberOfPolygons; polygonItr++)
  {
    // skip cell type and number of vertices in cell
    index += 2;

    const PointType & p0 = m_Points[cellsBuffer[index++]];
    const PointType & p1 = m_Points[cellsBuffer[index++]];
    const PointType & p2 = m_Points[cellsBuffer[index++]];

    const VectorType v10(p0 - p1);
    const VectorType v12(p2 - p1);

    CrossProduct(normal, v12, v10);

    //
    // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
    //
    //    foreach triangle
    //    REAL32[3] – Normal vector
    //    REAL32[3] – Vertex 1
    //    REAL32[3] – Vertex 2
    //    REAL32[3] – Vertex 3
    //    UINT16 – Attribute byte count
    //
    this->WriteNormalAsBinary(normal);
    this->WritePointAsBinary(p0);
    this->WritePointAsBinary(p1);
    this->WritePointAsBinary(p2);
    this->WriteInt16AsBinary(0);
  }

  //
  // There is no ending section when doing BINARY
  //
}

void
STLMeshIO ::WriteCellsAsAscii(void * buffer)
{

  const IdentifierType numberOfPolygons = this->GetNumberOfCells();

  const auto * cellsBuffer = reinterpret_cast<const IdentifierType *>(buffer);

  SizeValueType index = 0;

  NormalType normal;

  for (SizeValueType polygonItr = 0; polygonItr < numberOfPolygons; polygonItr++)
  {
    const auto cellType = static_cast<CellGeometryEnum>(cellsBuffer[index++]);
    const auto numberOfVerticesInCell = static_cast<IdentifierType>(cellsBuffer[index++]);

    const bool isTriangle = (cellType == CellGeometryEnum::TRIANGLE_CELL) ||
                            (cellType == CellGeometryEnum::POLYGON_CELL && numberOfVerticesInCell == 3);

    if (!isTriangle)
    {
      itkExceptionMacro("Found Non-Triangular Cell.");
    }

    const PointType & p0 = m_Points[cellsBuffer[index++]];
    const PointType & p1 = m_Points[cellsBuffer[index++]];
    const PointType & p2 = m_Points[cellsBuffer[index++]];

    const VectorType v10(p0 - p1);
    const VectorType v12(p2 - p1);

    CrossProduct(normal, v12, v10);

    this->m_OutputStream << "  facet normal ";
    this->m_OutputStream << normal[0] << " " << normal[1] << " " << normal[2] << std::endl;

    this->m_OutputStream << "    outer loop" << std::endl;

    this->m_OutputStream << "      vertex " << p0[0] << " " << p0[1] << " " << p0[2] << std::endl;
    this->m_OutputStream << "      vertex " << p1[0] << " " << p1[1] << " " << p1[2] << std::endl;
    this->m_OutputStream << "      vertex " << p2[0] << " " << p2[1] << " " << p2[2] << std::endl;

    this->m_OutputStream << "    endloop" << std::endl;
    this->m_OutputStream << "  endfacet" << std::endl;
  }

  this->m_OutputStream << "endsolid" << std::endl;
}

void
STLMeshIO ::WriteInt32AsBinary(int32_t value)
{
  //
  // Binary values in STL files are expected to be in little endian
  // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
  //
  ByteSwapper<int32_t>::SwapFromSystemToLittleEndian(&value);
  this->m_OutputStream.write(reinterpret_cast<const char *>(&value), sizeof(value));
}


void
STLMeshIO ::WriteInt16AsBinary(int16_t value)
{
  //
  // Binary values in STL files are expected to be in little endian
  // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
  //
  ByteSwapper<int16_t>::SwapFromSystemToLittleEndian(&value);
  this->m_OutputStream.write(reinterpret_cast<const char *>(&value), sizeof(value));
}


void
STLMeshIO ::WriteNormalAsBinary(const NormalType & normal)
{
  for (unsigned int i = 0; i < 3; ++i)
  {
    float value = normal[i];
    //
    // Binary values in STL files are expected to be in little endian
    // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
    //
    ByteSwapper<float>::SwapFromSystemToLittleEndian(&value);
    this->m_OutputStream.write(reinterpret_cast<const char *>(&value), sizeof(value));
  }
}


void
STLMeshIO ::WritePointAsBinary(const PointType & point)
{
  for (unsigned int i = 0; i < 3; ++i)
  {
    float value = point[i];
    //
    // Binary values in STL files are expected to be in little endian
    // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
    //
    ByteSwapper<float>::SwapFromSystemToLittleEndian(&value);
    this->m_OutputStream.write(reinterpret_cast<const char *>(&value), sizeof(value));
  }
}


void
STLMeshIO ::ReadNormalAsBinary(NormalType & normal)
{
  float value;
  for (unsigned int i = 0; i < 3; ++i)
  {
    this->m_InputStream.read(reinterpret_cast<char *>(&value), sizeof(value));
    //
    // Binary values in STL files are expected to be in little endian
    // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
    //
    ByteSwapper<float>::SwapFromSystemToLittleEndian(&value);
    normal[i] = value;
  }
}


void
STLMeshIO ::ReadInt32AsBinary(int32_t & value)
{
  this->m_InputStream.read(reinterpret_cast<char *>(&value), sizeof(value));
  //
  // Binary values in STL files are expected to be in little endian
  // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
  //
  ByteSwapper<int32_t>::SwapFromSystemToLittleEndian(&value);
}


void
STLMeshIO ::ReadInt16AsBinary(int16_t & value)
{
  this->m_InputStream.read(reinterpret_cast<char *>(&value), sizeof(value));
  //
  // Binary values in STL files are expected to be in little endian
  // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
  //
  ByteSwapper<int16_t>::SwapFromSystemToLittleEndian(&value);
}


void
STLMeshIO ::ReadPointAsBinary(PointType & point)
{
  float value;
  for (unsigned int i = 0; i < 3; ++i)
  {
    this->m_InputStream.read(reinterpret_cast<char *>(&value), sizeof(value));
    //
    // Binary values in STL files are expected to be in little endian
    // https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
    //
    ByteSwapper<float>::SwapFromSystemToLittleEndian(&value);
    point[i] = value;
  }

  this->InsertPointIntoSet(point);
}


void
STLMeshIO ::ReadPointAsAscii(PointType & point)
{
  std::string keyword;
  this->m_InputStream >> keyword;

  if (keyword.find("vertex") == std::string::npos)
  {
    itkExceptionMacro("Parsing error: missed 'vertex' in line " << this->m_InputLineNumber);
  }

  this->m_InputStream >> point;

  this->InsertPointIntoSet(point);

  // read remaining of the line, most often just the end of line.
  std::string restOfLine;
  std::getline(this->m_InputStream, restOfLine, '\n');

  this->m_InputLineNumber++;
}


void
STLMeshIO ::InsertPointIntoSet(const PointType & point)
{

  PointsMapType::const_iterator pointMapItr = this->m_PointsMap.find(point);

  IdentifierType pointId;

  if (pointMapItr == this->m_PointsMap.end())
  {
    this->m_PointsMap[point] = this->m_LatestPointId;
    pointId = this->m_LatestPointId;
    this->m_LatestPointId++;
  }
  else
  {
    pointId = pointMapItr->second;
  }

  switch (this->m_PointInTriangleCounter)
  {
    case 0:
      this->m_TrianglePointIds.p0 = pointId;
      break;
    case 1:
      this->m_TrianglePointIds.p1 = pointId;
      break;
    case 2:
      this->m_TrianglePointIds.p2 = pointId;
      break;
    default:
      itkExceptionMacro("Point counter went beyond value 2");
  }

  this->m_PointInTriangleCounter++;

  if (this->m_PointInTriangleCounter == 3)
  {
    this->m_PointInTriangleCounter = 0;
  }
}


bool
STLMeshIO ::GetUpdatePoints() const
{
  // Always true, since we are reading the point information
  // in ReadMeshInformation(), and we need ReadPoints() to be
  // called in order to store the point data into the buffer.
  return true;
}


bool
STLMeshIO ::GetUpdateCells() const
{
  // Always true, since we are reading the cell information
  // in ReadMeshInformation(), and we need ReadCells() to be
  // called in order to store the cell data into the buffer.
  return true;
}


void
STLMeshIO ::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end of namespace itk
