/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkSTLMeshIO.h"
#include "itkMetaDataObject.h"

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
  if (this->GetFileType() == ASCII)
  {
    this->m_InputStream.open(this->m_FileName.c_str(), std::ios::in);
  }
  else if (this->GetFileType() == BINARY)
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


  std::string inputLine;

  // Read STL header file(the first line)
  std::getline(this->m_InputStream, inputLine, '\n');

  // Determine file type
  if (inputLine.find("solid") != std::string::npos)
  {
    if (this->GetFileType() != ASCII)
    {
      this->SetFileType(ASCII);
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
    if (this->GetFileType() != BINARY)
    {
      this->SetFileType(BINARY);
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
  std::string inputLine;
  PointType   p0;
  PointType   p1;
  PointType   p2;

  this->m_InputLineNumber = 2;

  while (!this->CheckStringFromAscii("endsolid"))
  {
    //
    // http://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
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

    // this->m_CellsVector.push_back( this->m_TrianglePointIds );
  }
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
  // http://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
  //
  // UINT8[80] header
  //
  char header[80];
  this->m_InputStream.read(header, 80);

  //
  // UINT32 -- Number of Triangles
  //
  int32_t numberOfTriangles;
  this->m_InputStream.read(reinterpret_cast<char *>(&numberOfTriangles), sizeof(numberOfTriangles));

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

    // this->m_CellsVector.push_back( this->m_TrianglePointIds );
  }
}


void
STLMeshIO ::ReadPoints(void * itkNotUsed(buffer))
{
  // Read input file

  if (this->GetFileType() == ASCII)
  {
    this->m_InputStream.open(this->m_FileName.c_str(), std::ios::in);
  }
  else if (this->GetFileType() == BINARY)
  {
    this->m_InputStream.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
  }

  // Test whether the file has been opened
  if (!this->m_InputStream.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "inputFilename= "
                      << this->m_FileName);
    return;
  }

  this->m_InputStream.close();
}

void
STLMeshIO ::ReadCells(void * itkNotUsed(buffer))
{
  // Read input file
  std::ifstream inputFile;

  if (this->GetFileType() == ASCII)
  {
    this->m_InputStream.open(this->m_FileName.c_str(), std::ios::in);
  }
  else if (this->GetFileType() == BINARY)
  {
    this->m_InputStream.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
  }

  // Test whether the file has been opened
  if (!this->m_InputStream.is_open())
  {
    itkExceptionMacro(<< "Unable to open file\n"
                         "inputFilename= "
                      << this->m_FileName);
    return;
  }

  this->m_InputStream.close();
}


void
STLMeshIO ::WriteMeshInformation()
{

  // Use default filetype
  if (this->GetFileType() == ASCII)
  {
    this->m_OutputStream.open(this->m_FileName.c_str(), std::ios::out);
  }
  else if (this->GetFileType() == BINARY)
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

  if (this->GetFileType() == ASCII)
  {
    this->m_OutputStream << "solid ascii" << std::endl;
  }
  else if (this->GetFileType() == BINARY)
  {
    //
    // http://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
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

  const IOComponentType pointValueType = this->GetPointComponentType();

  switch (pointValueType)
  {
    case UCHAR:
      this->WritePointsTyped(reinterpret_cast<unsigned char *>(buffer));
      break;
    case CHAR:
      this->WritePointsTyped(reinterpret_cast<const char *>(buffer));
      break;
    case USHORT:
      this->WritePointsTyped(reinterpret_cast<const unsigned short *>(buffer));
      break;
    case SHORT:
      this->WritePointsTyped(reinterpret_cast<const short *>(buffer));
      break;
    case UINT:
      this->WritePointsTyped(reinterpret_cast<const unsigned int *>(buffer));
      break;
    case INT:
      this->WritePointsTyped(reinterpret_cast<const int *>(buffer));
      break;
    case ULONG:
      this->WritePointsTyped(reinterpret_cast<const unsigned long *>(buffer));
      break;
    case LONG:
      this->WritePointsTyped(reinterpret_cast<const long *>(buffer));
      break;
    case ULONGLONG:
      this->WritePointsTyped(reinterpret_cast<const unsigned long long *>(buffer));
      break;
    case LONGLONG:
      this->WritePointsTyped(reinterpret_cast<const long long *>(buffer));
      break;
    case FLOAT:
      this->WritePointsTyped(reinterpret_cast<const float *>(buffer));
      break;
    case DOUBLE:
      this->WritePointsTyped(reinterpret_cast<const double *>(buffer));
      break;
    case LDOUBLE:
      this->WritePointsTyped(reinterpret_cast<const long double *>(buffer));
      break;
    default:
      itkExceptionMacro(<< "Unknonwn point component type");
  }
}


void
STLMeshIO ::WriteCells(void * buffer)
{

  const IdentifierType numberOfPolygons = this->GetNumberOfCells();

  const IdentifierType * cellsBuffer = reinterpret_cast<const IdentifierType *>(buffer);

  SizeValueType index = 0;

  typedef MeshIOBase::CellGeometryType CellGeometryType;

  NormalType normal;

  if (this->GetFileType() == BINARY)
  {
    //
    // http://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
    //
    // UINT32 -- Number of Triangles
    //
    int32_t numberOfTriangles = 0;

    SizeValueType index2 = 0;

    for (SizeValueType polygonItr = 0; polygonItr < numberOfPolygons; polygonItr++)
    {
      const MeshIOBase::CellGeometryType cellType = static_cast<CellGeometryType>(cellsBuffer[index2++]);
      const IdentifierType               numberOfVerticesInCell = static_cast<IdentifierType>(cellsBuffer[index2++]);

      const bool isTriangle = (cellType == TRIANGLE_CELL) || (cellType == POLYGON_CELL && numberOfVerticesInCell == 3);

      if (isTriangle)
      {
        numberOfTriangles++;
      }

      index2 += numberOfVerticesInCell;
    }

    this->WriteInt32AsBinary(numberOfTriangles);
  }

  for (SizeValueType polygonItr = 0; polygonItr < numberOfPolygons; polygonItr++)
  {
    const MeshIOBase::CellGeometryType cellType = static_cast<CellGeometryType>(cellsBuffer[index++]);
    const IdentifierType               numberOfVerticesInCell = static_cast<IdentifierType>(cellsBuffer[index++]);

    const bool isTriangle = (cellType == TRIANGLE_CELL) || (cellType == POLYGON_CELL && numberOfVerticesInCell == 3);

    if (isTriangle)
    {
      const PointType & p0 = m_Points[cellsBuffer[index++]];
      const PointType & p1 = m_Points[cellsBuffer[index++]];
      const PointType & p2 = m_Points[cellsBuffer[index++]];

      const VectorType v10(p0 - p1);
      const VectorType v12(p2 - p1);

      CrossProduct(normal, v12, v10);

      if (this->GetFileType() == ASCII)
      {
        this->m_OutputStream << "  facet normal ";
        this->m_OutputStream << normal[0] << " " << normal[1] << " " << normal[2] << std::endl;

        this->m_OutputStream << "    outer loop" << std::endl;

        this->m_OutputStream << "      vertex " << p0[0] << " " << p0[1] << " " << p0[2] << std::endl;
        this->m_OutputStream << "      vertex " << p1[0] << " " << p1[1] << " " << p1[2] << std::endl;
        this->m_OutputStream << "      vertex " << p2[0] << " " << p2[1] << " " << p2[2] << std::endl;

        this->m_OutputStream << "    endloop" << std::endl;
        this->m_OutputStream << "  endfacet" << std::endl;
      }
      else if (this->GetFileType() == BINARY)
      {
        //
        // http://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
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
    }
    else
    {
      index += numberOfVerticesInCell;
    }
  }

  if (this->GetFileType() == ASCII)
  {
    this->m_OutputStream << "endsolid" << std::endl;
  }
  //
  // There is no ending section when doing BINARY
  //
}


void
STLMeshIO ::WriteInt32AsBinary(int32_t value)
{
  this->m_OutputStream.write(reinterpret_cast<const char *>(&value), sizeof(value));
}


void
STLMeshIO ::WriteInt16AsBinary(int16_t value)
{
  this->m_OutputStream.write(reinterpret_cast<const char *>(&value), sizeof(value));
}


void
STLMeshIO ::WriteNormalAsBinary(const NormalType & normal)
{
  for (unsigned int i = 0; i < 3; ++i)
  {
    const float value = normal[i];
    this->m_OutputStream.write(reinterpret_cast<const char *>(&value), sizeof(value));
  }
}


void
STLMeshIO ::WritePointAsBinary(const PointType & point)
{
  for (unsigned int i = 0; i < 3; ++i)
  {
    const float value = point[i];
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
    normal[i] = value;
  }
}


void
STLMeshIO ::ReadInt32AsBinary(int32_t & value)
{
  this->m_InputStream.read(reinterpret_cast<char *>(&value), sizeof(value));
}


void
STLMeshIO ::ReadInt16AsBinary(int16_t & value)
{
  this->m_InputStream.read(reinterpret_cast<char *>(&value), sizeof(value));
}


void
STLMeshIO ::ReadPointAsBinary(PointType & point)
{
  float value;
  for (unsigned int i = 0; i < 3; ++i)
  {
    this->m_InputStream.read(reinterpret_cast<char *>(&value), sizeof(value));
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
  //
  // combines the FLOAT32 bits components of points into a hash
  //
  const unsigned int size = sizeof(float);

  char pointHash[3 * size];

  float valueX = point[0];
  float valueY = point[1];
  float valueZ = point[2];

  char * destination = pointHash;

  strncpy(destination, reinterpret_cast<char *>(&valueX), size);
  destination += size;

  strncpy(destination, reinterpret_cast<char *>(&valueY), size);
  destination += size;

  strncpy(destination, reinterpret_cast<char *>(&valueZ), size);

  PointSetResultType result = this->m_PointsSet.insert(point);

  if (result.second)
  {
    this->m_PointsVector.push_back(result.first);
    this->m_LatestPointId = this->m_PointsVector.size();
    this->m_TrianglePointIds[this->m_PointInTriangleCounter++] = this->m_LatestPointId;
  }
}


void
STLMeshIO ::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end of namespace itk
