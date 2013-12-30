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

namespace itk
{
// Constructor
STLMeshIO ::STLMeshIO() { this->AddSupportedWriteExtension(".stl"); }

bool
STLMeshIO ::CanReadFile(const char * fileName)
{
  if (!itksys::SystemTools::FileExists(fileName, true))
  {
    return false;
  }

  if (itksys::SystemTools::GetFilenameLastExtension(fileName) != ".stl")
  {
    return false;
  }

  return true;
}

bool
STLMeshIO ::CanWriteFile(const char * fileName)
{
  if (itksys::SystemTools::GetFilenameLastExtension(fileName) != ".stl")
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
  else if (GetFileType() == BINARY)
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
  }
  else if (inputLine.find("BINARY") != std::string::npos)
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
  }


  this->m_InputStream.close();
}

void
STLMeshIO ::ReadPoints(void * itkNotUsed(buffer))
{
  // Read input file

  if (this->GetFileType() == ASCII)
  {
    this->m_InputStream.open(this->m_FileName.c_str(), std::ios::in);
  }
  else if (GetFileType() == BINARY)
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
  else if (GetFileType() == BINARY)
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
}


void
STLMeshIO ::Write()
{}

void
STLMeshIO ::WritePoints(void * buffer)
{
  const IdentifierType numberOfPoints = this->GetNumberOfPoints();

  // Revisit this choice. Although.. STL will only manage 3D.
  // This probably should throw and exception if the input Mesh is not in 3D space.
  const unsigned int PointDimension = 3;

  typedef float PointValueType; // FIXME Revisit this choice. It should be read from the MetaData.

  const PointValueType * pointCoordinates = reinterpret_cast<const PointValueType *>(buffer);

  this->m_Points.clear();

  this->m_Points.resize(numberOfPoints);

  for (IdentifierType pi = 0; pi < numberOfPoints; ++pi)
  {
    for (unsigned int i = 0; i < PointDimension; ++i)
    {
      m_Points[pi][i] = *pointCoordinates++;
    }
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

      CrossProduct(normal, v10, v12);

      this->m_OutputStream << "  facet normal ";
      this->m_OutputStream << normal[0] << " " << normal[1] << " " << normal[2] << std::endl;

      this->m_OutputStream << "    outer loop" << std::endl;

      this->m_OutputStream << "      vertex " << p0[0] << " " << p0[1] << " " << p0[2] << std::endl;
      this->m_OutputStream << "      vertex " << p1[0] << " " << p1[1] << " " << p1[2] << std::endl;
      this->m_OutputStream << "      vertex " << p2[0] << " " << p2[1] << " " << p2[2] << std::endl;

      this->m_OutputStream << "    endloop" << std::endl;
      this->m_OutputStream << "  endfacet" << std::endl;
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
}


void
STLMeshIO ::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end of namespace itk
