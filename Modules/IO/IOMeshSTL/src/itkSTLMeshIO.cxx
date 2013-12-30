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
STLMeshIO ::WriteCells(void * buffer)
{

  IdentifierType numberOfPolygons = this->GetNumberOfCells();

  IdentifierType * cellsBuffer = reinterpret_cast<IdentifierType *>(buffer);

  SizeValueType index = 0;

  typedef MeshIOBase::CellGeometryType CellGeometryType;

  std::cout << "numberOfPolygons " << numberOfPolygons << std::endl;

  for (SizeValueType polygonItr = 0; polygonItr < numberOfPolygons; polygonItr++)
  {
    const MeshIOBase::CellGeometryType cellType = static_cast<CellGeometryType>(cellsBuffer[index++]);
    const IdentifierType               numberOfVerticesInCell = static_cast<IdentifierType>(cellsBuffer[index++]);

    const bool isTriangle = (cellType == TRIANGLE_CELL) || (cellType == POLYGON_CELL && numberOfVerticesInCell == 3);

    if (isTriangle)
    {
      std::cout << "POLYGON_CELL with " << numberOfVerticesInCell << " vertices " << std::endl;
      this->m_OutputStream << "  facet normal" << std::endl;
      this->m_OutputStream << "    outer loop" << std::endl;
      for (unsigned int jj = 0; jj < numberOfVerticesInCell; jj++)
      {
        this->m_OutputStream << "      vertex " << cellsBuffer[index++] << std::endl;
      }
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
