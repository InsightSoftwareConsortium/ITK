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

#include "itkOFFMeshIO.h"

#include <itksys/SystemTools.hxx>

namespace itk
{
OFFMeshIO
::OFFMeshIO()
{
  this->AddSupportedWriteExtension(".off");
  this->SetByteOrderToBigEndian();
  m_PointsStartPosition = itk::NumericTraits< StreamOffsetType >::ZeroValue();
  m_TriangleCellType = true;
}

OFFMeshIO
::~OFFMeshIO()
{
}

bool
OFFMeshIO
::CanReadFile(const char *fileName)
{
  if ( !itksys::SystemTools::FileExists(fileName, true) )
    {
    return false;
    }

  if ( itksys::SystemTools::GetFilenameLastExtension(fileName) != ".off" )
    {
    return false;
    }

  return true;
}

bool
OFFMeshIO
::CanWriteFile(const char *fileName)
{
  if ( itksys::SystemTools::GetFilenameLastExtension(fileName) != ".off" )
    {
    return false;
    }

  return true;
}

void
OFFMeshIO
::OpenFile()
{
  if ( this->m_FileName.empty() )
    {
    itkExceptionMacro("No input FileName");
    }

  if ( !itksys::SystemTools::FileExists( m_FileName.c_str() ) )
    {
    itkExceptionMacro("File " << this->m_FileName << " does not exist");
    }

  // Read file as ascii
  // Due to the windows doesn't work well for tellg() and seekg() for ASCII mode, hence we
  //open the file with std::ios::binary
  m_InputFile.open(this->m_FileName.c_str(), std::ios_base::in | std::ios::binary);

  // Test whether the file was opened
  if ( !m_InputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file " << this->m_FileName);
    }
}

void
OFFMeshIO
::CloseFile()
{
  if ( m_InputFile.is_open() )
    {
    m_InputFile.close();
    }
}

void
OFFMeshIO
::ReadMeshInformation()
{
  // Define input file stream and attach it to input file
  OpenFile();

  // Read and analyze the first line in the file
  std::string line;

  // The OFF file must containe "OFF"
  std::getline(m_InputFile, line, '\n');  // delimiter is '\n'
  if ( line.find("OFF") == std::string::npos )
    {
    itkExceptionMacro(<< "Error, the file doesn't begin with keyword \"OFF\" ");
    }

  // If the file is binary file, it contains "BINARY"
  if ( line.find("BINARY") != std::string::npos )
    {
    this->m_FileType = BINARY;
    }
  else
    {
    this->m_FileType = ASCII;
    }

  // Read and Set point dimension
  if ( line.find("nOFF") != std::string::npos )
    {
    m_InputFile >> this->m_PointDimension;
    m_PointDimension++;
    }
  else if ( line.find("4OFF") != std::string::npos )
    {
    this->m_PointDimension = 4;
    }
  else
    {
    this->m_PointDimension = 3;
    }

  // Ignore comment lines
  std::getline(m_InputFile, line, '\n');
  while ( line.find("#") != std::string::npos )
    {
    std::getline(m_InputFile, line, '\n');
    }

  // Read points and cells information
  if ( this->m_FileType == ASCII )
    {
    // Put the last line with output '#' into a stringstream.
    std::stringstream ss;
    ss << line;

    // Read number of points
    ss >> this->m_NumberOfPoints;

    // Read number of cells
    ss >> this->m_NumberOfCells;

    // Read number of edges
    unsigned int numberOfEdges = 0;
    ss >> numberOfEdges;

    // Read points start position in the file
    m_PointsStartPosition = m_InputFile.tellg();

    for ( SizeValueType id = 0; id < this->m_NumberOfPoints; id++ )
      {
      std::getline(m_InputFile, line, '\n');
      }

    // Set default cell component type
    this->m_CellBufferSize = this->m_NumberOfCells * 2;

    //Read each ecll's number of points and put them to cell buffer size
    unsigned int numberOfCellPoints = 0;
    for ( SizeValueType id = 0; id < this->m_NumberOfCells; id++ )
      {
      m_InputFile >> numberOfCellPoints;
      this->m_CellBufferSize += numberOfCellPoints;
      std::getline(m_InputFile, line, '\n');

      if ( numberOfCellPoints != 3 )
        {
        m_TriangleCellType = false;
        }
      }
    }
  // Read points and cells information from binary mesh
  else if ( this->m_FileType == BINARY )
    {
    // Read the number of points
    itk::uint32_t numberOfPoints;
    this->ReadBufferAsBinary(&numberOfPoints, m_InputFile, 1);
    this->m_NumberOfPoints = numberOfPoints;

    // Read the number of cells
    itk::uint32_t numberOfCells;
    this->ReadBufferAsBinary(&numberOfCells, m_InputFile, 1);
    this->m_NumberOfCells = numberOfCells;

    // Read number of edges
    itk::uint32_t numberOfEdges;
    this->ReadBufferAsBinary(&numberOfEdges, m_InputFile, 1);

    // Get points start position
    m_PointsStartPosition = m_InputFile.tellg();

    // Read points
    float *pointsBuffer = new float[this->m_NumberOfPoints * this->m_PointDimension];
    this->ReadBufferAsBinary(pointsBuffer, m_InputFile, this->m_NumberOfPoints * this->m_PointDimension);
    delete[] pointsBuffer;

    // Set default cell component type
    this->m_CellBufferSize = this->m_NumberOfCells * 2;

    // Read cells
    itk::uint32_t  numberOfCellPoints = 0;
    itk::uint32_t *cellsBuffer = new itk::uint32_t[this->m_NumberOfCells];
    for ( unsigned long id = 0; id < this->m_NumberOfCells; id++ )
      {
      this->ReadBufferAsBinary(&numberOfCellPoints, m_InputFile, 1);
      this->m_CellBufferSize += numberOfCellPoints;
      this->ReadBufferAsBinary(cellsBuffer, m_InputFile, numberOfCellPoints);
      if ( numberOfCellPoints != 3 )
        {
        m_TriangleCellType = false;
        }
      }
    delete[] cellsBuffer;
    }

  // Set default point component type
  this->m_PointComponentType = FLOAT;

  // Set default cell component type
  this->m_CellComponentType  = UINT;

  // If number of points is not equal zero, update points
  if ( this->m_NumberOfPoints )
    {
    this->m_UpdatePoints = true;
    }

  // If number of cells is not equal zero, update points
  if ( this->m_NumberOfCells )
    {
    this->m_UpdateCells = true;
    }

  // Set default point pixel component and point pixel type
  this->m_PointPixelComponentType = FLOAT;
  this->m_PointPixelType  = SCALAR;
  this->m_UpdatePointData = false;
  this->m_NumberOfPointPixelComponents = itk::NumericTraits< unsigned int >::OneValue();

  // Set default cell pixel component and point pixel type
  this->m_CellPixelComponentType = FLOAT;
  this->m_CellPixelType  = SCALAR;
  this->m_UpdateCellData = false;
  this->m_NumberOfCellPixelComponents = itk::NumericTraits< unsigned int >::OneValue();
}

void
OFFMeshIO
::ReadPoints(void *buffer)
{
  // Set file position to points start position
  m_InputFile.seekg(m_PointsStartPosition, std::ios::beg);

  // Read file according to ASCII or BINARY
  if ( this->m_FileType == ASCII )
    {
    this->ReadBufferAsAscii(static_cast< float * >( buffer ), m_InputFile, this->m_NumberOfPoints * this->m_PointDimension);
    }
  else if ( this->m_FileType == BINARY )
    {
    this->ReadBufferAsBinary(static_cast< float * >( buffer ), m_InputFile, this->m_NumberOfPoints * this->m_PointDimension);
    }
  else
    {
    itkExceptionMacro(<< "Invalid file type (not ASCII or BINARY)");
    }
}

void
OFFMeshIO
::ReadCells(void *buffer)
{
  itk::uint32_t *data = new itk::uint32_t[this->m_CellBufferSize - this->m_NumberOfCells];

  if ( this->m_FileType == ASCII )
    {
    this->ReadCellsBufferAsAscii(data, m_InputFile);
    }
  else if ( this->m_FileType == BINARY )
    {
    this->ReadBufferAsBinary(data, m_InputFile, this->m_CellBufferSize - this->m_NumberOfCells);
    }
  else
    {
    itkExceptionMacro(<< "Invalid file type (not ASCII or BINARY)");
    }

  CloseFile();

  if ( m_TriangleCellType )
    {
    this->WriteCellsBuffer(data, static_cast< unsigned int * >( buffer ), TRIANGLE_CELL, this->m_NumberOfCells);
    }
  else
    {
    this->WriteCellsBuffer(data, static_cast< unsigned int * >( buffer ), POLYGON_CELL, this->m_NumberOfCells);
    }

  delete[] data;
}

void
OFFMeshIO
::ReadPointData(void * itkNotUsed( buffer) )
{
}

void
OFFMeshIO
::ReadCellData(void * itkNotUsed( buffer) )
{
}

void
OFFMeshIO
::WriteMeshInformation()
{
  // Check file name
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro("No Input FileName");
    }

  // Write to output file
  std::ofstream outputFile;
  if ( this->m_FileType == ASCII )
    {
    outputFile.open(this->m_FileName.c_str(), std::ios::out);
    }
  else if ( m_FileType == BINARY )
    {
    outputFile.open(this->m_FileName.c_str(), std::ios::out | std::ios::binary);
    }

  if ( !outputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= " << this->m_FileName);
    }

  // Write Object file format header
  outputFile << "OFF " << std::endl;

  //Read points and cells information
  if ( this->m_FileType == ASCII )
    {
    // Write number of points
    outputFile << this->m_NumberOfPoints << "    ";

    // Write number of cells
    outputFile << this->m_NumberOfCells << "    ";

    // Write number of edges
    unsigned int numberOfEdges = 0;
    outputFile << numberOfEdges << std::endl;
    }
  else if ( this->m_FileType == BINARY )
    {
    // Write number of points
    itk::uint32_t numberOfPoints = static_cast<itk::uint32_t>(this->m_NumberOfPoints);
    this->WriteBufferAsBinary< itk::uint32_t >(&( numberOfPoints ), outputFile, 1);

    // Write number of cells
    itk::uint32_t numberOfCells = static_cast<itk::uint32_t>(this->m_NumberOfCells);
    this->WriteBufferAsBinary< itk::uint32_t >(&( numberOfCells ), outputFile, 1);

    // Write number of edges
    itk::uint32_t numberOfEdges = 0;
    this->WriteBufferAsBinary< itk::uint32_t >(&( numberOfEdges ), outputFile, 1);
    }

  outputFile.close();
}

void
OFFMeshIO
::WritePoints(void *buffer)
{
  // check file name
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro("No Input FileName");
    }

  // Write to output file
  std::ofstream outputFile;
  if ( this->m_FileType == ASCII )
    {
    outputFile.open(this->m_FileName.c_str(), std::ios::app);
    }
  else if ( m_FileType == BINARY )
    {
    outputFile.open(this->m_FileName.c_str(), std::ios::app | std::ios::binary);
    }

  if ( !outputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= " << this->m_FileName);
    }

  // Write points
  if ( this->m_FileType == ASCII )
    {
    switch ( this->m_PointComponentType )
      {
      case UCHAR:
        {
        WriteBufferAsAscii(static_cast< unsigned char * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);
        break;
        }
      case CHAR:
        {
        WriteBufferAsAscii(static_cast< char * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);

        break;
        }
      case USHORT:
        {
        WriteBufferAsAscii(static_cast< unsigned short * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);

        break;
        }
      case SHORT:
        {
        WriteBufferAsAscii(static_cast< short * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);

        break;
        }
      case UINT:
        {
        WriteBufferAsAscii(static_cast< unsigned int * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);

        break;
        }
      case INT:
        {
        WriteBufferAsAscii(static_cast< int * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);

        break;
        }
      case ULONG:
        {
        WriteBufferAsAscii(static_cast< unsigned long * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);

        break;
        }
      case LONG:
        {
        WriteBufferAsAscii(static_cast< long * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);

        break;
        }
      case ULONGLONG:
        {
        WriteBufferAsAscii(static_cast< unsigned long long * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);

        break;
        }
      case LONGLONG:
        {
        WriteBufferAsAscii(static_cast< long long * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);

        break;
        }
      case FLOAT:
        {
        WriteBufferAsAscii(static_cast< float * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);

        break;
        }
      case DOUBLE:
        {
        WriteBufferAsAscii(static_cast< double * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);

        break;
        }
      case LDOUBLE:
        {
        WriteBufferAsAscii(static_cast< long double * >( buffer ), outputFile, m_NumberOfPoints, m_PointDimension);

        break;
        }
      default:
        {
        itkExceptionMacro(<< "Unknown point pixel component type" << std::endl);
        }
      }
    }
  else if ( this->m_FileType == BINARY )
    {
    switch ( this->m_PointComponentType )
      {
      case UCHAR:
        {
        WriteBufferAsBinary< float >(static_cast< unsigned char * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);
        break;
        }
      case CHAR:
        {
        WriteBufferAsBinary< float >(static_cast< char * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);

        break;
        }
      case USHORT:
        {
        WriteBufferAsBinary< float >(static_cast< unsigned short * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);

        break;
        }
      case SHORT:
        {
        WriteBufferAsBinary< float >(static_cast< short * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);

        break;
        }
      case UINT:
        {
        WriteBufferAsBinary< float >(static_cast< unsigned int * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);

        break;
        }
      case INT:
        {
        WriteBufferAsBinary< float >(static_cast< int * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);

        break;
        }
      case ULONG:
        {
        WriteBufferAsBinary< float >(static_cast< unsigned long * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);

        break;
        }
      case LONG:
        {
        WriteBufferAsBinary< float >(static_cast< long * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);

        break;
        }
      case ULONGLONG:
        {
        WriteBufferAsBinary< float >(static_cast< unsigned long long * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);

        break;
        }
      case LONGLONG:
        {
        WriteBufferAsBinary< float >(static_cast< long long * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);

        break;
        }
      case FLOAT:
        {
        WriteBufferAsBinary< float >(static_cast< float * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);

        break;
        }
      case DOUBLE:
        {
        WriteBufferAsBinary< float >(static_cast< double * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);

        break;
        }
      case LDOUBLE:
        {
        WriteBufferAsBinary< float >(static_cast< long double * >( buffer ), outputFile, m_NumberOfPoints * m_PointDimension);

        break;
        }
      default:
        {
        itkExceptionMacro(<< "Unknown point pixel component type" << std::endl);
        }
      }
    }

  outputFile.close();
}

void
OFFMeshIO
::WriteCells(void *buffer)
{
  // Check file name
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro("No Input FileName");
    }

  // Write to output file
  std::ofstream outputFile;
  if ( this->m_FileType == ASCII )
    {
    outputFile.open(this->m_FileName.c_str(), std::ios::app);
    }
  else if ( m_FileType == BINARY )
    {
    outputFile.open(this->m_FileName.c_str(), std::ios::app | std::ios::binary);
    }

  if ( !outputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= " << this->m_FileName);
    }

  // Write cells
  if ( this->m_FileType == ASCII )
    {
    switch ( this->m_CellComponentType )
      {
      case UCHAR:
        {
        WriteCellsAsAscii(static_cast< unsigned char * >( buffer ), outputFile);

        break;
        }
      case CHAR:
        {
        WriteCellsAsAscii(static_cast< unsigned char * >( buffer ), outputFile);

        break;
        }
      case USHORT:
        {
        WriteCellsAsAscii(static_cast< unsigned short * >( buffer ), outputFile);

        break;
        }
      case SHORT:
        {
        WriteCellsAsAscii(static_cast< short * >( buffer ), outputFile);

        break;
        }
      case UINT:
        {
        WriteCellsAsAscii(static_cast< unsigned int * >( buffer ), outputFile);

        break;
        }
      case INT:
        {
        WriteCellsAsAscii(static_cast< int * >( buffer ), outputFile);

        break;
        }
      case ULONG:
        {
        WriteCellsAsAscii(static_cast< long * >( buffer ), outputFile);

        break;
        }
      case LONG:
        {
        WriteCellsAsAscii(static_cast< long * >( buffer ), outputFile);

        break;
        }
      case ULONGLONG:
        {
        WriteCellsAsAscii(static_cast< unsigned long long * >( buffer ), outputFile);

        break;
        }
      case LONGLONG:
        {
        WriteCellsAsAscii(static_cast< long long * >( buffer ), outputFile);

        break;
        }
      case FLOAT:
        {
        WriteCellsAsAscii(static_cast< float * >( buffer ), outputFile);

        break;
        }
      case DOUBLE:
        {
        WriteCellsAsAscii(static_cast< double * >( buffer ), outputFile);

        break;
        }
      case LDOUBLE:
        {
        WriteCellsAsAscii(static_cast< long double * >( buffer ), outputFile);

        break;
        }
      default:
        {
        itkExceptionMacro(<< "Unknown cell pixel component type" << std::endl);
        }
      }
    }
  else if ( this->m_FileType == BINARY )
    {
    switch ( this->m_CellComponentType )
      {
      case UCHAR:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< unsigned char * >( buffer ), outputFile);

        break;
        }
      case CHAR:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< char * >( buffer ), outputFile);

        break;
        }
      case USHORT:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< unsigned short * >( buffer ), outputFile);

        break;
        }
      case SHORT:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< short * >( buffer ), outputFile);

        break;
        }
      case UINT:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< unsigned int * >( buffer ), outputFile);

        break;
        }
      case INT:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< int * >( buffer ), outputFile);

        break;
        }
      case ULONG:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< long * >( buffer ), outputFile);

        break;
        }
      case LONG:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< long * >( buffer ), outputFile);

        break;
        }
      case ULONGLONG:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< unsigned long long * >( buffer ), outputFile);

        break;
        }
      case LONGLONG:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< long long * >( buffer ), outputFile);

        break;
        }
      case FLOAT:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< float * >( buffer ), outputFile);

        break;
        }
      case DOUBLE:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< double * >( buffer ), outputFile);

        break;
        }
      case LDOUBLE:
        {
        WriteCellsAsBinary< itk::uint32_t >(static_cast< long double * >( buffer ), outputFile);

        break;
        }
      default:
        {
        itkExceptionMacro(<< "Unknown cell pixel component type" << std::endl);
        }
      }
    }

  outputFile.close();
}

void
OFFMeshIO
::WritePointData(void * itkNotUsed( buffer ) )
{
}

void
OFFMeshIO
::WriteCellData(void * itkNotUsed( buffer ) )
{
}

void
OFFMeshIO
::Write()
{
}

void
OFFMeshIO
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // namespace itk end
