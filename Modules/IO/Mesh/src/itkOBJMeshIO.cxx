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

#include "itkOBJMeshIO.h"
#include "itkNumericTraits.h"
#include <itksys/SystemTools.hxx>
#include  <locale>
#include <vector>


namespace itk
{
OBJMeshIO
::OBJMeshIO()
{
  this->AddSupportedWriteExtension(".obj");
}

OBJMeshIO
::~OBJMeshIO()
{
}

bool
OBJMeshIO
::CanReadFile(const char *fileName)
{
  if ( !itksys::SystemTools::FileExists(fileName, true) )
    {
    return false;
    }

  if ( itksys::SystemTools::GetFilenameLastExtension(fileName) != ".obj" )
    {
    return false;
    }

  return true;
}

bool
OBJMeshIO
::CanWriteFile(const char *fileName)
{
  if ( itksys::SystemTools::GetFilenameLastExtension(fileName) != ".obj" )
    {
    return false;
    }

  return true;
}

void
OBJMeshIO
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
  // Due to the windows couldn't work well for tellg() and seekg() for ASCII
  // mode, hence we
  // open the file with std::ios::binary
  m_InputFile.open(this->m_FileName.c_str(), std::ios_base::in | std::ios::binary);

  // Test whether the file was opened
  if ( !m_InputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file " << this->m_FileName);
    }

  // If not set to start of file stream, windows won't work properly
  m_InputFile.seekg(0, std::ios::beg);
}

void
OBJMeshIO
::CloseFile()
{
  if ( m_InputFile.is_open() )
    {
    m_InputFile.close();
    }
}

bool
OBJMeshIO
::SplitLine(const std::string& line, std::string& type, std::string& content)
{
  std::locale loc;
  std::string::const_iterator start = line.begin();

  while (start != line.end() && std::isspace(*start, loc))
    {
    ++start; // start should be at the first non-empty sign.
    }

  std::string::const_iterator it = start;
  while (it != line.end() && !std::isspace(*it, loc))
    {
    ++it;
    }

  if (it == line.end())
    {
    return false;
    }
  // else anyways
  type = line.substr(start - line.begin(), it - start);
  content = line.substr(it - line.begin());

  return true;
}

void
OBJMeshIO
::ReadMeshInformation()
{
  // Define input file stream and attach it to input file
  OpenFile();

  // Read and analyze the first line in the file
  SizeValueType numberOfCellPoints = 0;
  this->m_NumberOfPoints = 0;
  this->m_NumberOfCells = 0;
  this->m_NumberOfPointPixels = 0;
  std::string   line;
  std::string   inputLine;
  std::string   type;
  std::locale   loc;
  while ( std::getline(m_InputFile, line, '\n') )
    {
    if ( SplitLine(line, type, inputLine) && !inputLine.empty() )
      {
      if ( type == "v" )
        {
        this->m_NumberOfPoints++;
        }
      else if ( type == "f" )
        {
        this->m_NumberOfCells++;

        std::stringstream ss(inputLine);
        std::string       item;
        while ( ss >> item )
          {
          numberOfCellPoints++;
          }
        }
      else if ( type == "vn" )
        {
        this->m_NumberOfPointPixels++;
        this->m_UpdatePointData = true;
        }
      }
    }

  this->m_PointDimension = 3;

  // If number of points is not equal zero, update points
  if ( this->m_NumberOfPoints )
    {
    this->m_UpdatePoints = true;
    }
  else
    {
    this->m_UpdatePoints = false;
    }

  // If number of cells is not equal zero, update points
  if ( this->m_NumberOfCells )
    {
    this->m_UpdateCells = true;
    }
  else
    {
    this->m_UpdateCells = false;
    }

  // Set default point component type
  this->m_PointComponentType = FLOAT;

  // Set default cell component type
  this->m_CellComponentType  = LONG;
  this->m_CellBufferSize = this->m_NumberOfCells * 2 + numberOfCellPoints;

  // Set default point pixel component and point pixel type
  this->m_PointPixelComponentType = FLOAT;
  this->m_PointPixelType = VECTOR;
  this->m_NumberOfPointPixelComponents = 3;
  this->m_NumberOfPointPixels = this->m_NumberOfPoints;
  // this->m_UpdatePointData = true;

  // Set default cell pixel component and point pixel type
  this->m_CellPixelComponentType = FLOAT;
  this->m_CellPixelType  = SCALAR;
  this->m_NumberOfCellPixelComponents = itk::NumericTraits< unsigned int >::OneValue();
  this->m_UpdateCellData = false;

  CloseFile();
}

void
OBJMeshIO
::ReadPoints(void *buffer)
{
  // Define input file stream and attach it to input file
  OpenFile();

  // Number of data array
  float *       data = static_cast< float * >( buffer );
  SizeValueType index = 0;

  // Read and analyze the first line in the file
  std::string line;
  std::string inputLine;
  std::string type;
  std::locale loc;
  while ( std::getline(m_InputFile, line, '\n') )
    {
    if ( SplitLine(line, type, inputLine) && !inputLine.empty() )
      {
      if ( type == "v" )
        {
        std::stringstream ss(inputLine);
        for ( unsigned int ii = 0; ii < this->m_PointDimension; ii++ )
          {
          ss >> data[index++];
          }
        }
      }
    }

  CloseFile();
}

void
OBJMeshIO
::ReadCells(void *buffer)
{
  // Define input file stream and attach it to input file
  OpenFile();

  // Read and analyze the first line in the file
  long *        data = new long[this->m_CellBufferSize - this->m_NumberOfCells];
  SizeValueType index = 0;

  std::string line;
  std::string inputLine;
  std::string type;
  std::locale loc;
  while ( std::getline(m_InputFile, line, '\n') )
    {
    if ( SplitLine(line, type, inputLine) && !inputLine.empty() )
      {
      if ( type == "f" )
        {
        std::stringstream   ss(inputLine);
        std::string         item;
        std::vector< long > idList;
        while ( ss >> item )
          {
          long                   id;
          std::string::size_type pos = item.find('/');
          while ( pos != std::string::npos )
            {
            item.replace(pos, 1, " ");
            pos = item.find('/', pos);
            }

          std::stringstream st(item);
          st >> id;

          idList.push_back(id);
          }

        data[index++] = static_cast<long>( idList.size() );
        for ( std::vector< long >::const_iterator it = idList.begin(); it != idList.end(); ++it )
          {
          data[index++] = ( *it - 1 );
          }
        }
      }
    }

  CloseFile();

  this->WriteCellsBuffer(data, static_cast< long * >( buffer ), POLYGON_CELL, this->m_NumberOfCells);
  // this->WriteCellsBuffer(data, static_cast<unsigned int *>(buffer),
  // TRIANGLE_CELL, 3, this->m_NumberOfCells);
  delete[] data;
}

void
OBJMeshIO
::ReadPointData(void *buffer)
{
  // Define input file stream and attach it to input file
  OpenFile();

  // Number of data array
  float *       data = static_cast< float * >( buffer );
  SizeValueType index = 0;

  // Read and analyze the first line in the file
  std::string line;
  std::string inputLine;
  std::string type;
  std::locale loc;
  while ( std::getline(m_InputFile, line, '\n') )
    {
    if ( SplitLine(line, type, inputLine) && !inputLine.empty() )
      {
      if ( type == "vn" )
        {
        std::stringstream ss(inputLine);
        for ( unsigned int ii = 0; ii < this->m_PointDimension; ii++ )
          {
          ss >> data[index++];
          }
        }
      }
    }

  CloseFile();
}

void
OBJMeshIO
::ReadCellData(void * itkNotUsed( buffer ) )
{
}

void
OBJMeshIO
::WriteMeshInformation()
{
  // Check file name
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro("No Input FileName");
    }

  // Define output stream and open it
  std::ofstream outputFile;
  outputFile.open( this->m_FileName.c_str() );

  // Test whether input file was opened successfuly
  if ( !outputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= " << this->m_FileName);
    }

  // write comments
  outputFile << "# OBJ file generated by ITK\n";

  // Write the number of points and number of cells
  outputFile << "#  Number of points " << this->m_NumberOfPoints << "\n";
  outputFile << "#  Number of cells " << this->m_NumberOfCells << "\n";
  outputFile.close();
}

void
OBJMeshIO::
WritePoints(void *buffer)
{
  // Check file name
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro("No Input FileName");
    }

  // Define output stream and open it
  std::ofstream outputFile;
  outputFile.open(this->m_FileName.c_str(), std::ios_base::app);

  // Test whether input file was opened successfuly
  if ( !outputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= " << this->m_FileName);
    }

  // Write points
  switch ( this->m_PointComponentType )
    {
    case UCHAR:
      {
      WritePoints(static_cast< unsigned char * >( buffer ), outputFile);
      break;
      }
    case CHAR:
      {
      WritePoints(static_cast< char * >( buffer ), outputFile);

      break;
      }
    case USHORT:
      {
      WritePoints(static_cast< unsigned short * >( buffer ), outputFile);

      break;
      }
    case SHORT:
      {
      WritePoints(static_cast< short * >( buffer ), outputFile);

      break;
      }
    case UINT:
      {
      WritePoints(static_cast< unsigned int * >( buffer ), outputFile);

      break;
      }
    case INT:
      {
      WritePoints(static_cast< int * >( buffer ), outputFile);

      break;
      }
    case ULONG:
      {
      WritePoints(static_cast< unsigned long * >( buffer ), outputFile);

      break;
      }
    case LONG:
      {
      WritePoints(static_cast< long * >( buffer ), outputFile);

      break;
      }
    case ULONGLONG:
      {
      WritePoints(static_cast< unsigned long long * >( buffer ), outputFile);

      break;
      }
    case LONGLONG:
      {
      WritePoints(static_cast< long long * >( buffer ), outputFile);

      break;
      }
    case FLOAT:
      {
      WritePoints(static_cast< float * >( buffer ), outputFile);

      break;
      }
    case DOUBLE:
      {
      WritePoints(static_cast< double * >( buffer ), outputFile);

      break;
      }
    case LDOUBLE:
      {
      WritePoints(static_cast< long double * >( buffer ), outputFile);

      break;
      }
    default:
      {
      itkExceptionMacro(<< "Unknown point component type" << std::endl);
      }
    }

  outputFile.close();
}

void
OBJMeshIO
::WriteCells(void *buffer)
{
  // check file name
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro("No Input FileName");
    }

  // Define output stream and open it
  std::ofstream outputFile;
  outputFile.open(this->m_FileName.c_str(), std::ios_base::app);

  // Test whether input file was opened successfuly
  if ( !outputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= " << this->m_FileName);
    }

  switch ( this->m_CellComponentType )
    {
    case UCHAR:
      {
      WriteCells(static_cast< unsigned char * >( buffer ), outputFile);
      break;
      }
    case CHAR:
      {
      WriteCells(static_cast< unsigned char * >( buffer ), outputFile);
      break;
      }
    case USHORT:
      {
      WriteCells(static_cast< unsigned short * >( buffer ), outputFile);
      break;
      }
    case SHORT:
      {
      WriteCells(static_cast< short * >( buffer ), outputFile);
      break;
      }
    case UINT:
      {
      WriteCells(static_cast< unsigned int * >( buffer ), outputFile);
      break;
      }
    case INT:
      {
      WriteCells(static_cast< int * >( buffer ), outputFile);
      break;
      }
    case ULONG:
      {
      WriteCells(static_cast< unsigned long * >( buffer ), outputFile);
      break;
      }
    case LONG:
      {
      WriteCells(static_cast< long * >( buffer ), outputFile);
      break;
      }
    case ULONGLONG:
      {
      WriteCells(static_cast< unsigned long long * >( buffer ), outputFile);
      break;
      }
    case LONGLONG:
      {
      WriteCells(static_cast< long long * >( buffer ), outputFile);
      break;
      }
    case FLOAT:
      {
      WriteCells(static_cast< float * >( buffer ), outputFile);
      break;
      }
    case DOUBLE:
      {
      WriteCells(static_cast< double * >( buffer ), outputFile);
      break;
      }
    case LDOUBLE:
      {
      WriteCells(static_cast< long double * >( buffer ), outputFile);
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
OBJMeshIO
::WritePointData(void *buffer)
{
  // Point data must be vector
  if ( !m_UpdatePointData || m_NumberOfPointPixelComponents != m_PointDimension )
    {
    itkExceptionMacro(<< "MNI Mesh required point normals");
    }

  // Check file name
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro("No Input FileName");
    }

  // Define output stream and open it
  std::ofstream outputFile;
  outputFile.open(this->m_FileName.c_str(), std::ios_base::app);

  // Test whether input file was opened successfuly
  if ( !outputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= " << this->m_FileName);
    }

  // Write point data
  switch ( this->m_PointPixelComponentType )
    {
    case UCHAR:
      {
      WritePointData(static_cast< unsigned char * >( buffer ), outputFile);
      break;
      }
    case CHAR:
      {
      WritePointData(static_cast< char * >( buffer ), outputFile);

      break;
      }
    case USHORT:
      {
      WritePointData(static_cast< unsigned short * >( buffer ), outputFile);

      break;
      }
    case SHORT:
      {
      WritePointData(static_cast< short * >( buffer ), outputFile);

      break;
      }
    case UINT:
      {
      WritePointData(static_cast< unsigned int * >( buffer ), outputFile);

      break;
      }
    case INT:
      {
      WritePointData(static_cast< int * >( buffer ), outputFile);

      break;
      }
    case ULONG:
      {
      WritePointData(static_cast< unsigned long * >( buffer ), outputFile);

      break;
      }
    case LONG:
      {
      WritePointData(static_cast< long * >( buffer ), outputFile);

      break;
      }
    case ULONGLONG:
      {
      WritePointData(static_cast< unsigned long long * >( buffer ), outputFile);

      break;
      }
    case LONGLONG:
      {
      WritePointData(static_cast< long long * >( buffer ), outputFile);

      break;
      }
    case FLOAT:
      {
      WritePointData(static_cast< float * >( buffer ), outputFile);

      break;
      }
    case DOUBLE:
      {
      WritePointData(static_cast< double * >( buffer ), outputFile);

      break;
      }
    case LDOUBLE:
      {
      WritePointData(static_cast< long double * >( buffer ), outputFile);

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
OBJMeshIO
::WriteCellData(void * itkNotUsed( buffer ) )
{
}

void
OBJMeshIO
::Write()
{
}

void
OBJMeshIO
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // namespace itk end
