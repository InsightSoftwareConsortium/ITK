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

#include "itkBYUMeshIO.h"

#include <itksys/SystemTools.hxx>

namespace itk
{
BYUMeshIO
::BYUMeshIO() :
  m_FilePosition(0),
  m_PartId(NumericTraits< SizeValueType >::max()),
  m_FirstCellId(NumericTraits< SizeValueType >::OneValue()),
  m_LastCellId(NumericTraits< SizeValueType >::max())
{
  this->AddSupportedWriteExtension(".byu");
}

BYUMeshIO
::~BYUMeshIO()
{
}

bool
BYUMeshIO
::CanReadFile(const char *fileName)
{
  if( !itksys::SystemTools::FileExists(fileName, true) )
    {
    return false;
    }

  if( itksys::SystemTools::GetFilenameLastExtension(fileName) != ".byu" )
    {
    return false;
    }

  return true;
}

bool
BYUMeshIO
::CanWriteFile(const char *fileName)
{
  if( itksys::SystemTools::GetFilenameLastExtension(fileName) != ".byu" )
    {
    return false;
    }

  return true;
}

void
BYUMeshIO
::ReadMeshInformation()
{
  // Define input file stream and attach it to input file
  std::ifstream inputFile;

  // Due to the windows couldn't work well for tellg() and seekg() for ASCII mode, hence we
  //open the file with std::ios::binary
  inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);

  if ( !inputFile.is_open() )
    {
    itkExceptionMacro(<< "Unable to open input file " << this->m_FileName);
    }

  // Read the ASCII file information
  unsigned int numberOfParts = 0;
  unsigned int numberOfConnectivityEntries = 0;

  // Read the number of points and number of cells
  inputFile >> numberOfParts;
  inputFile >> this->m_NumberOfPoints;
  inputFile >> this->m_NumberOfCells;
  inputFile >> numberOfConnectivityEntries;

  // Determine which part to read, default is to readl all parts
  if ( m_PartId > numberOfParts )
    {
    for ( unsigned int ii = 0; ii < numberOfParts; ii++ )
      {
      inputFile >> m_FirstCellId >> m_LastCellId;
      }

    m_FirstCellId = 1;
    m_LastCellId = this->m_NumberOfCells;
    }
  else
    {
    unsigned int firstId;
    unsigned int lastId;
    for ( unsigned int ii = 0; ii < m_PartId; ii++ )
      {
      inputFile >> firstId >> lastId;
      }

    inputFile >> m_FirstCellId;
    inputFile >> m_LastCellId;

    for ( unsigned int ii = m_PartId + 1; ii < numberOfParts; ii++ )
      {
      inputFile >> firstId >> lastId;
      }
    }

  // Determine the start position of points
  m_FilePosition = inputFile.tellg();

  /** 6. Set default parameters */
  this->m_PointDimension = 3;
  this->m_FileType = ASCII;

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

  // Set default point component type
  this->m_PointComponentType = DOUBLE;

  // Read and omit points
  double x;
  for ( SizeValueType ii = 0; ii < this->m_NumberOfPoints; ii++ )
    {
    for ( unsigned int jj = 0; jj < this->m_PointDimension; jj++ )
      {
      inputFile >> x;
      }
    }

  // Determine cellbuffersize
  int ptId;
  this->m_CellBufferSize = 0;
  SizeValueType numLines = 0;
  while ( numLines < this->m_NumberOfCells )
    {
    inputFile >> ptId;

    this->m_CellBufferSize++;
    if ( ptId < 0 )
      {
      numLines++;
      }
    }

  // Set default cell component type
  this->m_CellComponentType  = UINT;
  this->m_CellBufferSize += this->m_NumberOfCells * 2;

  // Set default point pixel component and point pixel type
  this->m_PointPixelComponentType = FLOAT;
  this->m_PointPixelType = SCALAR;
  this->m_NumberOfPointPixelComponents = itk::NumericTraits< unsigned int >::OneValue();

  // Set default cell pixel component and point pixel type
  this->m_CellPixelComponentType = FLOAT;
  this->m_CellPixelType  = SCALAR;
  this->m_NumberOfCellPixelComponents = itk::NumericTraits< unsigned int >::OneValue();

  inputFile.close();
}

void
BYUMeshIO
::ReadPoints(void *buffer)
{
  // Define input file stream and attach it to input file
  std::ifstream inputFile;

  /** Due to the windows couldn't work well for tellg() and seekg() for ASCII mode, hence we
  open the file with std::ios::binary */
  inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);

  if ( !inputFile.is_open() )
    {
    itkExceptionMacro(<< "Unable to open input file " << this->m_FileName);
    }

  // Set the position to points start
  inputFile.seekg(m_FilePosition, std::ios::beg);

  // Number of data array
  double *data = static_cast< double * >( buffer );

  // Read points
  inputFile.precision(12);

  SizeValueType index = 0;
  for ( SizeValueType id = 0; id < this->m_NumberOfPoints; id++ )
    {
    for ( unsigned int ii = 0; ii < this->m_PointDimension; ii++ )
      {
      inputFile >> data[index++];
      }
    }

  // Determine cells start position
  m_FilePosition = inputFile.tellg();
  inputFile.close();
}

void
BYUMeshIO
::ReadCells(void *buffer)
{
  // Define input file stream and attach it to input file
  std::ifstream inputFile;

  inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);

  if ( !inputFile.is_open() )
    {
    itkExceptionMacro(<< "Unable to open input file " << this->m_FileName);
    }

  // Set the position to current position
  inputFile.seekg(m_FilePosition, std::ios::beg);

  // Get cell buffer
  inputFile.precision(12);
  unsigned int *data = static_cast< unsigned int * >( buffer );
  SizeValueType  numPoints = 0;
  SizeValueType id = itk::NumericTraits< SizeValueType >::ZeroValue();
  SizeValueType index = 2;
  int           ptId;
  m_FirstCellId -= 1;
  m_LastCellId -= 1;
  while ( id < this->m_NumberOfCells )
    {
    inputFile >> ptId;
    if ( ptId >= 0 )
      {
      if ( id >= m_FirstCellId && id <= m_LastCellId )
        {
        data[index++] = ptId - 1;
        numPoints++;
        }
      }
    else
      {
      if ( id >= m_FirstCellId && id <= m_LastCellId )
        {
        data[index++] = -( ptId + 1 );
        numPoints++;
        data[index - numPoints - 2] = MeshIOBase::POLYGON_CELL;
        data[index - numPoints - 1] = numPoints;
        numPoints = 0;
        index += 2;
        }
      id++;
      }
    }

  inputFile.close();
}

void
BYUMeshIO
::ReadPointData(void * itkNotUsed( buffer) )
{
}

void
BYUMeshIO
::ReadCellData(void * itkNotUsed( buffer) )
{
}

void
BYUMeshIO
::WriteMeshInformation()
{
  // Check file name
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro("No Input FileName");
    }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str(), std::ios::out);

  if ( !outputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= " << this->m_FileName);
    }

  // Write BYU file header
  Indent indent(7);
  outputFile << indent << 1;
  outputFile << indent << this->m_NumberOfPoints;
  outputFile << indent << this->m_NumberOfCells;
  outputFile << indent << this->m_CellBufferSize - 2 * this->m_NumberOfCells << std::endl;
  outputFile << indent << 1;
  outputFile << indent << this->m_NumberOfCells << std::endl;

  outputFile.close();
}

void
BYUMeshIO
::WritePoints(void *buffer)
{
  // check file name
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro("No Input FileName");
    }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str(), std::ios_base::app);

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
      itkExceptionMacro(<< "Unknown point pixel component type" << std::endl);
      }
    }

  outputFile.close();
}

void
BYUMeshIO
::WriteCells(void *buffer)
{
  // Check file name
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro("No Input FileName");
    }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str(), std::ios_base::app);

  if ( !outputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= " << this->m_FileName);
    }

  // Write polygons
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
      itkExceptionMacro(<< "Unknown cell pixel component type" << std::endl);
      }
    }

  outputFile.close();
}

void
BYUMeshIO
::WritePointData(void * itkNotUsed( buffer) )
{
}

void
BYUMeshIO
::WriteCellData(void * itkNotUsed( buffer) )
{
}

void
BYUMeshIO
::Write()
{
}

void
BYUMeshIO
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "PartId: " << m_PartId << std::endl;
  os << indent << "First Cell Id: " << m_FirstCellId << std::endl;
  os << indent << "Last Cell Id: " << m_LastCellId << std::endl;
}
} // namespace itk end
