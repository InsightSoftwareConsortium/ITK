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

#include "itkVTKPolyDataMeshIO.h"

#include <itksys/SystemTools.hxx>
#include <fstream>

namespace itk
{
// Constructor
VTKPolyDataMeshIO
::VTKPolyDataMeshIO()
{
  this->AddSupportedWriteExtension(".vtk");
  this->m_ByteOrder = BigEndian;

  MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
  EncapsulateMetaData< StringType >(metaDic, "pointScalarDataName", "PointScalarData");
  EncapsulateMetaData< StringType >(metaDic, "pointColorScalarDataName", "PointColorScalarData");
  EncapsulateMetaData< StringType >(metaDic, "pointVectorDataName", "PointVectorData");
  EncapsulateMetaData< StringType >(metaDic, "pointTensorDataName", "PointTensorData");
  EncapsulateMetaData< StringType >(metaDic, "cellScalarDataName", "CellScalarData");
  EncapsulateMetaData< StringType >(metaDic, "cellColorScalarDataName", "CellColorScalarData");
  EncapsulateMetaData< StringType >(metaDic, "cellVectorDataName", "CellVectorData");
  EncapsulateMetaData< StringType >(metaDic, "cellTensorDataName", "CellTensorData");
}

VTKPolyDataMeshIO
::~VTKPolyDataMeshIO()
{
}

bool
VTKPolyDataMeshIO
::CanReadFile(const char *fileName)
{
  if ( !itksys::SystemTools::FileExists(fileName, true) )
    {
    return false;
    }

  if ( itksys::SystemTools::GetFilenameLastExtension(fileName) != ".vtk" )
    {
    return false;
    }

  return true;
}

bool
VTKPolyDataMeshIO
::CanWriteFile(const char *fileName)
{
  if ( itksys::SystemTools::GetFilenameLastExtension(fileName) != ".vtk" )
    {
    return false;
    }

  return true;
}

MeshIOBase::IOComponentType
VTKPolyDataMeshIO::GetComponentTypeFromString(const std::string & pointType)
{
  IOComponentType compType;
  if ( pointType == "unsigned_char" )
    {
    compType = UCHAR;
    }
  else if ( pointType == "char" )
    {
    compType = CHAR;
    }
  else if ( pointType == "unsigned_short" )
    {
    compType = USHORT;
    }
  else if ( pointType == "short" )
    {
    compType = SHORT;
    }
  else if ( pointType == "unsigned_int" )
    {
    compType = UINT;
    }
  else if ( pointType == "int" )
    {
    compType = INT;
    }
  else if ( pointType == "unsigned_long" )
    {
    compType = ULONG;
    }
  else if ( pointType == "long" )
    {
    compType = LONG;
    }
  else if ( pointType == "unsigned_long_long" || pointType == "vtktypeuint64" )
    {
    compType = ULONGLONG; // is this supported by standard vtk format?
    }
  else if ( pointType == "long_long"  || pointType == "vtktypeint64" )
    {
    compType = LONGLONG; // is this supported by standard vtk format?
    }
  else if ( pointType == "float" )
    {
    compType = FLOAT;
    }
  else if ( pointType == "double" )
    {
    compType = DOUBLE;
    }
  else if ( pointType == "long_double" )
    {
    compType = LDOUBLE; // not supported by standard vtk format
    }
  else
    {
    compType = UNKNOWNCOMPONENTTYPE;
    }
  return compType;
}

void
VTKPolyDataMeshIO
::ReadMeshInformation()
{
  // Read input file into a file stream
  std::ifstream inputFile;

  // Use default filetype
  if ( this->m_FileType == ASCII )
    {
    inputFile.open(this->m_FileName.c_str(), std::ios::in);
    }
  else if ( m_FileType == BINARY )
    {
    inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
    }

  if ( !inputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n" "inputFilename= " << this->m_FileName);
    }

  // Define used variables
  unsigned int numLine = 0;
  std::string line;

  // Read vtk header file(the first 3 lines)
  while ( !inputFile.eof() && numLine < 3 )
    {
    std::getline(inputFile, line, '\n');
    ++numLine;
    }

  // Determine file type
  if ( line.find("ASCII") != std::string::npos )
    {
    if ( this->m_FileType != ASCII )
      {
      this->m_FileType = ASCII;
#ifdef _WIN32
      inputFile.close();
      inputFile.open(this->m_FileName.c_str(), std::ios::in);
      if ( !inputFile.is_open() )
        {
        itkExceptionMacro("Unable to open file\n" "inputFilename= " << this->m_FileName);
        }
#endif
      }
    }
  else if ( line.find("BINARY") != std::string::npos )
    {
    if ( this->m_FileType != BINARY )
      {
      this->m_FileType = BINARY;
#ifdef _WIN32
      inputFile.close();
      inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
      if ( !inputFile.is_open() )
        {
        itkExceptionMacro("Unable to open file\n" "inputFilename= " << this->m_FileName);
        }
#endif
      }
    }
  else
    {
    this->m_FileType = TYPENOTAPPLICABLE;
    itkExceptionMacro("Unknown File store type");
    }

  // Initialize number of cells
  this->m_NumberOfCells  = itk::NumericTraits<SizeValueType>::ZeroValue();
  this->m_CellBufferSize = itk::NumericTraits<SizeValueType>::ZeroValue();
  MetaDataDictionary & metaDic = this->GetMetaDataDictionary();

  // Searching the vtk file
  while ( !inputFile.eof() )
    {
    //  Read lines from input file
    std::getline(inputFile, line, '\n');
    StringType item;

    //  If there are points
    if ( line.find("POINTS") != std::string::npos )
      {
      // define string stream and put line into it
      StringStreamType ss;
      ss << line;

      // Get each item
      ss >> item; // "should be POINTS"

      // Get number of Points
      ss >> this->m_NumberOfPoints;
      this->m_PointDimension = 3; // vtk only support 3 dimensional points

      // Get point component type
      StringType pointType;
      ss >> pointType;
      this->m_PointComponentType= this->GetComponentTypeFromString(pointType);
      if (this->m_PointComponentType == UNKNOWNCOMPONENTTYPE)
        {
        itkExceptionMacro(<< "Unknown point component type");
        }

      this->m_UpdatePoints = true;
      }
    else if ( line.find("VERTICES") != std::string::npos )
      {
      // define string stream and put line into it
      StringStreamType ss;
      ss << line;

      // Get each item
      ss >> item; // should be "VERTICES"

      // Get number of Points
      unsigned int numberOfVertices;
      unsigned int numberOfVertexIndices;
      ss >> numberOfVertices;
      ss >> numberOfVertexIndices;
      this->m_NumberOfCells += numberOfVertices;
      this->m_CellBufferSize += numberOfVertexIndices;
      EncapsulateMetaData< unsigned int >(metaDic, "numberOfVertices", numberOfVertices);
      EncapsulateMetaData< unsigned int >(metaDic, "numberOfVertexIndices", numberOfVertexIndices);

      // Check whether numberOfVertices and numberOfVertexIndices are correct
      if ( numberOfVertices < 1 )
        {
        itkExceptionMacro("ERROR: numberOfVertices < 1\n "
                          << "numberOfVertices= " << numberOfVertices);
        }

      if ( numberOfVertexIndices < numberOfVertices )
        {
        itkExceptionMacro("ERROR: numberOfVertexIndices < numberOfVertices\n"
                          << "numberOfVertexIndices= " << numberOfVertexIndices << "\n"
                          << "numberOfVertices= " << numberOfVertices);
        }

      // Set cell component type
      this->m_CellComponentType = UINT;
      this->m_UpdateCells = true;
      }
    else if ( line.find("LINES") != std::string::npos )
      {
      // define string stream and put line into it
      StringStreamType ss;
      ss << line;

      // Get each item
      ss >> item; // should be "LINES"

      // Get number of Polylines
      unsigned int numberOfLines;
      unsigned int numberOfLineIndices;
      ss >> numberOfLines;
      ss >> numberOfLineIndices;
      this->m_NumberOfCells += numberOfLines;
      this->m_CellBufferSize += numberOfLineIndices;
      EncapsulateMetaData< unsigned int >(metaDic, "numberOfLines", numberOfLines);
      EncapsulateMetaData< unsigned int >(metaDic, "numberOfLineIndices", numberOfLineIndices);

      // Check whether numberOfPolylines and numberOfPolylineIndices are correct
      if ( numberOfLines < 1 )
        {
        itkExceptionMacro("ERROR: numberOfLines < 1\n "
                          << "numberOfLines= " << numberOfLines);
        }

      if ( numberOfLineIndices < numberOfLines )
        {
        itkExceptionMacro("ERROR: numberOfLineIndices < numberOfLines\n"
                          << "numberOfLineIndices= " << numberOfLineIndices << "\n"
                          << "numberOfLines= " << numberOfLines);
        }

      // Set cell component type
      this->m_CellComponentType = UINT;
      this->m_UpdateCells = true;
      }
    else if ( line.find("POLYGONS") != std::string::npos )
      {
      // define string stream and put line into it
      StringStreamType ss;
      ss << line;

      // Get each item
      ss >> item; // should be "POLYGONS"

      // Get number of Polygons
      unsigned int numberOfPolygons;
      unsigned int numberOfPolygonIndices;
      ss >> numberOfPolygons;
      ss >> numberOfPolygonIndices;
      this->m_NumberOfCells += numberOfPolygons;
      this->m_CellBufferSize += numberOfPolygonIndices;
      EncapsulateMetaData< unsigned int >(metaDic, "numberOfPolygons", numberOfPolygons);
      EncapsulateMetaData< unsigned int >(metaDic, "numberOfPolygonIndices", numberOfPolygonIndices);

      // Check whether numberOfPolygons and numberOfPolygonIndices are correct
      if ( numberOfPolygons < 1 )
        {
        itkExceptionMacro("ERROR: numberOfPolygons < 1\n "
                          << "numberOfPolygons= " << numberOfPolygons);
        }

      if ( numberOfPolygonIndices < numberOfPolygons )
        {
        itkExceptionMacro("ERROR: numberOfPolygonIndices < numberOfPolygons\n"
                          << "numberOfPolygonIndices= " << numberOfPolygonIndices << "\n"
                          << "numberOfPolygons= " << numberOfPolygons);
        }

      // Set cell component type
      this->m_CellComponentType = UINT;
      this->m_UpdateCells = true;
      }
    else if ( line.find("POINT_DATA") != std::string::npos )
      {
      // define string stream and put line into it
      StringStreamType pdss;
      pdss << line;

      // Get each item
      pdss >> item; // should be "POINT_DATA"

      // Get number of Point pixels
      pdss >> this->m_NumberOfPointPixels;

      // Continue to read line and get data type
      if ( !inputFile.eof() )
        {
        std::getline(inputFile, line, '\n');
        }
      else
        {
        itkExceptionMacro("UnExpected end of line while trying to read POINT_DATA");
        }

      this->m_UpdatePointData = true;

      //  Read point data pixel type
      if ( line.find("SCALARS") != std::string::npos )
        {
        // define string stream and put line into it
        StringStreamType scss;
        scss << line;

        // Get each item
        scss >> item; // should be "SCALARS"

        if ( item == "SCALARS" )
          {
          scss >> item;   // "point data name "

          // Get point data component type
          StringType pointDataComponentType;
          scss >> pointDataComponentType;

          // Set point pixel component type
          this->m_PointPixelComponentType = this->GetComponentTypeFromString(pointDataComponentType);
          if (this->m_PointPixelComponentType == UNKNOWNCOMPONENTTYPE)
            {
            itkExceptionMacro(<< "Unknown point data component type");
            }

          // Set point pixel type
          this->m_PointPixelType  = SCALAR;
          this->m_NumberOfPointPixelComponents = itk::NumericTraits< unsigned int >::OneValue();
          this->m_UpdatePointData = true;
          }
        }

      // Read point data pixel type
      if ( line.find("COLOR_SCALARS") != std::string::npos )
        {
        // define string stream and put line into it
        StringStreamType csss;
        csss << line;

        csss >> item;  // should be "COLOR_SCALARS"
        csss >> item;  // "point data name "
        csss >> this->m_NumberOfPointPixelComponents;

        // Set point pixel type
        this->m_PointPixelType = VARIABLELENGTHVECTOR;
        if ( this->m_FileType == ASCII )
          {
          this->m_PointPixelComponentType = FLOAT;
          }
        else
          {
          this->m_PointPixelComponentType = UCHAR;
          }

        this->m_UpdatePointData = true;
        }

      // Read point vector data
      if ( line.find("VECTORS") != std::string::npos || line.find("NORMALS") != std::string::npos )
        {
        // define string stream and put line into it
        StringStreamType vss;
        vss << line;

        // Get each item
        vss >> item; // should be "VECTORS"
        vss >> item; // "point data name "

        // Get point data component type
        StringType pointDataComponentType;
        vss >> pointDataComponentType;

        // Set point pixel component type
        this->m_PointPixelComponentType = this->GetComponentTypeFromString(pointDataComponentType);
        if (this->m_PointPixelComponentType == UNKNOWNCOMPONENTTYPE)
          {
          itkExceptionMacro(<< "Unknown point vector component type");
          }

        // Set point pixel type
        this->m_PointPixelType  = VECTOR;
        this->m_NumberOfPointPixelComponents = this->m_PointDimension;
        this->m_UpdatePointData = true;
        }
      if ( line.find("TENSORS") != std::string::npos )
        {
        // define string stream and put line into it
        StringStreamType tss;
        tss << line;

        // Get each item
        tss >> item; // should be "TENSORS"
        tss >> item; // "point data name "

        // Get point data component type
        StringType pointDataComponentType;
        tss >> pointDataComponentType;

        // Set point pixel component type
        this->m_PointPixelComponentType = this->GetComponentTypeFromString(pointDataComponentType);
        if (this->m_PointPixelComponentType == UNKNOWNCOMPONENTTYPE)
          {
          itkExceptionMacro(<< "Unknown point SYMMETRICSECONDRANKTENSOR component type");
          }

        // Set point pixel type
        this->m_PointPixelType  = SYMMETRICSECONDRANKTENSOR;
        this->m_NumberOfPointPixelComponents = this->m_PointDimension * ( this->m_PointDimension + 1 ) / 2;
        this->m_UpdatePointData = true;
        }
      }
    else if ( line.find("CELL_DATA") != std::string::npos )
      {
      // define string stream and put line into it
      StringStreamType cdss;
      cdss << line;

      // Get each item
      cdss >> item; // should be "CELL_DATA"

      // Get number of Point pixels
      cdss >> this->m_NumberOfCellPixels;

      // Continue to read line and get data type
      if ( !inputFile.eof() )
        {
        std::getline(inputFile, line, '\n');
        }
      else
        {
        itkExceptionMacro("UnExpected end of line while trying to read POINT_DATA");
        }

      this->m_UpdateCellData = true;

      // Read cell data pixel type
      if ( line.find("SCALARS") != std::string::npos )
        {
        // define string stream and put line into it
        StringStreamType sss;
        sss << line;

        // Get each item
        sss >> item; // should be "SCALARS"

        if ( item == "SCALARS" )
          {
          sss >> item;   // "cell data name "

          //  Get cell data component type
          StringType cellDataComponentType;
          sss >> cellDataComponentType;

          // Set point pixel component type
          this->m_CellPixelComponentType = this->GetComponentTypeFromString(cellDataComponentType);
          if (this->m_CellPixelComponentType == UNKNOWNCOMPONENTTYPE)
            {
            itkExceptionMacro(<< "Unknown cell component type");
            }

          // Set cell pixel type
          this->m_CellPixelType  = SCALAR;
          this->m_NumberOfCellPixelComponents = itk::NumericTraits< unsigned int >::OneValue();
          this->m_UpdateCellData = true;
          }
        }
      if ( line.find("COLOR_SCALARS") != std::string::npos )
        {
        // define string stream and put line into it
        StringStreamType csss;
        csss << line;

        csss >> item;  // should be "COLOR_SCALARS"
        csss >> item;  // "cell data name "
        csss >> this->m_NumberOfCellPixelComponents;

        // Set cell pixel type
        this->m_CellPixelType  = VARIABLELENGTHVECTOR;
        if ( this->m_FileType == ASCII )
          {
          this->m_CellPixelComponentType = FLOAT;
          }
        else
          {
          this->m_CellPixelComponentType = UCHAR;
          }

        this->m_UpdateCellData = true;
        }

      // Read cell vector data
      if ( line.find("VECTORS") != std::string::npos || line.find("NORMALS") != std::string::npos )
        {
        // define string stream and put line into it
        StringStreamType vss;
        vss << line;

        // Get each item
        vss >> item; // should be "VECTORS"
        vss >> item; // "cell data name "

        // Get cell data component type
        StringType cellDataComponentType;
        vss >> cellDataComponentType;

        // Set cell pixel component type
        this->m_CellPixelComponentType = this->GetComponentTypeFromString(cellDataComponentType);
        if (this->m_CellPixelComponentType == UNKNOWNCOMPONENTTYPE)
          {
          itkExceptionMacro(<< "Unknown cell normal component type");
          }

        // Set cell pixel type
        this->m_CellPixelType  = VECTOR;
        this->m_NumberOfCellPixelComponents = this->m_PointDimension;
        this->m_UpdateCellData = true;
        }
      if ( line.find("TENSORS") != std::string::npos )
        {
        // define string stream and put line into it
        StringStreamType tss;
        tss << line;

        // Get each item
        tss >> item; // should be "TENSORS"
        tss >> item; // "cell data name "

        // Get cell data component type
        StringType cellDataComponentType;
        tss >> cellDataComponentType;

        // Set cell pixel component type
        this->m_CellPixelComponentType = this->GetComponentTypeFromString(cellDataComponentType);
        if (this->m_CellPixelComponentType == UNKNOWNCOMPONENTTYPE)
          {
          itkExceptionMacro(<< "Unknown cell SYMMETRICSECONDRANKTENSOR component type");
          }

        // Set cell pixel type
        this->m_CellPixelType  = SYMMETRICSECONDRANKTENSOR;
        this->m_NumberOfCellPixelComponents = this->m_PointDimension * ( this->m_PointDimension + 1 ) / 2;
        this->m_UpdateCellData = true;
        }
      }
    }

  if ( this->m_CellBufferSize )
    {
    this->m_CellBufferSize += this->m_NumberOfCells;
    }

  inputFile.close();
}

void
VTKPolyDataMeshIO
::ReadPoints(void *buffer)
{
  // Read input file
  std::ifstream inputFile;

  if ( this->m_FileType == ASCII )
    {
    inputFile.open(this->m_FileName.c_str(), std::ios::in);
    }
  else if ( m_FileType == BINARY )
    {
    inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
    }

  // Test whether the file has been opened
  if ( !inputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n" "inputFilename= " << this->m_FileName);
    }

  // Read points according to filetype as ASCII or BINARY
  if ( this->m_FileType == ASCII )
    {
    switch ( this->m_PointComponentType )
      {
      case UCHAR:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< unsigned char * >( buffer ) );
        break;
        }
      case CHAR:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< char * >( buffer ) );
        break;
        }
      case USHORT:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< unsigned short * >( buffer ) );
        break;
        }
      case SHORT:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< short * >( buffer ) );
        break;
        }
      case UINT:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< unsigned int * >( buffer ) );
        break;
        }
      case INT:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< int * >( buffer ) );
        break;
        }
      case ULONG:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< unsigned long * >( buffer ) );
        break;
        }
      case LONG:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< long * >( buffer ) );
        break;
        }
      case ULONGLONG:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< unsigned long long * >( buffer ) );
        break;
        }
      case LONGLONG:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< long long * >( buffer ) );
        break;
        }
      case FLOAT:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< float * >( buffer ) );
        break;
        }
      case DOUBLE:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< double * >( buffer ) );
        break;
        }
      case LDOUBLE:
        {
        ReadPointsBufferAsASCII( inputFile, static_cast< long double * >( buffer ) );
        break;
        }
      default:
        {
        itkExceptionMacro(<< "Unknown point component type");
        }
      }
    }
  else if ( this->m_FileType == BINARY )
    {
    switch ( this->m_PointComponentType )
      {
      case UCHAR:
        {
        ReadPointsBufferAsBINARY( inputFile, static_cast< unsigned char * >( buffer ) );
        break;
        }
      case CHAR:
        {
        ReadPointsBufferAsBINARY( inputFile, static_cast< char * >( buffer ) );
        break;
        }
      case USHORT:
        {
        ReadPointsBufferAsBINARY( inputFile, static_cast< unsigned short * >( buffer ) );
        break;
        }
      case SHORT:
        {
        ReadPointsBufferAsBINARY( inputFile, static_cast< short * >( buffer ) );
        break;
        }
      case UINT:
        {
        ReadPointsBufferAsBINARY( inputFile, static_cast< unsigned int * >( buffer ) );
        break;
        }
      case INT:
        {
        ReadPointsBufferAsBINARY( inputFile, static_cast< int * >( buffer ) );
        break;
        }
      case ULONG:
        {
        ReadPointsBufferAsBINARY( inputFile, static_cast< unsigned long * >( buffer ) );
        break;
        }
      case LONG:
        {
        ReadPointsBufferAsBINARY( inputFile, static_cast< long * >( buffer ) );
        break;
        }
      case ULONGLONG:
        {
        SizeValueType       numberOfComponents = this->m_NumberOfPoints * this->m_PointDimension;
        unsigned long long *input = static_cast< unsigned long long * >( buffer );
        unsigned long *     data = new unsigned long[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< unsigned long >( input[ii] );
          }

        ReadPointsBufferAsBINARY(inputFile, data);
        delete[] data;
        break;
        }
      case LONGLONG:
        {
        SizeValueType numberOfComponents = this->m_NumberOfPoints * this->m_PointDimension;
        long long *   input = static_cast< long long * >( buffer );
        long *        data = new long[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< long >( input[ii] );
          }

        ReadPointsBufferAsBINARY(inputFile, data);
        delete[] data;
        break;
        }
      case FLOAT:
        {
        ReadPointsBufferAsBINARY( inputFile, static_cast< float * >( buffer ) );
        break;
        }
      case DOUBLE:
        {
        ReadPointsBufferAsBINARY( inputFile, static_cast< double * >( buffer ) );
        break;
        }
      case LDOUBLE:
        {
        SizeValueType numberOfComponents = this->m_NumberOfPoints * this->m_PointDimension;
        long double * input = static_cast< long double * >( buffer );
        double *      data = new double[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< double >( input[ii] );
          }

        ReadPointsBufferAsBINARY(inputFile, data);
        delete[] data;
        break;
        }
      default:
        {
        itkExceptionMacro(<< "Unknown point component type");
        }
      }
    }
  else
    {
    itkExceptionMacro(<< "Invalid output file type(not ASCII or BINARY)");
    }

  inputFile.close();
}

void
VTKPolyDataMeshIO
::ReadCells(void *buffer)
{
  // Read input file
  std::ifstream inputFile;

  if ( this->m_FileType == ASCII )
    {
    inputFile.open(this->m_FileName.c_str(), std::ios::in);
    }
  else if ( m_FileType == BINARY )
    {
    inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
    }

  // Test whether the file has been opened
  if ( !inputFile.is_open() )
    {
    itkExceptionMacro(<< "Unable to open file\n" "inputFilename= " << this->m_FileName);
    }

  // Read cells according to file type
  if ( this->m_FileType == ASCII )
    {
    ReadCellsBufferAsASCII(inputFile, buffer);
    }
  else if ( this->m_FileType == BINARY )
    {
    ReadCellsBufferAsBINARY(inputFile, buffer);
    }
  else
    {
    itkExceptionMacro(<< "Unkonw file type");
    }

  inputFile.close();
}

void VTKPolyDataMeshIO::ReadCellsBufferAsASCII(std::ifstream & inputFile, void *buffer)
{
  std::string   line;
  SizeValueType index = 0;
  unsigned int  numPoints; // number of point in each cell

  MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
  unsigned int *       data = static_cast< unsigned int * >( buffer );

  while ( !inputFile.eof() )
    {
    std::getline(inputFile, line, '\n');
    if ( line.find("VERTICES") != std::string::npos )
      {
      unsigned int numberOfVertices = 0;
      ExposeMetaData< unsigned int >(metaDic, "numberOfVertices", numberOfVertices);

      for ( unsigned int ii = 0; ii < numberOfVertices; ii++ )
        {
        inputFile >> numPoints;
        data[index++] = MeshIOBase::VERTEX_CELL;
        data[index++] = numPoints;
        for ( unsigned int jj = 0; jj < numPoints; jj++ )
          {
          inputFile >> data[index++];
          }
        }
      }
    else if ( line.find("LINES") != std::string::npos )
      {
      unsigned int numberOfLines = 0;
      ExposeMetaData< unsigned int >(metaDic, "numberOfLines", numberOfLines);

      for ( unsigned int ii = 0; ii < numberOfLines; ii++ )
        {
        inputFile >> numPoints;
        data[index++] = MeshIOBase::LINE_CELL;
        data[index++] = numPoints;
        for ( unsigned int jj = 0; jj < numPoints; jj++ )
          {
          inputFile >> data[index++];
          }
        }
      }
    else if ( line.find("POLYGONS") != std::string::npos )
      {
      unsigned int numberOfPolygons = 0;
      ExposeMetaData< unsigned int >(metaDic, "numberOfPolygons", numberOfPolygons);

      for ( unsigned int ii = 0; ii < numberOfPolygons; ii++ )
        {
        inputFile >> numPoints;

        data[index++] = MeshIOBase::POLYGON_CELL;
        data[index++] = numPoints;
        for ( unsigned int jj = 0; jj < numPoints; jj++ )
          {
          inputFile >> data[index++];
          }
        }
      }
    }
}

void
VTKPolyDataMeshIO
::ReadCellsBufferAsBINARY(std::ifstream & inputFile, void *buffer)
{
  if ( !this->m_CellBufferSize )
    {
    return;
    }

  unsigned int *inputBuffer = new unsigned int[this->m_CellBufferSize - this->m_NumberOfCells];
  void *        pv = inputBuffer;
  char *        startBuffer = static_cast< char * >( pv );
  unsigned int *outputBuffer = static_cast< unsigned int * >( buffer );

  std::string          line;
  MetaDataDictionary & metaDic = this->GetMetaDataDictionary();

  while ( !inputFile.eof() )
    {
    std::getline(inputFile, line, '\n');
    if ( line.find("VERTICES") != std::string::npos )
      {
      unsigned int numberOfVertices = 0;
      unsigned int numberOfVertexIndices = 0;
      ExposeMetaData< unsigned int >(metaDic, "numberOfVertices", numberOfVertices);
      ExposeMetaData< unsigned int >(metaDic, "numberOfVertexIndices", numberOfVertexIndices);
      inputFile.read( startBuffer, numberOfVertexIndices * sizeof( unsigned int ) );

      pv = startBuffer;
      unsigned int *data = static_cast< unsigned int * >( pv );
      if ( itk::ByteSwapper< unsigned int >::SystemIsLittleEndian() )
        {
        itk::ByteSwapper< unsigned int >::SwapRangeFromSystemToBigEndian(data, numberOfVertexIndices);
        }
      this->WriteCellsBuffer(data, outputBuffer, MeshIOBase::VERTEX_CELL, numberOfVertices);
      startBuffer += numberOfVertexIndices * sizeof( unsigned int );
      outputBuffer += ( numberOfVertexIndices + numberOfVertices ) * sizeof( unsigned int );
      }
    else if ( line.find("LINES") != std::string::npos )
      {
      unsigned int numberOfLines = 0;
      unsigned int numberOfLineIndices = 0;
      ExposeMetaData< unsigned int >(metaDic, "numberOfLines", numberOfLines);
      ExposeMetaData< unsigned int >(metaDic, "numberOfLineIndices", numberOfLineIndices);
      inputFile.read( startBuffer, numberOfLineIndices * sizeof( unsigned int ) );

      pv = startBuffer;
      unsigned int *data = static_cast< unsigned int * >( pv );
      if ( itk::ByteSwapper< unsigned int >::SystemIsLittleEndian() )
        {
        itk::ByteSwapper< unsigned int >::SwapRangeFromSystemToBigEndian(data, numberOfLineIndices);
        }
      this->WriteCellsBuffer(data, outputBuffer, MeshIOBase::LINE_CELL, numberOfLines);
      startBuffer += numberOfLineIndices * sizeof( unsigned int );
      outputBuffer += ( numberOfLineIndices + numberOfLines ) * sizeof( unsigned int );
      }
    else if ( line.find("POLYGONS") != std::string::npos )
      {
      unsigned int numberOfPolygons = 0;
      unsigned int numberOfPolygonIndices = 0;
      ExposeMetaData< unsigned int >(metaDic, "numberOfPolygons", numberOfPolygons);
      ExposeMetaData< unsigned int >(metaDic, "numberOfPolygonIndices", numberOfPolygonIndices);
      inputFile.read( startBuffer, numberOfPolygonIndices * sizeof( unsigned int ) );

      pv = startBuffer;
      unsigned int *data = static_cast< unsigned int * >( pv );
      if ( itk::ByteSwapper< unsigned int >::SystemIsLittleEndian() )
        {
        itk::ByteSwapper< unsigned int >::SwapRangeFromSystemToBigEndian(data, numberOfPolygonIndices);
        }

      this->WriteCellsBuffer(data, outputBuffer, MeshIOBase::POLYGON_CELL, numberOfPolygons);
      startBuffer += numberOfPolygonIndices * sizeof( unsigned int );
      outputBuffer += ( numberOfPolygonIndices + numberOfPolygons ) * sizeof( unsigned int );
      }
    }

  if ( this->m_CellBufferSize )
    {
    delete[] inputBuffer;
    }
}

void
VTKPolyDataMeshIO
::ReadPointData(void *buffer)
{
  // Read input file
  std::ifstream inputFile;

  if ( this->m_FileType == ASCII )
    {
    inputFile.open(this->m_FileName.c_str(), std::ios::in);
    }
  else if ( m_FileType == BINARY )
    {
    inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
    }

  // Test whether the file has been opened
  if ( !inputFile.is_open() )
    {
    itkExceptionMacro(<< "Unable to open file\n" "inputFilename= " << this->m_FileName);
    }

  // Read cells according to file type
  if ( this->m_FileType == ASCII )
    {
    switch ( this->m_PointPixelComponentType )
      {
      case UCHAR:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< unsigned char * >( buffer ) );
        break;
        }
      case CHAR:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< char * >( buffer ) );
        break;
        }
      case USHORT:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< unsigned short * >( buffer ) );
        break;
        }
      case SHORT:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< short * >( buffer ) );
        break;
        }
      case UINT:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< unsigned int * >( buffer ) );
        break;
        }
      case INT:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< int * >( buffer ) );
        break;
        }
      case ULONG:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< unsigned long * >( buffer ) );
        break;
        }
      case LONG:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< long * >( buffer ) );
        break;
        }
      case ULONGLONG:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< unsigned long long * >( buffer ) );
        break;
        }
      case LONGLONG:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< long long * >( buffer ) );
        break;
        }
      case FLOAT:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< float * >( buffer ) );
        break;
        }
      case DOUBLE:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< double * >( buffer ) );
        break;
        }
      case LDOUBLE:
        {
        ReadPointDataBufferAsASCII( inputFile, static_cast< long double * >( buffer ) );
        break;
        }
      default:
        {
        itkExceptionMacro(<< "Unknown point pixel component");
        }
      }
    }
  else if ( this->m_FileType == BINARY )
    {
    switch ( this->m_PointPixelComponentType )
      {
      case UCHAR:
        {
        ReadPointDataBufferAsBINARY( inputFile, static_cast< unsigned char * >( buffer ) );
        break;
        }
      case CHAR:
        {
        ReadPointDataBufferAsBINARY( inputFile, static_cast< char * >( buffer ) );
        break;
        }
      case USHORT:
        {
        ReadPointDataBufferAsBINARY( inputFile, static_cast< unsigned short * >( buffer ) );
        break;
        }
      case SHORT:
        {
        ReadPointDataBufferAsBINARY( inputFile, static_cast< short * >( buffer ) );
        break;
        }
      case UINT:
        {
        ReadPointDataBufferAsBINARY( inputFile, static_cast< unsigned int * >( buffer ) );
        break;
        }
      case INT:
        {
        ReadPointDataBufferAsBINARY( inputFile, static_cast< int * >( buffer ) );
        break;
        }
      case ULONG:
        {
        ReadPointDataBufferAsBINARY( inputFile, static_cast< unsigned long * >( buffer ) );
        break;
        }
      case LONG:
        {
        ReadPointDataBufferAsBINARY( inputFile, static_cast< long * >( buffer ) );
        break;
        }
      case ULONGLONG:
        {
        SizeValueType       numberOfComponents = this->m_NumberOfPointPixels * this->m_NumberOfPointPixelComponents;
        unsigned long long *input = static_cast< unsigned long long * >( buffer );
        unsigned long *     data = new unsigned long[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< unsigned long >( input[ii] );
          }

        ReadPointDataBufferAsBINARY(inputFile, data);
        delete[] data;
        break;
        }
      case LONGLONG:
        {
        SizeValueType numberOfComponents = this->m_NumberOfPointPixels * this->m_NumberOfPointPixelComponents;
        long long *   input = static_cast< long long * >( buffer );
        long *        data = new long[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< long >( input[ii] );
          }

        ReadPointDataBufferAsBINARY(inputFile, data);
        delete[] data;
        break;
        }
      case FLOAT:
        {
        ReadPointDataBufferAsBINARY( inputFile, static_cast< float * >( buffer ) );
        break;
        }
      case DOUBLE:
        {
        ReadPointDataBufferAsBINARY( inputFile, static_cast< double * >( buffer ) );
        break;
        }
      case LDOUBLE:
        {
        SizeValueType numberOfComponents = this->m_NumberOfPointPixels * this->m_NumberOfPointPixelComponents;
        long double * input = static_cast< long double * >( buffer );
        double *      data = new double[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< double >( input[ii] );
          }

        ReadPointDataBufferAsBINARY(inputFile, data);
        delete[] data;
        break;
        }
      default:
        {
        itkExceptionMacro(<< "Unknown point pixel component");
        }
      }
    }
  else
    {
    itkExceptionMacro(<< "Unkonw file type");
    }

  inputFile.close();
}

void
VTKPolyDataMeshIO
::ReadCellData(void *buffer)
{
  // Read input file
  std::ifstream inputFile;

  if ( this->m_FileType == ASCII )
    {
    inputFile.open(this->m_FileName.c_str(), std::ios::in);
    }
  else if ( m_FileType == BINARY )
    {
    inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
    }

  // Test whether the file has been opened
  if ( !inputFile.is_open() )
    {
    itkExceptionMacro(<< "Unable to open file\n" "inputFilename= " << this->m_FileName);
    }

  // Read cell data according file type
  if ( this->m_FileType == ASCII )
    {
    switch ( this->m_CellPixelComponentType )
      {
      case UCHAR:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< unsigned char * >( buffer ) );
        break;
        }
      case CHAR:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< char * >( buffer ) );
        break;
        }
      case USHORT:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< unsigned short * >( buffer ) );
        break;
        }
      case SHORT:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< short * >( buffer ) );
        break;
        }
      case UINT:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< unsigned int * >( buffer ) );
        break;
        }
      case INT:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< int * >( buffer ) );
        break;
        }
      case ULONG:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< unsigned long * >( buffer ) );
        break;
        }
      case LONG:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< long * >( buffer ) );
        break;
        }
      case ULONGLONG:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< unsigned long long * >( buffer ) );
        break;
        }
      case LONGLONG:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< long long * >( buffer ) );
        break;
        }
      case FLOAT:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< float * >( buffer ) );
        break;
        }
      case DOUBLE:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< double * >( buffer ) );
        break;
        }
      case LDOUBLE:
        {
        ReadCellDataBufferAsASCII( inputFile, static_cast< long double * >( buffer ) );
        break;
        }
      default:
        {
        itkExceptionMacro(<< "Unknown cell pixel component");
        }
      }
    }
  else if ( this->m_FileType == BINARY )
    {
    switch ( this->m_CellPixelComponentType )
      {
      case UCHAR:
        {
        ReadCellDataBufferAsBINARY( inputFile, static_cast< unsigned char * >( buffer ) );
        break;
        }
      case CHAR:
        {
        ReadCellDataBufferAsBINARY( inputFile, static_cast< char * >( buffer ) );
        break;
        }
      case USHORT:
        {
        ReadCellDataBufferAsBINARY( inputFile, static_cast< unsigned short * >( buffer ) );
        break;
        }
      case SHORT:
        {
        ReadCellDataBufferAsBINARY( inputFile, static_cast< short * >( buffer ) );
        break;
        }
      case UINT:
        {
        ReadCellDataBufferAsBINARY( inputFile, static_cast< unsigned int * >( buffer ) );
        break;
        }
      case INT:
        {
        ReadCellDataBufferAsBINARY( inputFile, static_cast< int * >( buffer ) );
        break;
        }
      case ULONG:
        {
        ReadCellDataBufferAsBINARY( inputFile, static_cast< unsigned long * >( buffer ) );
        break;
        }
      case LONG:
        {
        ReadCellDataBufferAsBINARY( inputFile, static_cast< long * >( buffer ) );
        break;
        }
      case ULONGLONG:
        {
        SizeValueType       numberOfComponents = this->m_NumberOfCellPixels * this->m_NumberOfCellPixelComponents;
        unsigned long long *input = static_cast< unsigned long long * >( buffer );
        unsigned long *     data = new unsigned long[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< unsigned long >( input[ii] );
          }

        ReadCellDataBufferAsBINARY(inputFile, data);
        delete[] data;
        break;
        }
      case LONGLONG:
        {
        SizeValueType numberOfComponents = this->m_NumberOfCellPixels * this->m_NumberOfCellPixelComponents;
        long long *   input = static_cast< long long * >( buffer );
        long *        data = new long[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< long >( input[ii] );
          }

        ReadCellDataBufferAsBINARY(inputFile, data);
        delete[] data;
        break;
        }
      case FLOAT:
        {
        ReadCellDataBufferAsBINARY( inputFile, static_cast< float * >( buffer ) );
        break;
        }
      case DOUBLE:
        {
        ReadCellDataBufferAsBINARY( inputFile, static_cast< double * >( buffer ) );
        break;
        }
      case LDOUBLE:
        {
        SizeValueType numberOfComponents = this->m_NumberOfCellPixels * this->m_NumberOfCellPixelComponents;
        long double * input = static_cast< long double * >( buffer );
        double *      data = new double[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< double >( input[ii] );
          }

        ReadCellDataBufferAsBINARY(inputFile, data);
        delete[] data;
        break;
        }
      default:
        {
        itkExceptionMacro(<< "Unknown cell pixel component");
        }
      }
    }
  else
    {
    itkExceptionMacro(<< "Unkonw file type");
    }

  inputFile.close();
}

void
VTKPolyDataMeshIO
::WriteMeshInformation()
{
  // Check file name
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro("No Input FileName");
    }

  // Define output file stream
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

  // Write VTK header
  outputFile << "# vtk DataFile Version 2.0" << "\n";
  outputFile << "File written by itkPolyDataMeshIO" << "\n";
  if ( m_FileType == ASCII )
    {
    outputFile << "ASCII" << "\n";
    }
  else if ( m_FileType == BINARY )
    {
    outputFile << "BINARY" << "\n";
    }
  else
    {
    itkExceptionMacro(<< "Invalid output file type (not ASCII or BINARY)");
    }

  outputFile << "DATASET POLYDATA" << "\n";

  outputFile.close();
}

void
VTKPolyDataMeshIO
::WritePoints(void *buffer)
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

  // Write file according to ASCII or BINARY
  if ( this->m_FileType == ASCII )
    {
    switch ( this->m_PointComponentType )
      {
      case UCHAR:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< unsigned char * >( buffer ), " unsigned_char");
        break;
        }
      case CHAR:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< char * >( buffer ), " char");
        break;
        }
      case USHORT:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< unsigned short * >( buffer ), " unsigned_short");
        break;
        }

      case SHORT:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< short * >( buffer ), " short");
        break;
        }

      case UINT:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< unsigned int * >( buffer ), " unsigned_int");
        break;
        }

      case INT:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< int * >( buffer ), " int");
        break;
        }
      case ULONG:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< unsigned long * >( buffer ), " unsigned_long");
        break;
        }
      case LONG:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< long * >( buffer ), " long");
        break;
        }
      case ULONGLONG:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< unsigned long long * >( buffer ), " unsigned_long");
        break;
        }
      case LONGLONG:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< long long * >( buffer ), " long");
        break;
        }
      case FLOAT:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< float * >( buffer ), " float");
        break;
        }
      case DOUBLE:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< double * >( buffer ), " double");
        break;
        }
      case LDOUBLE:
        {
        WritePointsBufferAsASCII(outputFile, static_cast< long double * >( buffer ), " double");
        break;
        }
      default:
        itkExceptionMacro(<< "Unknonwn point component type");
      }
    }
  else if ( this->m_FileType == BINARY )
    {
    switch ( this->m_PointComponentType )
      {
      case UCHAR:
        {
        WritePointsBufferAsBINARY(outputFile, static_cast< unsigned char * >( buffer ), " unsigned_char");
        break;
        }
      case CHAR:
        {
        WritePointsBufferAsBINARY(outputFile, static_cast< char * >( buffer ), " char");
        break;
        }
      case USHORT:
        {
        WritePointsBufferAsBINARY(outputFile, static_cast< unsigned short * >( buffer ), " unsigned_short");
        break;
        }

      case SHORT:
        {
        WritePointsBufferAsBINARY(outputFile, static_cast< short * >( buffer ), " short");
        break;
        }

      case UINT:
        {
        WritePointsBufferAsBINARY(outputFile, static_cast< unsigned int * >( buffer ), " unsigned_int");
        break;
        }

      case INT:
        {
        WritePointsBufferAsBINARY(outputFile, static_cast< int * >( buffer ), " int");
        break;
        }
      case ULONG:
        {
        WritePointsBufferAsBINARY(outputFile, static_cast< unsigned long * >( buffer ), " unsigned_long");
        break;
        }
      case LONG:
        {
        WritePointsBufferAsBINARY(outputFile, static_cast< long * >( buffer ), " long");
        break;
        }
      case ULONGLONG:
        {
        SizeValueType       numberOfComponents = this->m_NumberOfPoints * this->m_PointDimension;
        unsigned long long *input = static_cast< unsigned long long * >( buffer );
        unsigned long *     data = new unsigned long[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< unsigned long >( input[ii] );
          }
        WritePointsBufferAsBINARY(outputFile, data, " unsigned_long");
        delete[] data;
        break;
        }
      case LONGLONG:
        {
        SizeValueType numberOfComponents = this->m_NumberOfPoints * this->m_PointDimension;
        long long *   input = static_cast< long long * >( buffer );
        long *        data = new long[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< long >( input[ii] );
          }
        WritePointsBufferAsBINARY(outputFile, data, " long");
        delete[] data;
        break;
        }
      case FLOAT:
        {
        WritePointsBufferAsBINARY(outputFile, static_cast< float * >( buffer ), " float");
        break;
        }
      case DOUBLE:
        {
        WritePointsBufferAsBINARY(outputFile, static_cast< double * >( buffer ), " double");
        break;
        }
      case LDOUBLE:
        {
        SizeValueType numberOfComponents = this->m_NumberOfPoints * this->m_PointDimension;
        long double * input = static_cast< long double * >( buffer );
        double *      data = new double[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< double >( input[ii] );
          }
        WritePointsBufferAsBINARY(outputFile, data, " double");
        delete[] data;
        break;
        }
      default:
        itkExceptionMacro(<< "Unknonwn point component type");
      }
    }
  else
    {
    itkExceptionMacro(<< "Invalid output file type(not ASCII or BINARY)");
    }

  outputFile.close();
}

void
VTKPolyDataMeshIO
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

  // Write file according to ASCII or BINARY
  if ( this->m_FileType == ASCII )
    {
    switch ( this->m_CellComponentType )
      {
      case UCHAR:
        {
        UpdateCellInformation( static_cast< unsigned char * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< unsigned char * >( buffer ) );
        break;
        }
      case CHAR:
        {
        UpdateCellInformation( static_cast< char * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< char * >( buffer ) );
        break;
        }
      case USHORT:
        {
        UpdateCellInformation( static_cast< unsigned short * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< unsigned short * >( buffer ) );
        break;
        }

      case SHORT:
        {
        UpdateCellInformation( static_cast< short * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< short * >( buffer ) );
        break;
        }

      case UINT:
        {
        UpdateCellInformation( static_cast< unsigned int * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< unsigned int * >( buffer ) );
        break;
        }

      case INT:
        {
        UpdateCellInformation( static_cast< int * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< int * >( buffer ) );
        break;
        }
      case ULONG:
        {
        UpdateCellInformation( static_cast< unsigned long * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< unsigned long * >( buffer ) );
        break;
        }
      case LONG:
        {
        UpdateCellInformation( static_cast< long * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< long * >( buffer ) );
        break;
        }
      case ULONGLONG:
        {
        UpdateCellInformation( static_cast< unsigned long long * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< unsigned long long * >( buffer ) );
        break;
        }
      case LONGLONG:
        {
        UpdateCellInformation( static_cast< long long * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< long long * >( buffer ) );
        break;
        }
      case FLOAT:
        {
        UpdateCellInformation( static_cast< float * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< float * >( buffer ) );
        break;
        }
      case DOUBLE:
        {
        UpdateCellInformation( static_cast< double * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< double * >( buffer ) );
        break;
        }
      case LDOUBLE:
        {
        UpdateCellInformation( static_cast< long double * >( buffer ) );
        WriteCellsBufferAsASCII( outputFile, static_cast< long double * >( buffer ) );
        break;
        }
      default:
        itkExceptionMacro(<< "Unknonwn cell component type");
      }
    }
  else if ( this->m_FileType == BINARY )
    {
    switch ( this->m_CellComponentType )
      {
      case UCHAR:
        {
        UpdateCellInformation( static_cast< unsigned char * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< unsigned char * >( buffer ) );
        break;
        }
      case CHAR:
        {
        UpdateCellInformation( static_cast< char * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< char * >( buffer ) );
        break;
        }
      case USHORT:
        {
        UpdateCellInformation( static_cast< unsigned short * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< unsigned short * >( buffer ) );
        break;
        }

      case SHORT:
        {
        UpdateCellInformation( static_cast< short * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< short * >( buffer ) );
        break;
        }

      case UINT:
        {
        UpdateCellInformation( static_cast< unsigned int * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< unsigned int * >( buffer ) );
        break;
        }

      case INT:
        {
        UpdateCellInformation( static_cast< int * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< int * >( buffer ) );
        break;
        }
      case ULONG:
        {
        UpdateCellInformation( static_cast< unsigned long * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< unsigned long * >( buffer ) );
        break;
        }
      case LONG:
        {
        UpdateCellInformation( static_cast< long * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< long * >( buffer ) );
        break;
        }
      case ULONGLONG:
        {
        UpdateCellInformation( static_cast< unsigned long long * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< unsigned long long * >( buffer ) );
        break;
        }
      case LONGLONG:
        {
        UpdateCellInformation( static_cast< long long * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< long long * >( buffer ) );
        break;
        }
      case FLOAT:
        {
        UpdateCellInformation( static_cast< float * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< float * >( buffer ) );
        break;
        }
      case DOUBLE:
        {
        UpdateCellInformation( static_cast< double * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< double * >( buffer ) );
        break;
        }
      case LDOUBLE:
        {
        UpdateCellInformation( static_cast< long double * >( buffer ) );
        WriteCellsBufferAsBINARY( outputFile, static_cast< long double * >( buffer ) );
        break;
        }
      default:
        itkExceptionMacro(<< "Unknonwn cell component type");
      }
    }
  else
    {
    itkExceptionMacro(<< "Invalid output file type(not ASCII or BINARY)");
    }

  outputFile.close();
}

void
VTKPolyDataMeshIO
::WritePointData(void *buffer)
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

  // Write point data according to ASCII or BINARY
  if ( this->m_FileType == ASCII )
    {
    switch ( this->m_PointPixelComponentType )
      {
      case UCHAR:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< unsigned char * >( buffer ), " unsigned_char");
        break;
        }
      case CHAR:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< char * >( buffer ), " char");
        break;
        }
      case USHORT:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< unsigned short * >( buffer ), " unsigned_short");
        break;
        }

      case SHORT:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< short * >( buffer ), " short");
        break;
        }

      case UINT:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< unsigned int * >( buffer ), " unsigned_int");
        break;
        }
      case INT:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< int * >( buffer ), " int");
        break;
        }
      case ULONG:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< unsigned long * >( buffer ), " unsigned_long");
        break;
        }
      case LONG:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< long * >( buffer ), " long");
        break;
        }
      case ULONGLONG:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< unsigned long long * >( buffer ), " unsigned_long");
        break;
        }
      case LONGLONG:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< long long * >( buffer ), " long");
        break;
        }
      case FLOAT:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< float * >( buffer ), " float");
        break;
        }
      case DOUBLE:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< double * >( buffer ), " double");
        break;
        }
      case LDOUBLE:
        {
        WritePointDataBufferAsASCII(outputFile, static_cast< long double * >( buffer ), " double");
        break;
        }
      default:
        itkExceptionMacro(<< "Unknonwn point pixel component type");
      }
    }
  else if ( this->m_FileType == BINARY )
    {
    switch ( this->m_PointPixelComponentType )
      {
      case UCHAR:
        {
        WritePointDataBufferAsBINARY(outputFile, static_cast< unsigned char * >( buffer ), " unsigned_char");
        break;
        }
      case CHAR:
        {
        WritePointDataBufferAsBINARY(outputFile, static_cast< char * >( buffer ), " char");
        break;
        }
      case USHORT:
        {
        WritePointDataBufferAsBINARY(outputFile, static_cast< unsigned short * >( buffer ), " unsigned_short");
        break;
        }

      case SHORT:
        {
        WritePointDataBufferAsBINARY(outputFile, static_cast< short * >( buffer ), " short");
        break;
        }

      case UINT:
        {
        WritePointDataBufferAsBINARY(outputFile, static_cast< unsigned int * >( buffer ), " unsigned_int");
        break;
        }

      case INT:
        {
        WritePointDataBufferAsBINARY(outputFile, static_cast< int * >( buffer ), " int");
        break;
        }
      case ULONG:
        {
        WritePointDataBufferAsBINARY(outputFile, static_cast< unsigned long * >( buffer ), " unsigned_long");
        break;
        }
      case LONG:
        {
        WritePointDataBufferAsBINARY(outputFile, static_cast< long * >( buffer ), " long");
        break;
        }
      case ULONGLONG:
        {
        SizeValueType       numberOfComponents = this->m_NumberOfPointPixels * this->m_NumberOfPointPixelComponents;
        unsigned long long *input = static_cast< unsigned long long * >( buffer );
        unsigned long *     data = new unsigned long[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< unsigned long >( input[ii] );
          }

        WritePointDataBufferAsBINARY(outputFile, data, " unsigned_long");
        delete[] data;
        break;
        }
      case LONGLONG:
        {
        SizeValueType numberOfComponents = this->m_NumberOfPointPixels * this->m_NumberOfPointPixelComponents;
        long long *   input = static_cast< long long * >( buffer );
        long *        data = new long[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< long >( input[ii] );
          }

        WritePointDataBufferAsBINARY(outputFile, data, " long");
        delete[] data;
        break;
        }
      case FLOAT:
        {
        WritePointDataBufferAsBINARY(outputFile, static_cast< float * >( buffer ), " float");
        break;
        }
      case DOUBLE:
        {
        WritePointDataBufferAsBINARY(outputFile, static_cast< double * >( buffer ), " double");
        break;
        }
      case LDOUBLE:
        {
        SizeValueType numberOfComponents = this->m_NumberOfPointPixels * this->m_NumberOfPointPixelComponents;
        long double * input = static_cast< long double * >( buffer );
        double *      data = new double[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< double >( input[ii] );
          }

        WritePointDataBufferAsBINARY(outputFile, data, " double");
        delete[] data;
        break;
        }
      default:
        itkExceptionMacro(<< "Unknonwn point pixel component type");
      }
    }
  else
    {
    itkExceptionMacro(<< "Invalid output file type(not ASCII or BINARY)");
    }

  outputFile.close();
}

void
VTKPolyDataMeshIO
::WriteCellData(void *buffer)
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

  // Write cell data according to ASCII or BINARY
  if ( this->m_FileType == ASCII )
    {
    switch ( this->m_CellPixelComponentType )
      {
      case UCHAR:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< unsigned char * >( buffer ), " unsigned_char");
        break;
        }
      case CHAR:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< char * >( buffer ), " char");
        break;
        }
      case USHORT:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< unsigned short * >( buffer ), " unsigned_short");
        break;
        }

      case SHORT:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< short * >( buffer ), " short");
        break;
        }

      case UINT:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< unsigned int * >( buffer ), " unsigned_int");
        break;
        }

      case INT:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< int * >( buffer ), " int");
        break;
        }
      case ULONG:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< unsigned long * >( buffer ), " unsigned_long");
        break;
        }
      case LONG:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< long * >( buffer ), " long");
        break;
        }
      case ULONGLONG:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< unsigned long long * >( buffer ), " unsigned_long");
        break;
        }
      case LONGLONG:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< long long * >( buffer ), " long");
        break;
        }
      case FLOAT:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< float * >( buffer ), " float");
        break;
        }
      case DOUBLE:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< double * >( buffer ), " double");
        break;
        }
      case LDOUBLE:
        {
        WriteCellDataBufferAsASCII(outputFile, static_cast< long double * >( buffer ), " double");
        break;
        }
      default:
        itkExceptionMacro(<< "Unknonwn cell pixel component type");
      }
    }
  else if ( this->m_FileType == BINARY )
    {
    switch ( this->m_CellPixelComponentType )
      {
      case UCHAR:
        {
        WriteCellDataBufferAsBINARY(outputFile, static_cast< unsigned char * >( buffer ), " unsigned_char");
        break;
        }
      case CHAR:
        {
        WriteCellDataBufferAsBINARY(outputFile, static_cast< char * >( buffer ), " char");
        break;
        }
      case USHORT:
        {
        WriteCellDataBufferAsBINARY(outputFile, static_cast< unsigned short * >( buffer ), " unsigned_short");
        break;
        }

      case SHORT:
        {
        WriteCellDataBufferAsBINARY(outputFile, static_cast< short * >( buffer ), " short");
        break;
        }

      case UINT:
        {
        WriteCellDataBufferAsBINARY(outputFile, static_cast< unsigned int * >( buffer ), " unsigned_int");
        break;
        }

      case INT:
        {
        WriteCellDataBufferAsBINARY(outputFile, static_cast< int * >( buffer ), " int");
        break;
        }
      case ULONG:
        {
        WriteCellDataBufferAsBINARY(outputFile, static_cast< unsigned long * >( buffer ), " unsigned_long");
        break;
        }
      case LONG:
        {
        WriteCellDataBufferAsBINARY(outputFile, static_cast< long * >( buffer ), " long");
        break;
        }
      case ULONGLONG:
        {
        SizeValueType       numberOfComponents = this->m_NumberOfCellPixels * this->m_NumberOfCellPixelComponents;
        unsigned long long *input = static_cast< unsigned long long * >( buffer );
        unsigned long *     data = new unsigned long[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< unsigned long >( input[ii] );
          }

        WriteCellDataBufferAsBINARY(outputFile, data, " unsigned_long");
        delete[] data;
        break;
        }
      case LONGLONG:
        {
        SizeValueType numberOfComponents = this->m_NumberOfCellPixels * this->m_NumberOfCellPixelComponents;
        long long *   input = static_cast< long long * >( buffer );
        long *        data = new long[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< long >( input[ii] );
          }

        WriteCellDataBufferAsBINARY(outputFile, data, " long");
        delete[] data;
        break;
        }
      case FLOAT:
        {
        WriteCellDataBufferAsBINARY(outputFile, static_cast< float * >( buffer ), " float");
        break;
        }
      case DOUBLE:
        {
        WriteCellDataBufferAsBINARY(outputFile, static_cast< double * >( buffer ), " double");
        break;
        }
      case LDOUBLE:
        {
        SizeValueType numberOfComponents = this->m_NumberOfCellPixels * this->m_NumberOfCellPixelComponents;
        long double * input = static_cast< long double * >( buffer );
        double *      data = new double[numberOfComponents];
        for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
          {
          data[ii] = static_cast< double >( input[ii] );
          }

        WriteCellDataBufferAsBINARY(outputFile, data, " double");
        delete[] data;
        break;
        }
      default:
        itkExceptionMacro(<< "Unknonwn cell pixel component type");
      }
    }
  else
    {
    itkExceptionMacro(<< "Invalid output file type(not ASCII or BINARY)");
    }

  outputFile.close();
}

void
VTKPolyDataMeshIO
::Write()
{
}

void
VTKPolyDataMeshIO
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  const MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
  unsigned int               value = 0;

  if ( ExposeMetaData< unsigned int >(metaDic, "numberOfVertices", value) )
    {
    os << indent << "number of vertices : " << value << std::endl;
    }

  if ( ExposeMetaData< unsigned int >(metaDic, "numberOfLines", value) )
    {
    os << indent << "number of lines : " << value << std::endl;
    }

  if ( ExposeMetaData< unsigned int >(metaDic, "numberOfPolygons", value) )
    {
    os << indent << "number of polygons : " << value << std::endl;
    }

  StringType dataName;
  if ( ExposeMetaData< StringType >(metaDic, "pointScalarDataName", dataName) )
    {
    os << indent << "pointScalarDataName : " << dataName << std::endl;
    }

  if ( ExposeMetaData< StringType >(metaDic, "pointVectorDataName", dataName) )
    {
    os << indent << "pointVectorDataName : " << dataName << std::endl;
    }

  if ( ExposeMetaData< StringType >(metaDic, "pointTensorDataName", dataName) )
    {
    os << indent << "pointTensorDataName : " << dataName << std::endl;
    }

  if ( ExposeMetaData< StringType >(metaDic, "cellScalarDataName", dataName) )
    {
    os << indent << "cellScalarDataName : " << dataName << std::endl;
    }

  if ( ExposeMetaData< StringType >(metaDic, "cellVectorDataName", dataName) )
    {
    os << indent << "cellVectorDataName : " << dataName << std::endl;
    }

  if ( ExposeMetaData< StringType >(metaDic, "cellTensorDataName", dataName) )
    {
    os << indent << "cellTensorDataName : " << dataName << std::endl;
    }
}
} // end of namespace itk
