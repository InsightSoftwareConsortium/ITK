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

#include "itkVTKPolyDataMeshIO.h"

#include <itksys/SystemTools.hxx>
#include <fstream>

namespace itk
{
// Constructor
VTKPolyDataMeshIO ::VTKPolyDataMeshIO()
{
  this->AddSupportedWriteExtension(".vtk");
  this->m_ByteOrder = IOByteOrderEnum::BigEndian;

  MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
  EncapsulateMetaData<StringType>(metaDic, "pointScalarDataName", "PointScalarData");
  EncapsulateMetaData<StringType>(metaDic, "pointColorScalarDataName", "PointColorScalarData");
  EncapsulateMetaData<StringType>(metaDic, "pointVectorDataName", "PointVectorData");
  EncapsulateMetaData<StringType>(metaDic, "pointTensorDataName", "PointTensorData");
  EncapsulateMetaData<StringType>(metaDic, "cellScalarDataName", "CellScalarData");
  EncapsulateMetaData<StringType>(metaDic, "cellColorScalarDataName", "CellColorScalarData");
  EncapsulateMetaData<StringType>(metaDic, "cellVectorDataName", "CellVectorData");
  EncapsulateMetaData<StringType>(metaDic, "cellTensorDataName", "CellTensorData");
}

VTKPolyDataMeshIO ::~VTKPolyDataMeshIO() = default;


int
VTKPolyDataMeshIO ::GetNextLine(std::ifstream & ifs, std::string & line, bool lowerCase, SizeValueType count)
{
  // The terminal condition for this recursive calls
  if (count > 5)
  {
    itkExceptionMacro(<< "Error of GetNextLine due to consecutive 5 empty lines in the given .*vtk file ");
  }

  // Get a next line from a given *.vtk file
  std::getline(ifs, line);

  // Check the End-of-File of the file
  if (ifs.eof())
  {
    itkExceptionMacro(<< "Premature EOF in reading a line");
  }

  // Convert characters of the line to lowercas
  if (lowerCase)
  {
    std::transform(line.begin(), line.end(), line.begin(), ::tolower);
  }

  // Check an empty line with the size of a read line from *.vtk file
  if (line.empty())
  {
    return GetNextLine(ifs, line, lowerCase, ++count);
  }

  return 1;
}


bool
VTKPolyDataMeshIO ::CanReadFile(const char * fileName)
{
  if (!itksys::SystemTools::FileExists(fileName, true))
  {
    return false;
  }

  if (itksys::SystemTools::GetFilenameLastExtension(fileName) != ".vtk")
  {
    return false;
  }

  std::ifstream file;
  file.open(fileName, std::ios::in);
  if (!file.is_open())
  {
    return false;
  }

  // Check to see if it is a vtk PolyData file
  std::string line;
  this->GetNextLine(file, line);
  this->GetNextLine(file, line);
  this->GetNextLine(file, line);
  this->GetNextLine(file, line);

  if (line.find("polydata") < line.length())
  {
    return true;
  }
  else
  {
    return false;
  }
}

bool
VTKPolyDataMeshIO ::CanWriteFile(const char * fileName)
{
  if (itksys::SystemTools::GetFilenameLastExtension(fileName) != ".vtk")
  {
    return false;
  }

  return true;
}

IOComponentEnum
VTKPolyDataMeshIO::GetComponentTypeFromString(const std::string & pointType)
{
  IOComponentEnum compType;
  if (pointType == "unsigned_char")
  {
    compType = IOComponentEnum::UCHAR;
  }
  else if (pointType == "char")
  {
    compType = IOComponentEnum::CHAR;
  }
  else if (pointType == "unsigned_short")
  {
    compType = IOComponentEnum::USHORT;
  }
  else if (pointType == "short")
  {
    compType = IOComponentEnum::SHORT;
  }
  else if (pointType == "unsigned_int")
  {
    compType = IOComponentEnum::UINT;
  }
  else if (pointType == "int")
  {
    compType = IOComponentEnum::INT;
  }
  else if (pointType == "unsigned_long")
  {
    compType = IOComponentEnum::ULONG;
  }
  else if (pointType == "long")
  {
    compType = IOComponentEnum::LONG;
  }
  else if (pointType == "unsigned_long_long" || pointType == "vtktypeuint64")
  {
    compType = IOComponentEnum::ULONGLONG;
  }
  else if (pointType == "long_long" || pointType == "vtktypeint64")
  {
    compType = IOComponentEnum::LONGLONG;
  }
  else if (pointType == "float")
  {
    compType = IOComponentEnum::FLOAT;
  }
  else if (pointType == "double")
  {
    compType = IOComponentEnum::DOUBLE;
  }
  else if (pointType == "long_double")
  {
    compType = IOComponentEnum::LDOUBLE; // not supported by standard vtk format
  }
  else
  {
    compType = IOComponentEnum::UNKNOWNCOMPONENTTYPE;
  }
  return compType;
}

void
VTKPolyDataMeshIO ::ReadMeshInformation()
{
  std::ifstream inputFile;

  // Use default filetype
  if (this->m_FileType == IOFileEnum::ASCII)
  {
    inputFile.open(this->m_FileName.c_str(), std::ios::in);
  }
  else if (m_FileType == IOFileEnum::BINARY)
  {
    inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
  }

  if (!inputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "inputFilename= "
                      << this->m_FileName);
  }

  unsigned int numLine = 0;
  std::string  line;

  // Read vtk file header (the first 3 lines)
  while (!inputFile.eof() && numLine < 3)
  {
    std::getline(inputFile, line, '\n');
    ++numLine;
  }

  if (line.find("ASCII") != std::string::npos)
  {
    if (this->m_FileType != IOFileEnum::ASCII)
    {
      this->m_FileType = IOFileEnum::ASCII;
#ifdef _WIN32
      inputFile.close();
      inputFile.open(this->m_FileName.c_str(), std::ios::in);
      if (!inputFile.is_open())
      {
        itkExceptionMacro("Unable to open file\n"
                          "inputFilename= "
                          << this->m_FileName);
      }
#endif
    }
  }
  else if (line.find("BINARY") != std::string::npos)
  {
    if (this->m_FileType != IOFileEnum::BINARY)
    {
      this->m_FileType = IOFileEnum::BINARY;
#ifdef _WIN32
      inputFile.close();
      inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
      if (!inputFile.is_open())
      {
        itkExceptionMacro("Unable to open file\n"
                          "inputFilename= "
                          << this->m_FileName);
      }
#endif
    }
  }
  else
  {
    this->m_FileType = IOFileEnum::TYPENOTAPPLICABLE;
    itkExceptionMacro("Unknown File store type");
  }

  // Initialize number of cells
  this->m_NumberOfCells = itk::NumericTraits<SizeValueType>::ZeroValue();
  this->m_CellBufferSize = itk::NumericTraits<SizeValueType>::ZeroValue();
  MetaDataDictionary & metaDic = this->GetMetaDataDictionary();

  // Searching the vtk file
  while (!inputFile.eof())
  {
    //  Read lines from input file
    std::getline(inputFile, line, '\n');
    StringType item;

    //  If there are points
    if (line.find("POINTS") != std::string::npos)
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
      this->m_PointComponentType = this->GetComponentTypeFromString(pointType);
      if (this->m_PointComponentType == IOComponentEnum::UNKNOWNCOMPONENTTYPE)
      {
        itkExceptionMacro(<< "Unknown point component type");
      }

      this->m_UpdatePoints = true;
    }
    else if (line.find("VERTICES") != std::string::npos)
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
      EncapsulateMetaData<unsigned int>(metaDic, "numberOfVertices", numberOfVertices);
      EncapsulateMetaData<unsigned int>(metaDic, "numberOfVertexIndices", numberOfVertexIndices);

      // Check whether numberOfVertices and numberOfVertexIndices are correct
      if (numberOfVertices < 1)
      {
        itkExceptionMacro("ERROR: numberOfVertices < 1\n "
                          << "numberOfVertices= " << numberOfVertices);
      }

      if (numberOfVertexIndices < numberOfVertices)
      {
        itkExceptionMacro("ERROR: numberOfVertexIndices < numberOfVertices\n"
                          << "numberOfVertexIndices= " << numberOfVertexIndices << "\n"
                          << "numberOfVertices= " << numberOfVertices);
      }

      // Set cell component type
      this->m_CellComponentType = IOComponentEnum::UINT;
      this->m_UpdateCells = true;
    }
    else if (line.find("LINES") != std::string::npos)
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
      EncapsulateMetaData<unsigned int>(metaDic, "numberOfLines", numberOfLines);
      EncapsulateMetaData<unsigned int>(metaDic, "numberOfLineIndices", numberOfLineIndices);

      // Check whether numberOfPolylines and numberOfPolylineIndices are correct
      if (numberOfLines < 1)
      {
        itkExceptionMacro("ERROR: numberOfLines < 1\n "
                          << "numberOfLines= " << numberOfLines);
      }

      if (numberOfLineIndices < numberOfLines)
      {
        itkExceptionMacro("ERROR: numberOfLineIndices < numberOfLines\n"
                          << "numberOfLineIndices= " << numberOfLineIndices << "\n"
                          << "numberOfLines= " << numberOfLines);
      }

      // Set cell component type
      this->m_CellComponentType = IOComponentEnum::UINT;
      this->m_UpdateCells = true;
    }
    else if (line.find("POLYGONS") != std::string::npos)
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
      EncapsulateMetaData<unsigned int>(metaDic, "numberOfPolygons", numberOfPolygons);
      EncapsulateMetaData<unsigned int>(metaDic, "numberOfPolygonIndices", numberOfPolygonIndices);

      // Check whether numberOfPolygons and numberOfPolygonIndices are correct
      if (numberOfPolygons < 1)
      {
        itkExceptionMacro("ERROR: numberOfPolygons < 1\n "
                          << "numberOfPolygons= " << numberOfPolygons);
      }

      if (numberOfPolygonIndices < numberOfPolygons)
      {
        itkExceptionMacro("ERROR: numberOfPolygonIndices < numberOfPolygons\n"
                          << "numberOfPolygonIndices= " << numberOfPolygonIndices << "\n"
                          << "numberOfPolygons= " << numberOfPolygons);
      }

      // Set cell component type
      this->m_CellComponentType = IOComponentEnum::UINT;
      this->m_UpdateCells = true;
    }
    else if (line.find("POINT_DATA") != std::string::npos)
    {
      // define string stream and put line into it
      StringStreamType pdss;
      pdss << line;

      // Get each item
      pdss >> item; // should be "POINT_DATA"

      // Get number of Point pixels
      pdss >> this->m_NumberOfPointPixels;

      // Continue to read line and get data type
      if (!inputFile.eof())
      {
        std::getline(inputFile, line, '\n');
      }
      else
      {
        itkExceptionMacro("UnExpected end of line while trying to read POINT_DATA");
      }

      this->m_UpdatePointData = true;

      //  Read point data pixel type
      if (line.find("SCALARS") != std::string::npos)
      {
        // define string stream and put line into it
        StringStreamType scss;
        scss << line;

        // Get each item
        scss >> item; // should be "SCALARS"

        if (item == "SCALARS")
        {
          scss >> item; // "point data name "

          // Get point data component type
          StringType pointDataComponentType;
          scss >> pointDataComponentType;

          // Set point pixel component type
          this->m_PointPixelComponentType = this->GetComponentTypeFromString(pointDataComponentType);
          if (this->m_PointPixelComponentType == IOComponentEnum::UNKNOWNCOMPONENTTYPE)
          {
            itkExceptionMacro(<< "Unknown point data component type");
          }

          // Set point pixel type
          this->m_PointPixelType = IOPixelEnum::SCALAR;
          this->m_NumberOfPointPixelComponents = itk::NumericTraits<unsigned int>::OneValue();
          this->m_UpdatePointData = true;
        }
      }

      // Read point data pixel type
      if (line.find("COLOR_SCALARS") != std::string::npos)
      {
        // define string stream and put line into it
        StringStreamType csss;
        csss << line;

        csss >> item; // should be "COLOR_SCALARS"
        csss >> item; // "point data name "
        csss >> this->m_NumberOfPointPixelComponents;

        // Set point pixel type
        this->m_PointPixelType = IOPixelEnum::VARIABLELENGTHVECTOR;
        if (this->m_FileType == IOFileEnum::ASCII)
        {
          this->m_PointPixelComponentType = IOComponentEnum::FLOAT;
        }
        else
        {
          this->m_PointPixelComponentType = IOComponentEnum::UCHAR;
        }

        this->m_UpdatePointData = true;
      }

      // Read point vector data
      if (line.find("VECTORS") != std::string::npos || line.find("NORMALS") != std::string::npos)
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
        if (this->m_PointPixelComponentType == IOComponentEnum::UNKNOWNCOMPONENTTYPE)
        {
          itkExceptionMacro(<< "Unknown point vector component type");
        }

        // Set point pixel type
        this->m_PointPixelType = IOPixelEnum::VECTOR;
        this->m_NumberOfPointPixelComponents = this->m_PointDimension;
        this->m_UpdatePointData = true;
      }
      if (line.find("TENSORS") != std::string::npos)
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
        if (this->m_PointPixelComponentType == IOComponentEnum::UNKNOWNCOMPONENTTYPE)
        {
          itkExceptionMacro(<< "Unknown point SYMMETRICSECONDRANKTENSOR component type");
        }

        // Set point pixel type
        this->m_PointPixelType = IOPixelEnum::SYMMETRICSECONDRANKTENSOR;
        this->m_NumberOfPointPixelComponents = this->m_PointDimension * (this->m_PointDimension + 1) / 2;
        this->m_UpdatePointData = true;
      }
    }
    else if (line.find("CELL_DATA") != std::string::npos)
    {
      // define string stream and put line into it
      StringStreamType cdss;
      cdss << line;

      // Get each item
      cdss >> item; // should be "CELL_DATA"

      // Get number of Point pixels
      cdss >> this->m_NumberOfCellPixels;

      // Continue to read line and get data type
      if (!inputFile.eof())
      {
        std::getline(inputFile, line, '\n');
      }
      else
      {
        itkExceptionMacro("UnExpected end of line while trying to read POINT_DATA");
      }

      this->m_UpdateCellData = true;

      // Read cell data pixel type
      if (line.find("SCALARS") != std::string::npos)
      {
        // define string stream and put line into it
        StringStreamType sss;
        sss << line;

        // Get each item
        sss >> item; // should be "SCALARS"

        if (item == "SCALARS")
        {
          sss >> item; // "cell data name "

          //  Get cell data component type
          StringType cellDataComponentType;
          sss >> cellDataComponentType;

          // Set point pixel component type
          this->m_CellPixelComponentType = this->GetComponentTypeFromString(cellDataComponentType);
          if (this->m_CellPixelComponentType == IOComponentEnum::UNKNOWNCOMPONENTTYPE)
          {
            itkExceptionMacro(<< "Unknown cell component type");
          }

          // Set cell pixel type
          this->m_CellPixelType = IOPixelEnum::SCALAR;
          this->m_NumberOfCellPixelComponents = itk::NumericTraits<unsigned int>::OneValue();
          this->m_UpdateCellData = true;
        }
      }
      if (line.find("COLOR_SCALARS") != std::string::npos)
      {
        // define string stream and put line into it
        StringStreamType csss;
        csss << line;

        csss >> item; // should be "COLOR_SCALARS"
        csss >> item; // "cell data name "
        csss >> this->m_NumberOfCellPixelComponents;

        // Set cell pixel type
        this->m_CellPixelType = IOPixelEnum::VARIABLELENGTHVECTOR;
        if (this->m_FileType == IOFileEnum::ASCII)
        {
          this->m_CellPixelComponentType = IOComponentEnum::FLOAT;
        }
        else
        {
          this->m_CellPixelComponentType = IOComponentEnum::UCHAR;
        }

        this->m_UpdateCellData = true;
      }

      // Read cell vector data
      if (line.find("VECTORS") != std::string::npos || line.find("NORMALS") != std::string::npos)
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
        if (this->m_CellPixelComponentType == IOComponentEnum::UNKNOWNCOMPONENTTYPE)
        {
          itkExceptionMacro(<< "Unknown cell normal component type");
        }

        // Set cell pixel type
        this->m_CellPixelType = IOPixelEnum::VECTOR;
        this->m_NumberOfCellPixelComponents = this->m_PointDimension;
        this->m_UpdateCellData = true;
      }
      if (line.find("TENSORS") != std::string::npos)
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
        if (this->m_CellPixelComponentType == IOComponentEnum::UNKNOWNCOMPONENTTYPE)
        {
          itkExceptionMacro(<< "Unknown cell SYMMETRICSECONDRANKTENSOR component type");
        }

        // Set cell pixel type
        this->m_CellPixelType = IOPixelEnum::SYMMETRICSECONDRANKTENSOR;
        this->m_NumberOfCellPixelComponents = this->m_PointDimension * (this->m_PointDimension + 1) / 2;
        this->m_UpdateCellData = true;
      }
    }
  }

  if (this->m_CellBufferSize)
  {
    this->m_CellBufferSize += this->m_NumberOfCells;
  }

  inputFile.close();
}

#define CASE_INVOKE_BY_TYPE(function, param)                                                                           \
  case IOComponentEnum::UCHAR:                                                                                         \
  {                                                                                                                    \
    function(param, static_cast<unsigned char *>(buffer));                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::CHAR:                                                                                          \
  {                                                                                                                    \
    function(param, static_cast<char *>(buffer));                                                                      \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::USHORT:                                                                                        \
  {                                                                                                                    \
    function(param, static_cast<unsigned short *>(buffer));                                                            \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::SHORT:                                                                                         \
  {                                                                                                                    \
    function(param, static_cast<short *>(buffer));                                                                     \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::UINT:                                                                                          \
  {                                                                                                                    \
    function(param, static_cast<unsigned int *>(buffer));                                                              \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::INT:                                                                                           \
  {                                                                                                                    \
    function(param, static_cast<int *>(buffer));                                                                       \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::ULONG:                                                                                         \
  {                                                                                                                    \
    function(param, static_cast<unsigned long *>(buffer));                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::LONG:                                                                                          \
  {                                                                                                                    \
    function(param, static_cast<long *>(buffer));                                                                      \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::ULONGLONG:                                                                                     \
  {                                                                                                                    \
    function(param, static_cast<unsigned long long *>(buffer));                                                        \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::LONGLONG:                                                                                      \
  {                                                                                                                    \
    function(param, static_cast<long long *>(buffer));                                                                 \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::FLOAT:                                                                                         \
  {                                                                                                                    \
    function(param, static_cast<float *>(buffer));                                                                     \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::DOUBLE:                                                                                        \
  {                                                                                                                    \
    function(param, static_cast<double *>(buffer));                                                                    \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::LDOUBLE:                                                                                       \
  {                                                                                                                    \
    function(param, static_cast<long double *>(buffer));                                                               \
    break;                                                                                                             \
  }

void
VTKPolyDataMeshIO ::ReadPoints(void * buffer)
{
  std::ifstream inputFile;

  if (this->m_FileType == IOFileEnum::ASCII)
  {
    inputFile.open(this->m_FileName.c_str(), std::ios::in);
  }
  else if (m_FileType == IOFileEnum::BINARY)
  {
    inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
  }

  if (!inputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "inputFilename= "
                      << this->m_FileName);
  }


  if (this->m_FileType == IOFileEnum::ASCII)
  {
    switch (this->m_PointComponentType)
    {
      CASE_INVOKE_BY_TYPE(ReadPointsBufferAsASCII, inputFile)

      default:
      {
        itkExceptionMacro(<< "Unknown point component type");
      }
    }
  }
  else if (this->m_FileType == IOFileEnum::BINARY)
  {
    switch (this->m_PointComponentType)
    {
      CASE_INVOKE_BY_TYPE(ReadPointsBufferAsBINARY, inputFile)

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
VTKPolyDataMeshIO ::ReadCells(void * buffer)
{
  std::ifstream inputFile;

  if (this->m_FileType == IOFileEnum::ASCII)
  {
    inputFile.open(this->m_FileName.c_str(), std::ios::in);
  }
  else if (m_FileType == IOFileEnum::BINARY)
  {
    inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
  }

  if (!inputFile.is_open())
  {
    itkExceptionMacro(<< "Unable to open file\n"
                         "inputFilename= "
                      << this->m_FileName);
  }


  if (this->m_FileType == IOFileEnum::ASCII)
  {
    ReadCellsBufferAsASCII(inputFile, buffer);
  }
  else if (this->m_FileType == IOFileEnum::BINARY)
  {
    ReadCellsBufferAsBINARY(inputFile, buffer);
  }
  else
  {
    itkExceptionMacro(<< "Unkonw file type");
  }

  inputFile.close();
}

void
VTKPolyDataMeshIO::ReadCellsBufferAsASCII(std::ifstream & inputFile, void * buffer)
{
  std::string   line;
  SizeValueType index = 0;
  unsigned int  numPoints; // number of point in each cell

  MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
  using GeometryIntegerType = unsigned int;
  auto * data = static_cast<GeometryIntegerType *>(buffer);

  while (!inputFile.eof())
  {
    std::getline(inputFile, line, '\n');
    if (line.find("VERTICES") != std::string::npos)
    {
      unsigned int numberOfVertices = 0;
      ExposeMetaData<unsigned int>(metaDic, "numberOfVertices", numberOfVertices);

      for (unsigned int ii = 0; ii < numberOfVertices; ii++)
      {
        inputFile >> numPoints;
        data[index++] = static_cast<GeometryIntegerType>(CellGeometryEnum::VERTEX_CELL);
        data[index++] = numPoints;
        for (unsigned int jj = 0; jj < numPoints; jj++)
        {
          inputFile >> data[index++];
        }
      }
    }
    else if (line.find("LINES") != std::string::npos)
    {
      unsigned int numberOfLines = 0;
      ExposeMetaData<unsigned int>(metaDic, "numberOfLines", numberOfLines);

      for (unsigned int ii = 0; ii < numberOfLines; ii++)
      {
        inputFile >> numPoints;
        data[index++] = static_cast<GeometryIntegerType>(CellGeometryEnum::LINE_CELL);
        data[index++] = numPoints;
        for (unsigned int jj = 0; jj < numPoints; jj++)
        {
          inputFile >> data[index++];
        }
      }
    }
    else if (line.find("POLYGONS") != std::string::npos)
    {
      unsigned int numberOfPolygons = 0;
      ExposeMetaData<unsigned int>(metaDic, "numberOfPolygons", numberOfPolygons);

      for (unsigned int ii = 0; ii < numberOfPolygons; ii++)
      {
        inputFile >> numPoints;

        data[index++] = static_cast<GeometryIntegerType>(CellGeometryEnum::POLYGON_CELL);
        data[index++] = numPoints;
        for (unsigned int jj = 0; jj < numPoints; jj++)
        {
          inputFile >> data[index++];
        }
      }
    }
  }
}

void
VTKPolyDataMeshIO ::ReadCellsBufferAsBINARY(std::ifstream & inputFile, void * buffer)
{
  if (!this->m_CellBufferSize)
  {
    return;
  }

  auto * inputBuffer = new unsigned int[this->m_CellBufferSize - this->m_NumberOfCells];
  void * pv = inputBuffer;
  auto * startBuffer = static_cast<char *>(pv);
  auto * outputBuffer = static_cast<unsigned int *>(buffer);

  std::string          line;
  MetaDataDictionary & metaDic = this->GetMetaDataDictionary();

  while (!inputFile.eof())
  {
    std::getline(inputFile, line, '\n');
    if (line.find("VERTICES") != std::string::npos)
    {
      unsigned int numberOfVertices = 0;
      unsigned int numberOfVertexIndices = 0;
      ExposeMetaData<unsigned int>(metaDic, "numberOfVertices", numberOfVertices);
      ExposeMetaData<unsigned int>(metaDic, "numberOfVertexIndices", numberOfVertexIndices);
      inputFile.read(startBuffer, numberOfVertexIndices * sizeof(unsigned int));

      pv = startBuffer;
      auto * data = static_cast<unsigned int *>(pv);
      if (itk::ByteSwapper<unsigned int>::SystemIsLittleEndian())
      {
        itk::ByteSwapper<unsigned int>::SwapRangeFromSystemToBigEndian(data, numberOfVertexIndices);
      }
      this->WriteCellsBuffer(data, outputBuffer, CellGeometryEnum::VERTEX_CELL, numberOfVertices);
      startBuffer += numberOfVertexIndices * sizeof(unsigned int);
      outputBuffer += (numberOfVertexIndices + numberOfVertices) * sizeof(unsigned int);
    }
    else if (line.find("LINES") != std::string::npos)
    {
      unsigned int numberOfLines = 0;
      unsigned int numberOfLineIndices = 0;
      ExposeMetaData<unsigned int>(metaDic, "numberOfLines", numberOfLines);
      ExposeMetaData<unsigned int>(metaDic, "numberOfLineIndices", numberOfLineIndices);
      inputFile.read(startBuffer, numberOfLineIndices * sizeof(unsigned int));

      pv = startBuffer;
      auto * data = static_cast<unsigned int *>(pv);
      if (itk::ByteSwapper<unsigned int>::SystemIsLittleEndian())
      {
        itk::ByteSwapper<unsigned int>::SwapRangeFromSystemToBigEndian(data, numberOfLineIndices);
      }
      this->WriteCellsBuffer(data, outputBuffer, CellGeometryEnum::LINE_CELL, numberOfLines);
      startBuffer += numberOfLineIndices * sizeof(unsigned int);
      outputBuffer += (numberOfLineIndices + numberOfLines) * sizeof(unsigned int);
    }
    else if (line.find("POLYGONS") != std::string::npos)
    {
      unsigned int numberOfPolygons = 0;
      unsigned int numberOfPolygonIndices = 0;
      ExposeMetaData<unsigned int>(metaDic, "numberOfPolygons", numberOfPolygons);
      ExposeMetaData<unsigned int>(metaDic, "numberOfPolygonIndices", numberOfPolygonIndices);
      inputFile.read(startBuffer, numberOfPolygonIndices * sizeof(unsigned int));

      pv = startBuffer;
      auto * data = static_cast<unsigned int *>(pv);
      if (itk::ByteSwapper<unsigned int>::SystemIsLittleEndian())
      {
        itk::ByteSwapper<unsigned int>::SwapRangeFromSystemToBigEndian(data, numberOfPolygonIndices);
      }

      this->WriteCellsBuffer(data, outputBuffer, CellGeometryEnum::POLYGON_CELL, numberOfPolygons);
      startBuffer += numberOfPolygonIndices * sizeof(unsigned int);
      outputBuffer += (numberOfPolygonIndices + numberOfPolygons) * sizeof(unsigned int);
    }
  }

  if (this->m_CellBufferSize)
  {
    delete[] inputBuffer;
  }
}

void
VTKPolyDataMeshIO ::ReadPointData(void * buffer)
{
  std::ifstream inputFile;

  if (this->m_FileType == IOFileEnum::ASCII)
  {
    inputFile.open(this->m_FileName.c_str(), std::ios::in);
  }
  else if (m_FileType == IOFileEnum::BINARY)
  {
    inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
  }

  if (!inputFile.is_open())
  {
    itkExceptionMacro(<< "Unable to open file\n"
                         "inputFilename= "
                      << this->m_FileName);
  }


  if (this->m_FileType == IOFileEnum::ASCII)
  {
    switch (this->m_PointPixelComponentType)
    {
      CASE_INVOKE_BY_TYPE(ReadPointDataBufferAsASCII, inputFile)

      default:
      {
        itkExceptionMacro(<< "Unknown point pixel component");
      }
    }
  }
  else if (this->m_FileType == IOFileEnum::BINARY)
  {
    switch (this->m_PointPixelComponentType)
    {
      CASE_INVOKE_BY_TYPE(ReadPointDataBufferAsBINARY, inputFile)

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
VTKPolyDataMeshIO ::ReadCellData(void * buffer)
{
  std::ifstream inputFile;

  if (this->m_FileType == IOFileEnum::ASCII)
  {
    inputFile.open(this->m_FileName.c_str(), std::ios::in);
  }
  else if (m_FileType == IOFileEnum::BINARY)
  {
    inputFile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
  }

  if (!inputFile.is_open())
  {
    itkExceptionMacro(<< "Unable to open file\n"
                         "inputFilename= "
                      << this->m_FileName);
  }


  if (this->m_FileType == IOFileEnum::ASCII)
  {
    switch (this->m_CellPixelComponentType)
    {
      CASE_INVOKE_BY_TYPE(ReadCellDataBufferAsASCII, inputFile)

      default:
      {
        itkExceptionMacro(<< "Unknown cell pixel component");
      }
    }
  }
  else if (this->m_FileType == IOFileEnum::BINARY)
  {
    switch (this->m_CellPixelComponentType)
    {
      CASE_INVOKE_BY_TYPE(ReadCellDataBufferAsBINARY, inputFile)

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
VTKPolyDataMeshIO ::WriteMeshInformation()
{
  // Check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  // Define output file stream
  std::ofstream outputFile;
  if (this->m_FileType == IOFileEnum::ASCII)
  {
    outputFile.open(this->m_FileName.c_str(), std::ios::out);
  }
  else if (m_FileType == IOFileEnum::BINARY)
  {
    outputFile.open(this->m_FileName.c_str(), std::ios::out | std::ios::binary);
  }

  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }

  // Write VTK header
  outputFile << "# vtk DataFile Version 2.0"
             << "\n";
  outputFile << "File written by itkPolyDataMeshIO"
             << "\n";
  if (m_FileType == IOFileEnum::ASCII)
  {
    outputFile << "ASCII"
               << "\n";
  }
  else if (m_FileType == IOFileEnum::BINARY)
  {
    outputFile << "BINARY"
               << "\n";
  }
  else
  {
    itkExceptionMacro(<< "Invalid output file type (not ASCII or BINARY)");
  }

  outputFile << "DATASET POLYDATA"
             << "\n";

  outputFile.close();
}

#define CASE_INVOKE_WITH_COMPONENT_TYPE(function)                                                                      \
  case IOComponentEnum::UCHAR:                                                                                         \
  {                                                                                                                    \
    function(outputFile, static_cast<unsigned char *>(buffer), " unsigned_char");                                      \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::CHAR:                                                                                          \
  {                                                                                                                    \
    function(outputFile, static_cast<char *>(buffer), " char");                                                        \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::USHORT:                                                                                        \
  {                                                                                                                    \
    function(outputFile, static_cast<unsigned short *>(buffer), " unsigned_short");                                    \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::SHORT:                                                                                         \
  {                                                                                                                    \
    function(outputFile, static_cast<short *>(buffer), " short");                                                      \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::UINT:                                                                                          \
  {                                                                                                                    \
    function(outputFile, static_cast<unsigned int *>(buffer), " unsigned_int");                                        \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::INT:                                                                                           \
  {                                                                                                                    \
    function(outputFile, static_cast<int *>(buffer), " int");                                                          \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::ULONG:                                                                                         \
  {                                                                                                                    \
    function(outputFile, static_cast<unsigned long *>(buffer), " unsigned_long");                                      \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::LONG:                                                                                          \
  {                                                                                                                    \
    function(outputFile, static_cast<long *>(buffer), " long");                                                        \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::ULONGLONG:                                                                                     \
  {                                                                                                                    \
    function(outputFile, static_cast<unsigned long long *>(buffer), " vtktypeuint64");                                 \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::LONGLONG:                                                                                      \
  {                                                                                                                    \
    function(outputFile, static_cast<long long *>(buffer), " vtktypeint64");                                           \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::FLOAT:                                                                                         \
  {                                                                                                                    \
    function(outputFile, static_cast<float *>(buffer), " float");                                                      \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::DOUBLE:                                                                                        \
  {                                                                                                                    \
    function(outputFile, static_cast<double *>(buffer), " double");                                                    \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::LDOUBLE:                                                                                       \
  {                                                                                                                    \
    function(outputFile, static_cast<long double *>(buffer), " long_double");                                          \
    break;                                                                                                             \
  }

void
VTKPolyDataMeshIO ::WritePoints(void * buffer)
{
  // Check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  std::ofstream outputFile;
  if (this->m_FileType == IOFileEnum::ASCII)
  {
    outputFile.open(this->m_FileName.c_str(), std::ios::app);
  }
  else if (m_FileType == IOFileEnum::BINARY)
  {
    outputFile.open(this->m_FileName.c_str(), std::ios::app | std::ios::binary);
  }

  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }


  if (this->m_FileType == IOFileEnum::ASCII)
  {
    switch (this->m_PointComponentType)
    {
      CASE_INVOKE_WITH_COMPONENT_TYPE(WritePointsBufferAsASCII)

      default:
        itkExceptionMacro(<< "Unknonwn point component type");
    }
  }
  else if (this->m_FileType == IOFileEnum::BINARY)
  {
    switch (this->m_PointComponentType)
    {
      CASE_INVOKE_WITH_COMPONENT_TYPE(WritePointsBufferAsBINARY)

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

#define CASE_UPDATE_AND_WRITE(function)                                                                                \
  case IOComponentEnum::UCHAR:                                                                                         \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<unsigned char *>(buffer));                                                       \
    function(outputFile, static_cast<unsigned char *>(buffer));                                                        \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::CHAR:                                                                                          \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<char *>(buffer));                                                                \
    function(outputFile, static_cast<char *>(buffer));                                                                 \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::USHORT:                                                                                        \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<unsigned short *>(buffer));                                                      \
    function(outputFile, static_cast<unsigned short *>(buffer));                                                       \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::SHORT:                                                                                         \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<short *>(buffer));                                                               \
    function(outputFile, static_cast<short *>(buffer));                                                                \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::UINT:                                                                                          \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<unsigned int *>(buffer));                                                        \
    function(outputFile, static_cast<unsigned int *>(buffer));                                                         \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::INT:                                                                                           \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<int *>(buffer));                                                                 \
    function(outputFile, static_cast<int *>(buffer));                                                                  \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::ULONG:                                                                                         \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<unsigned long *>(buffer));                                                       \
    function(outputFile, static_cast<unsigned long *>(buffer));                                                        \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::LONG:                                                                                          \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<long *>(buffer));                                                                \
    function(outputFile, static_cast<long *>(buffer));                                                                 \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::ULONGLONG:                                                                                     \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<unsigned long long *>(buffer));                                                  \
    function(outputFile, static_cast<unsigned long long *>(buffer));                                                   \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::LONGLONG:                                                                                      \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<long long *>(buffer));                                                           \
    function(outputFile, static_cast<long long *>(buffer));                                                            \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::FLOAT:                                                                                         \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<float *>(buffer));                                                               \
    function(outputFile, static_cast<float *>(buffer));                                                                \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::DOUBLE:                                                                                        \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<double *>(buffer));                                                              \
    function(outputFile, static_cast<double *>(buffer));                                                               \
    break;                                                                                                             \
  }                                                                                                                    \
  case IOComponentEnum::LDOUBLE:                                                                                       \
  {                                                                                                                    \
    UpdateCellInformation(static_cast<long double *>(buffer));                                                         \
    function(outputFile, static_cast<long double *>(buffer));                                                          \
    break;                                                                                                             \
  }

void
VTKPolyDataMeshIO ::WriteCells(void * buffer)
{
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  std::ofstream outputFile;
  if (this->m_FileType == IOFileEnum::ASCII)
  {
    outputFile.open(this->m_FileName.c_str(), std::ios::app);
  }
  else if (m_FileType == IOFileEnum::BINARY)
  {
    outputFile.open(this->m_FileName.c_str(), std::ios::app | std::ios::binary);
  }

  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }


  if (this->m_FileType == IOFileEnum::ASCII)
  {
    switch (this->m_CellComponentType)
    {
      CASE_UPDATE_AND_WRITE(WriteCellsBufferAsASCII)

      default:
        itkExceptionMacro(<< "Unknonwn cell component type");
    }
  }
  else if (this->m_FileType == IOFileEnum::BINARY)
  {
    switch (this->m_CellComponentType)
    {
      CASE_UPDATE_AND_WRITE(WriteCellsBufferAsBINARY)

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
VTKPolyDataMeshIO ::WritePointData(void * buffer)
{
  // check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  std::ofstream outputFile;
  if (this->m_FileType == IOFileEnum::ASCII)
  {
    outputFile.open(this->m_FileName.c_str(), std::ios::app);
  }
  else if (m_FileType == IOFileEnum::BINARY)
  {
    outputFile.open(this->m_FileName.c_str(), std::ios::app | std::ios::binary);
  }

  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }


  if (this->m_FileType == IOFileEnum::ASCII)
  {
    switch (this->m_PointPixelComponentType)
    {
      CASE_INVOKE_WITH_COMPONENT_TYPE(WritePointDataBufferAsASCII)

      default:
        itkExceptionMacro(<< "Unknonwn point pixel component type");
    }
  }
  else if (this->m_FileType == IOFileEnum::BINARY)
  {
    switch (this->m_PointPixelComponentType)
    {
      CASE_INVOKE_WITH_COMPONENT_TYPE(WritePointDataBufferAsBINARY)

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
VTKPolyDataMeshIO ::WriteCellData(void * buffer)
{
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  std::ofstream outputFile;
  if (this->m_FileType == IOFileEnum::ASCII)
  {
    outputFile.open(this->m_FileName.c_str(), std::ios::app);
  }
  else if (m_FileType == IOFileEnum::BINARY)
  {
    outputFile.open(this->m_FileName.c_str(), std::ios::app | std::ios::binary);
  }

  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }

  if (this->m_FileType == IOFileEnum::ASCII)
  {
    switch (this->m_CellPixelComponentType)
    {
      CASE_INVOKE_WITH_COMPONENT_TYPE(WriteCellDataBufferAsASCII)

      default:
        itkExceptionMacro(<< "Unknonwn cell pixel component type");
    }
  }
  else if (this->m_FileType == IOFileEnum::BINARY)
  {
    switch (this->m_CellPixelComponentType)
    {
      CASE_INVOKE_WITH_COMPONENT_TYPE(WriteCellDataBufferAsBINARY)

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
VTKPolyDataMeshIO ::Write()
{}

void
VTKPolyDataMeshIO ::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  const MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
  unsigned int               value = 0;

  if (ExposeMetaData<unsigned int>(metaDic, "numberOfVertices", value))
  {
    os << indent << "number of vertices : " << value << std::endl;
  }

  if (ExposeMetaData<unsigned int>(metaDic, "numberOfLines", value))
  {
    os << indent << "number of lines : " << value << std::endl;
  }

  if (ExposeMetaData<unsigned int>(metaDic, "numberOfPolygons", value))
  {
    os << indent << "number of polygons : " << value << std::endl;
  }

  StringType dataName;
  if (ExposeMetaData<StringType>(metaDic, "pointScalarDataName", dataName))
  {
    os << indent << "pointScalarDataName : " << dataName << std::endl;
  }

  if (ExposeMetaData<StringType>(metaDic, "pointVectorDataName", dataName))
  {
    os << indent << "pointVectorDataName : " << dataName << std::endl;
  }

  if (ExposeMetaData<StringType>(metaDic, "pointTensorDataName", dataName))
  {
    os << indent << "pointTensorDataName : " << dataName << std::endl;
  }

  if (ExposeMetaData<StringType>(metaDic, "cellScalarDataName", dataName))
  {
    os << indent << "cellScalarDataName : " << dataName << std::endl;
  }

  if (ExposeMetaData<StringType>(metaDic, "cellVectorDataName", dataName))
  {
    os << indent << "cellVectorDataName : " << dataName << std::endl;
  }

  if (ExposeMetaData<StringType>(metaDic, "cellTensorDataName", dataName))
  {
    os << indent << "cellTensorDataName : " << dataName << std::endl;
  }
}
} // end of namespace itk
