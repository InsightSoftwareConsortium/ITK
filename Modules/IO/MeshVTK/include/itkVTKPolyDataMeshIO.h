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
#ifndef itkVTKPolyDataMeshIO_h
#define itkVTKPolyDataMeshIO_h
#include "ITKIOMeshVTKExport.h"

#include "itkByteSwapper.h"
#include "itkMetaDataObject.h"
#include "itkMeshIOBase.h"
#include "itkVectorContainer.h"
#include "itkNumberToString.h"
#include "itkMakeUniqueForOverwrite.h"

#include <fstream>
#include <vector>

namespace itk
{
/**
 * \class VTKPolyDataMeshIO
 * \brief This class defines how to read and write vtk legacy file format.
 *
 * \author Wanlin Zhu. Uviversity of New South Wales, Australia.
 * \ingroup IOFilters
 * \ingroup ITKIOMeshVTK
 */
class ITKIOMeshVTK_EXPORT VTKPolyDataMeshIO : public MeshIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VTKPolyDataMeshIO);

  /** Standard "Self" type alias. */
  using Self = VTKPolyDataMeshIO;
  using Superclass = MeshIOBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using SizeValueType = Superclass::SizeValueType;

  using StringType = std::string;
  using StringVectorType = std::vector<StringType>;
  using StringStreamType = std::stringstream;
  using PointIdVector = std::vector<SizeValueType>;
  using PolylinesContainerType = VectorContainer<SizeValueType, PointIdVector>;
  using PolylinesContainerPointer = PolylinesContainerType::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKPolyDataMeshIO, MeshIOBase);

  /**-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this MeshIO implementation.
   * \param fileName The name of the file to test for reading.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can read the file specified.
   */
  bool
  CanReadFile(const char * fileName) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  ReadMeshInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  ReadPoints(void * buffer) override;

  void
  ReadCells(void * buffer) override;

  void
  ReadPointData(void * buffer) override;

  void
  ReadCellData(void * buffer) override;

  /*-------- This part of the interfaces deals with writing data. ----- */
  /** Determine if the file can be written with this MeshIO implementation.
   * \param fileName The name of the file to test for writing.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can write the file specified.
   */
  bool
  CanWriteFile(const char * fileName) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  WriteMeshInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  void
  WritePoints(void * buffer) override;

  void
  WriteCells(void * buffer) override;

  void
  WritePointData(void * buffer) override;

  void
  WriteCellData(void * buffer) override;

  void
  Write() override;

protected:
  VTKPolyDataMeshIO();
  ~VTKPolyDataMeshIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  // Internal function to get next line from a given file (*.vtk)
  int
  GetNextLine(std::ifstream & ifs, std::string & line, bool lowerCase = true, SizeValueType count = 0);

  template <typename T>
  void
  UpdateCellInformation(T * buffer)
  {
    unsigned int  numberOfVertices = 0;
    unsigned int  numberOfVertexIndices = 0;
    unsigned int  numberOfLines = 0;
    SizeValueType numberOfLineIndices = 0;
    unsigned int  numberOfPolygons = 0;
    unsigned int  numberOfPolygonIndices = 0;

    SizeValueType index = 0;

    for (SizeValueType ii = 0; ii < this->m_NumberOfCells; ++ii)
    {
      auto cellType = static_cast<CellGeometryEnum>(static_cast<int>(buffer[index++]));
      auto nn = static_cast<unsigned int>(buffer[index++]);
      switch (cellType)
      {
        case CellGeometryEnum::VERTEX_CELL:
          ++numberOfVertices;
          numberOfVertexIndices += nn + 1;
          break;
        case CellGeometryEnum::LINE_CELL:
          ++numberOfLines;
          numberOfLineIndices += nn + 1;
          break;
        case CellGeometryEnum::POLYLINE_CELL:
          ++numberOfLines;
          numberOfLineIndices += nn + 1;
          break;
        case CellGeometryEnum::TRIANGLE_CELL:
          ++numberOfPolygons;
          numberOfPolygonIndices += nn + 1;
          break;
        case CellGeometryEnum::POLYGON_CELL:
          ++numberOfPolygons;
          numberOfPolygonIndices += nn + 1;
          break;
        case CellGeometryEnum::QUADRILATERAL_CELL:
          ++numberOfPolygons;
          numberOfPolygonIndices += nn + 1;
          break;
        default:
          itkExceptionMacro(<< "Currently we dont support this cell type");
      }

      index += nn;
    }

    MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
    EncapsulateMetaData<unsigned int>(metaDic, "numberOfVertices", numberOfVertices);
    EncapsulateMetaData<unsigned int>(metaDic, "numberOfVertexIndices", numberOfVertexIndices);
    EncapsulateMetaData<unsigned int>(metaDic, "numberOfLines", numberOfLines);
    EncapsulateMetaData<unsigned int>(metaDic, "numberOfLineIndices", numberOfLineIndices);
    EncapsulateMetaData<unsigned int>(metaDic, "numberOfPolygons", numberOfPolygons);
    EncapsulateMetaData<unsigned int>(metaDic, "numberOfPolygonIndices", numberOfPolygonIndices);
    return;
  }

  template <typename T>
  void
  ReadPointsBufferAsASCII(std::ifstream & inputFile, T * buffer)
  {
    std::string line;

    while (!inputFile.eof())
    {
      std::getline(inputFile, line, '\n');

      if (line.find("POINTS") != std::string::npos)
      {
        /**  Load the point coordinates into the itk::Mesh */
        Self::ReadComponentsAsASCII(inputFile, buffer, this->m_NumberOfPoints * this->m_PointDimension);
      }
    }
  }

  template <typename T>
  void
  ReadPointsBufferAsBINARY(std::ifstream & inputFile, T * buffer)
  {
    std::string line;

    while (!inputFile.eof())
    {
      std::getline(inputFile, line, '\n');

      if (line.find("POINTS") != std::string::npos)
      {
        /**  Load the point coordinates into the itk::Mesh */
        SizeValueType numberOfComponents = this->m_NumberOfPoints * this->m_PointDimension;
        inputFile.read(reinterpret_cast<char *>(buffer), numberOfComponents * sizeof(T));
        if (itk::ByteSwapper<T>::SystemIsLittleEndian())
        {
          itk::ByteSwapper<T>::SwapRangeFromSystemToBigEndian(buffer, numberOfComponents);
        }
      }
    }
  }

  void
  ReadCellsBufferAsASCII(std::ifstream & inputFile, void * buffer);

  void
  ReadCellsBufferAsBINARY(std::ifstream & inputFile, void * buffer);

  template <typename T>
  void
  ReadPointDataBufferAsASCII(std::ifstream & inputFile, T * buffer)
  {
    StringType line;

    while (!inputFile.eof())
    {
      std::getline(inputFile, line, '\n');
      if (line.find("POINT_DATA") != std::string::npos)
      {
        if (!inputFile.eof())
        {
          std::getline(inputFile, line, '\n');
        }
        else
        {
          itkExceptionMacro("UnExpected end of line while trying to read POINT_DATA");
        }

        /** For scalars we have to read the next line of LOOKUP_TABLE */
        if (line.find("SCALARS") != std::string::npos && line.find("COLOR_SCALARS") == std::string::npos)
        {
          if (!inputFile.eof())
          {
            std::getline(inputFile, line, '\n');
            if (line.find("LOOKUP_TABLE") == std::string::npos)
            {
              itkExceptionMacro("UnExpected end of line while trying to read LOOKUP_TABLE");
            }
          }
          else
          {
            itkExceptionMacro("UnExpected end of line while trying to read LOOKUP_TABLE");
          }
        }

        /** for VECTORS or NORMALS or TENSORS, we could read them directly */
        Self::ReadComponentsAsASCII(
          inputFile, buffer, this->m_NumberOfPointPixels * this->m_NumberOfPointPixelComponents);
      }
    }
  }

  template <typename T>
  void
  ReadPointDataBufferAsBINARY(std::ifstream & inputFile, T * buffer)
  {
    StringType line;

    while (!inputFile.eof())
    {
      std::getline(inputFile, line, '\n');
      if (line.find("POINT_DATA") != std::string::npos)
      {
        if (!inputFile.eof())
        {
          std::getline(inputFile, line, '\n');
        }
        else
        {
          itkExceptionMacro("UnExpected end of line while trying to read POINT_DATA");
        }

        /** For scalars we have to read the next line of LOOKUP_TABLE */
        if (line.find("SCALARS") != std::string::npos && line.find("COLOR_SCALARS") == std::string::npos)
        {
          if (!inputFile.eof())
          {
            std::getline(inputFile, line, '\n');
            if (line.find("LOOKUP_TABLE") == std::string::npos)
            {
              itkExceptionMacro("UnExpected end of line while trying to read LOOKUP_TABLE");
            }
          }
          else
          {
            itkExceptionMacro("UnExpected end of line while trying to read LOOKUP_TABLE");
          }
        }

        /** for VECTORS or NORMALS or TENSORS, we could read them directly */
        SizeValueType numberOfComponents = this->m_NumberOfPointPixels * this->m_NumberOfPointPixelComponents;
        inputFile.read(reinterpret_cast<char *>(buffer), numberOfComponents * sizeof(T));
        if (itk::ByteSwapper<T>::SystemIsLittleEndian())
        {
          itk::ByteSwapper<T>::SwapRangeFromSystemToBigEndian(buffer, numberOfComponents);
        }
      }
    }
  }

  template <typename T>
  void
  ReadCellDataBufferAsASCII(std::ifstream & inputFile, T * buffer)
  {
    StringType line;

    while (!inputFile.eof())
    {
      std::getline(inputFile, line, '\n');
      if (line.find("CELL_DATA") != std::string::npos)
      {
        if (!inputFile.eof())
        {
          std::getline(inputFile, line, '\n');
        }
        else
        {
          itkExceptionMacro("UnExpected end of line while trying to read CELL_DATA");
        }

        /** For scalars we have to read the next line of LOOKUP_TABLE */
        if (line.find("SCALARS") != std::string::npos && line.find("COLOR_SCALARS") == std::string::npos)
        {
          if (!inputFile.eof())
          {
            std::getline(inputFile, line, '\n');
            if (line.find("LOOKUP_TABLE") == std::string::npos)
            {
              itkExceptionMacro("UnExpected end of line while trying to read LOOKUP_TABLE");
            }
          }
          else
          {
            itkExceptionMacro("UnExpected end of line while trying to read LOOKUP_TABLE");
          }
        }

        /** for VECTORS or NORMALS or TENSORS, we could read them directly */
        Self::ReadComponentsAsASCII(
          inputFile, buffer, this->m_NumberOfCellPixels * this->m_NumberOfCellPixelComponents);
      }
    }
  }

  template <typename T>
  void
  ReadCellDataBufferAsBINARY(std::ifstream & inputFile, T * buffer)
  {
    StringType line;

    while (!inputFile.eof())
    {
      std::getline(inputFile, line, '\n');
      if (line.find("POINT_DATA") != std::string::npos)
      {
        if (!inputFile.eof())
        {
          std::getline(inputFile, line, '\n');
        }
        else
        {
          itkExceptionMacro("UnExpected end of line while trying to read POINT_DATA");
        }

        /** For scalars we have to read the next line of LOOKUP_TABLE */
        if (line.find("SCALARS") != std::string::npos && line.find("COLOR_SCALARS") == std::string::npos)
        {
          if (!inputFile.eof())
          {
            std::getline(inputFile, line, '\n');
            if (line.find("LOOKUP_TABLE") == std::string::npos)
            {
              itkExceptionMacro("UnExpected end of line while trying to read LOOKUP_TABLE");
            }
          }
          else
          {
            itkExceptionMacro("UnExpected end of line while trying to read LOOKUP_TABLE");
          }
        }
        /** For VECTORS or NORMALS or TENSORS, we could read them directly */
        SizeValueType numberOfComponents = this->m_NumberOfCellPixels * this->m_NumberOfCellPixelComponents;
        inputFile.read(reinterpret_cast<char *>(buffer), numberOfComponents * sizeof(T));
        if (itk::ByteSwapper<T>::SystemIsLittleEndian())
        {
          itk::ByteSwapper<T>::SwapRangeFromSystemToBigEndian(buffer, numberOfComponents);
        }
      }
    }
  }

  template <typename T>
  void
  WritePointsBufferAsASCII(std::ofstream & outputFile, T * buffer, const StringType & pointComponentType)
  {
    /** 1. Write number of points */
    outputFile << "POINTS " << this->m_NumberOfPoints;

    outputFile << pointComponentType << '\n';
    for (SizeValueType ii = 0; ii < this->m_NumberOfPoints; ++ii)
    {
      for (unsigned int jj = 0; jj < this->m_PointDimension - 1; ++jj)
      {
        outputFile << ConvertNumberToString(buffer[ii * this->m_PointDimension + jj]) << " ";
      }

      outputFile << ConvertNumberToString(buffer[ii * this->m_PointDimension + this->m_PointDimension - 1]) << '\n';
    }

    return;
  }

  template <typename T>
  void
  WritePointsBufferAsBINARY(std::ofstream & outputFile, T * buffer, const StringType & pointComponentType)
  {
    /** 1. Write number of points */
    outputFile << "POINTS " << this->m_NumberOfPoints;
    outputFile << pointComponentType << "\n";
    itk::ByteSwapper<T>::SwapWriteRangeFromSystemToBigEndian(
      buffer, this->m_NumberOfPoints * this->m_PointDimension, &outputFile);
    outputFile << "\n";

    return;
  }

  template <typename T>
  void
  WriteCellsBufferAsASCII(std::ofstream & outputFile, T * buffer)
  {
    MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
    unsigned int         numberOfVertices = 0;
    unsigned int         numberOfVertexIndices = 0;
    unsigned int         numberOfLines = 0;
    unsigned int         numberOfLineIndices = 0;
    unsigned int         numberOfPolygons = 0;
    unsigned int         numberOfPolygonIndices = 0;

    /** Write vertices */
    SizeValueType index = 0;

    ExposeMetaData<unsigned int>(metaDic, "numberOfVertices", numberOfVertices);
    if (numberOfVertices)
    {
      ExposeMetaData<unsigned int>(metaDic, "numberOfVertexIndices", numberOfVertexIndices);
      outputFile << "VERTICES " << numberOfVertices << " " << numberOfVertexIndices << '\n';
      for (SizeValueType ii = 0; ii < this->m_NumberOfCells; ++ii)
      {
        auto cellType = static_cast<CellGeometryEnum>(static_cast<int>(buffer[index++]));
        auto nn = static_cast<unsigned int>(buffer[index++]);
        if (cellType == CellGeometryEnum::VERTEX_CELL)
        {
          outputFile << nn;
          for (unsigned int jj = 0; jj < nn; ++jj)
          {
            outputFile << " " << buffer[index++];
          }
          outputFile << '\n';
        }
        else
        {
          index += nn;
        }
      }
    }

    /** Write lines */
    index = 0;
    ExposeMetaData<unsigned int>(metaDic, "numberOfLines", numberOfLines);
    if (numberOfLines)
    {
      numberOfLineIndices = 0;
      SizeValueType             numberOfPolylines = 0;
      PolylinesContainerPointer polylines = PolylinesContainerType::New();
      PointIdVector             pointIds;
      for (SizeValueType ii = 0; ii < this->m_NumberOfCells; ++ii)
      {
        auto cellType = static_cast<CellGeometryEnum>(static_cast<int>(buffer[index++]));
        auto nn = static_cast<unsigned int>(buffer[index++]);

        pointIds.clear();
        if (cellType == CellGeometryEnum::LINE_CELL)
        {
          pointIds.push_back(static_cast<SizeValueType>(buffer[index]));
          pointIds.push_back(static_cast<SizeValueType>(buffer[index + 1]));
        }
        else if (cellType == CellGeometryEnum::POLYLINE_CELL)
        {
          for (unsigned int jj = 0; jj < nn; ++jj)
          {
            pointIds.push_back(static_cast<SizeValueType>(buffer[index + jj]));
          }
        }

        polylines->InsertElement(numberOfPolylines++, pointIds);
        numberOfLineIndices += pointIds.size();
        index += nn;
      }

      numberOfLines = polylines->Size();
      numberOfLineIndices += numberOfLines;
      EncapsulateMetaData<unsigned int>(metaDic, "numberOfLines", numberOfLines);
      EncapsulateMetaData<unsigned int>(metaDic, "numberOfLineIndices", numberOfLineIndices);
      outputFile << "LINES " << numberOfLines << " " << numberOfLineIndices << '\n';
      for (SizeValueType ii = 0; ii < polylines->Size(); ++ii)
      {
        auto nn = static_cast<unsigned int>(polylines->ElementAt(ii).size());
        outputFile << nn;
        for (unsigned int jj = 0; jj < nn; ++jj)
        {
          outputFile << " " << polylines->ElementAt(ii)[jj];
        }
        outputFile << '\n';
      }
    }

    /** Write polygons */
    index = 0;
    ExposeMetaData<unsigned int>(metaDic, "numberOfPolygons", numberOfPolygons);
    if (numberOfPolygons)
    {
      ExposeMetaData<unsigned int>(metaDic, "numberOfPolygonIndices", numberOfPolygonIndices);
      outputFile << "POLYGONS " << numberOfPolygons << " " << numberOfPolygonIndices << '\n';
      for (SizeValueType ii = 0; ii < this->m_NumberOfCells; ++ii)
      {
        auto cellType = static_cast<CellGeometryEnum>(static_cast<int>(buffer[index++]));
        auto nn = static_cast<unsigned int>(buffer[index++]);
        if (cellType == CellGeometryEnum::POLYGON_CELL || cellType == CellGeometryEnum::TRIANGLE_CELL ||
            cellType == CellGeometryEnum::QUADRILATERAL_CELL)
        {
          outputFile << nn;
          for (unsigned int jj = 0; jj < nn; ++jj)
          {
            outputFile << " " << buffer[index++];
          }
          outputFile << '\n';
        }
        else
        {
          index += nn;
        }
      }
    }
  }

  template <typename T>
  void
  WriteCellsBufferAsBINARY(std::ofstream & outputFile, T * buffer)
  {
    MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
    unsigned int         numberOfVertices = 0;
    unsigned int         numberOfVertexIndices = 0;
    unsigned int         numberOfLines = 0;
    unsigned int         numberOfLineIndices = 0;
    unsigned int         numberOfPolygons = 0;
    unsigned int         numberOfPolygonIndices = 0;

    /** Write vertices */
    SizeValueType index = 0;

    ExposeMetaData<unsigned int>(metaDic, "numberOfVertices", numberOfVertices);
    if (numberOfVertices)
    {
      ExposeMetaData<unsigned int>(metaDic, "numberOfVertexIndices", numberOfVertexIndices);
      outputFile << "VERTICES " << numberOfVertices << " " << numberOfVertexIndices << '\n';
      const auto data = make_unique_for_overwrite<unsigned int[]>(numberOfVertexIndices);
      ReadCellsBuffer(buffer, data.get());
      itk::ByteSwapper<unsigned int>::SwapWriteRangeFromSystemToBigEndian(
        data.get(), numberOfVertexIndices, &outputFile);
      outputFile << "\n";
    }

    /** Write lines */
    index = 0;
    ExposeMetaData<unsigned int>(metaDic, "numberOfLines", numberOfLines);
    if (numberOfLines)
    {
      numberOfLineIndices = 0;
      SizeValueType             numberOfPolylines = 0;
      PolylinesContainerPointer polylines = PolylinesContainerType::New();
      PointIdVector             pointIds;
      for (SizeValueType ii = 0; ii < this->m_NumberOfCells; ++ii)
      {
        auto cellType = static_cast<CellGeometryEnum>(static_cast<int>(buffer[index++]));
        auto nn = static_cast<unsigned int>(buffer[index++]);
        pointIds.clear();

        if (cellType == CellGeometryEnum::LINE_CELL)
        {
          pointIds.push_back(static_cast<SizeValueType>(buffer[index]));
          pointIds.push_back(static_cast<SizeValueType>(buffer[index + 1]));
        }
        else if (cellType == CellGeometryEnum::POLYLINE_CELL)
        {
          for (unsigned int jj = 0; jj < nn; ++jj)
          {
            pointIds.push_back(static_cast<SizeValueType>(buffer[index + jj]));
          }
        }
        polylines->InsertElement(numberOfPolylines++, pointIds);
        numberOfLineIndices += pointIds.size();
        index += nn;
      }

      numberOfLines = polylines->Size();
      numberOfLineIndices += numberOfLines;
      EncapsulateMetaData<unsigned int>(metaDic, "numberOfLines", numberOfLines);
      EncapsulateMetaData<unsigned int>(metaDic, "numberOfLineIndices", numberOfLineIndices);

      outputFile << "LINES " << numberOfLines << " " << numberOfLineIndices << '\n';
      const auto    data = make_unique_for_overwrite<unsigned int[]>(numberOfLineIndices);
      unsigned long outputIndex = 0;
      for (SizeValueType ii = 0; ii < polylines->Size(); ++ii)
      {
        auto nn = static_cast<unsigned int>(polylines->ElementAt(ii).size());
        data[outputIndex++] = nn;
        for (unsigned int jj = 0; jj < nn; ++jj)
        {
          data[outputIndex++] = polylines->ElementAt(ii)[jj];
        }
      }

      itk::ByteSwapper<unsigned int>::SwapWriteRangeFromSystemToBigEndian(data.get(), numberOfLineIndices, &outputFile);
      outputFile << "\n";
    }

    /** Write polygons */
    index = 0;
    ExposeMetaData<unsigned int>(metaDic, "numberOfPolygons", numberOfPolygons);
    if (numberOfPolygons)
    {
      ExposeMetaData<unsigned int>(metaDic, "numberOfPolygonIndices", numberOfPolygonIndices);
      outputFile << "POLYGONS " << numberOfPolygons << " " << numberOfPolygonIndices << '\n';
      const auto data = make_unique_for_overwrite<unsigned int[]>(numberOfPolygonIndices);
      ReadCellsBuffer(buffer, data.get());
      itk::ByteSwapper<unsigned int>::SwapWriteRangeFromSystemToBigEndian(
        data.get(), numberOfPolygonIndices, &outputFile);
      outputFile << "\n";
    }
  }

  template <typename T>
  void
  WritePointDataBufferAsASCII(std::ofstream & outputFile, T * buffer, const StringType & pointPixelComponentName)
  {
    MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
    StringType           dataName;

    outputFile << "POINT_DATA " << this->m_NumberOfPointPixels << '\n';
    switch (this->m_PointPixelType)
    {
      case IOPixelEnum::SCALAR:
      {
        outputFile << "SCALARS ";
        ExposeMetaData<StringType>(metaDic, "pointScalarDataName", dataName);
        outputFile << dataName << "  ";
        break;
      }
      case IOPixelEnum::OFFSET:
      case IOPixelEnum::POINT:
      case IOPixelEnum::COVARIANTVECTOR:
      case IOPixelEnum::VECTOR:
      {
        outputFile << "VECTORS ";
        ExposeMetaData<StringType>(metaDic, "pointVectorDataName", dataName);
        outputFile << dataName << "  ";
        break;
      }
      case IOPixelEnum::SYMMETRICSECONDRANKTENSOR:
      case IOPixelEnum::DIFFUSIONTENSOR3D:
      {
        outputFile << "TENSORS ";
        ExposeMetaData<StringType>(metaDic, "pointTensorDataName", dataName);
        outputFile << dataName << "  ";
        break;
      }
      case IOPixelEnum::ARRAY:
      case IOPixelEnum::VARIABLELENGTHVECTOR:
      {
        outputFile << "COLOR_SCALARS ";
        ExposeMetaData<StringType>(metaDic, "pointColorScalarDataName", dataName);
        outputFile << dataName << "  ";
        WriteColorScalarBufferAsASCII(
          outputFile, buffer, this->m_NumberOfPointPixelComponents, this->m_NumberOfPointPixels);
        return;
      }
      default:
      {
        itkExceptionMacro(<< "Unknown point pixel type");
      }
    }

    outputFile << pointPixelComponentName << '\n';

    if (this->m_PointPixelType == IOPixelEnum::SCALAR)
    {
      outputFile << "LOOKUP_TABLE default" << '\n';
    }

    Indent indent(2);
    if (this->m_PointPixelType == IOPixelEnum::SYMMETRICSECONDRANKTENSOR)
    {
      T *                 ptr = buffer;
      SizeValueType       i = 0;
      const SizeValueType num = this->m_NumberOfPointPixelComponents * this->m_NumberOfPointPixels;
      // Note that only the 3D tensors are supported in the VTK File Format
      // documentation.
      if (this->m_NumberOfPointPixelComponents == 3)
      {
        T zero(itk::NumericTraits<T>::ZeroValue());
        T e12;
        while (i < num)
        {
          // row 1
          outputFile << ConvertNumberToString(*ptr++) << indent;
          e12 = *ptr++;
          outputFile << ConvertNumberToString(e12) << indent;
          outputFile << ConvertNumberToString(zero) << '\n';
          // row 2
          outputFile << ConvertNumberToString(e12) << indent;
          outputFile << ConvertNumberToString(*ptr++) << indent;
          outputFile << ConvertNumberToString(zero) << '\n';
          // row 3
          outputFile << ConvertNumberToString(zero) << indent << ConvertNumberToString(zero) << indent
                     << ConvertNumberToString(zero) << "\n\n";
          i += 3;
        }
      }
      else if (this->m_NumberOfPointPixelComponents == 6)
      {
        T e12;
        T e13;
        T e23;
        while (i < num)
        {
          // row 1
          outputFile << ConvertNumberToString(*ptr++) << indent;
          e12 = *ptr++;
          outputFile << ConvertNumberToString(e12) << indent;
          e13 = *ptr++;
          outputFile << ConvertNumberToString(e13) << '\n';
          // row 2
          outputFile << ConvertNumberToString(e12) << indent;
          outputFile << ConvertNumberToString(*ptr++) << indent;
          e23 = *ptr++;
          outputFile << ConvertNumberToString(e23) << '\n';
          // row 3
          outputFile << ConvertNumberToString(e13) << indent;
          outputFile << ConvertNumberToString(e23) << indent;
          outputFile << ConvertNumberToString(*ptr++) << "\n\n";
          i += 6;
        }
      }
      else
      {
        itk::ExceptionObject e_(
          __FILE__, __LINE__, "itk::ERROR: VTKImageIO2: Unsupported number of components in tensor.", ITK_LOCATION);
        throw e_;
      }
    }
    else // not tensor
    {
      unsigned int jj;
      for (SizeValueType ii = 0; ii < this->m_NumberOfPointPixels; ++ii)
      {
        for (jj = 0; jj < this->m_NumberOfPointPixelComponents - 1; ++jj)
        {
          outputFile << ConvertNumberToString(buffer[ii * this->m_NumberOfPointPixelComponents + jj]) << indent;
        }
        outputFile << ConvertNumberToString(buffer[ii * this->m_NumberOfPointPixelComponents + jj]);
        outputFile << '\n';
      }
    }

    return;
  }

  template <typename T>
  void
  WritePointDataBufferAsBINARY(std::ofstream & outputFile, T * buffer, const StringType & pointPixelComponentName)
  {
    MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
    StringType           dataName;

    outputFile << "POINT_DATA " << this->m_NumberOfPointPixels << "\n";
    switch (this->m_PointPixelType)
    {
      case IOPixelEnum::SCALAR:
      {
        outputFile << "SCALARS ";
        ExposeMetaData<StringType>(metaDic, "pointScalarDataName", dataName);
        outputFile << dataName << "  ";
        break;
      }
      case IOPixelEnum::OFFSET:
      case IOPixelEnum::POINT:
      case IOPixelEnum::COVARIANTVECTOR:
      case IOPixelEnum::VECTOR:
      {
        outputFile << "VECTORS ";
        ExposeMetaData<StringType>(metaDic, "pointVectorDataName", dataName);
        outputFile << dataName << "  ";
        break;
      }
      case IOPixelEnum::SYMMETRICSECONDRANKTENSOR:
      case IOPixelEnum::DIFFUSIONTENSOR3D:
      {
        outputFile << "TENSORS ";
        ExposeMetaData<StringType>(metaDic, "pointTensorDataName", dataName);
        outputFile << dataName << "  ";
        break;
      }
      case IOPixelEnum::ARRAY:
      case IOPixelEnum::VARIABLELENGTHVECTOR:
      {
        outputFile << "COLOR_SCALARS ";
        ExposeMetaData<StringType>(metaDic, "pointColorScalarDataName", dataName);
        outputFile << dataName << "  ";
        WriteColorScalarBufferAsBINARY(
          outputFile, buffer, this->m_NumberOfPointPixelComponents, this->m_NumberOfPointPixels);
        return;
      }
      default:
      {
        itkExceptionMacro(<< "Unknown point pixel type");
      }
    }

    outputFile << pointPixelComponentName << "\n";
    if (this->m_PointPixelType == IOPixelEnum::SCALAR)
    {
      outputFile << "LOOKUP_TABLE default\n";
    }

    itk::ByteSwapper<T>::SwapWriteRangeFromSystemToBigEndian(
      buffer, this->m_NumberOfPointPixels * this->m_NumberOfPointPixelComponents, &outputFile);
    outputFile << "\n";
    return;
  }

  template <typename T>
  void
  WriteCellDataBufferAsASCII(std::ofstream & outputFile, T * buffer, const StringType & cellPixelComponentName)
  {
    MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
    StringType           dataName;

    outputFile << "CELL_DATA " << this->m_NumberOfCellPixels << '\n';
    switch (this->m_CellPixelType)
    {
      case IOPixelEnum::SCALAR:
      {
        outputFile << "SCALARS ";
        ExposeMetaData<StringType>(metaDic, "cellScalarDataName", dataName);
        outputFile << dataName << "  ";
        break;
      }
      case IOPixelEnum::OFFSET:
      case IOPixelEnum::POINT:
      case IOPixelEnum::COVARIANTVECTOR:
      case IOPixelEnum::VECTOR:
      {
        outputFile << "VECTORS ";
        ExposeMetaData<StringType>(metaDic, "cellVectorDataName", dataName);
        outputFile << dataName << "  ";
        break;
      }
      case IOPixelEnum::SYMMETRICSECONDRANKTENSOR:
      case IOPixelEnum::DIFFUSIONTENSOR3D:
      {
        outputFile << "TENSORS ";
        ExposeMetaData<StringType>(metaDic, "cellTensorDataName", dataName);
        outputFile << dataName << "  ";
        break;
      }
      case IOPixelEnum::ARRAY:
      case IOPixelEnum::VARIABLELENGTHVECTOR:
      {
        outputFile << "COLOR_SCALARS ";
        ExposeMetaData<StringType>(metaDic, "cellColorScalarDataName", dataName);
        outputFile << dataName << "  ";
        WriteColorScalarBufferAsASCII(
          outputFile, buffer, this->m_NumberOfCellPixelComponents, this->m_NumberOfCellPixels);
        return;
      }
      default:
      {
        itkExceptionMacro(<< "Unknown cell pixel type");
      }
    }

    outputFile << cellPixelComponentName << '\n';
    if (this->m_CellPixelType == IOPixelEnum::SCALAR)
    {
      outputFile << "LOOKUP_TABLE default" << '\n';
    }

    Indent indent(2);
    if (this->m_CellPixelType == IOPixelEnum::SYMMETRICSECONDRANKTENSOR)
    {
      T *                 ptr = buffer;
      SizeValueType       i = 0;
      const SizeValueType num = this->m_NumberOfCellPixelComponents * this->m_NumberOfCellPixels;
      if (this->m_NumberOfCellPixelComponents == 2)
      {
        T zero(itk::NumericTraits<T>::ZeroValue());
        T e12;
        while (i < num)
        {
          // row 1
          outputFile << *ptr++ << indent;
          e12 = *ptr++;
          outputFile << e12 << indent;
          outputFile << zero << '\n';
          // row 2
          outputFile << e12 << indent;
          outputFile << *ptr++ << indent;
          outputFile << zero << '\n';
          // row 3
          outputFile << zero << indent << zero << indent << zero << "\n\n";
          i += 3;
        }
      }
      else if (this->m_NumberOfCellPixelComponents == 3)
      {
        T e12;
        T e13;
        T e23;
        while (i < num)
        {
          // row 1
          outputFile << *ptr++ << indent;
          e12 = *ptr++;
          outputFile << e12 << indent;
          e13 = *ptr++;
          outputFile << e13 << '\n';
          // row 2
          outputFile << e12 << indent;
          outputFile << *ptr++ << indent;
          e23 = *ptr++;
          outputFile << e23 << '\n';
          // row 3
          outputFile << e13 << indent;
          outputFile << e23 << indent;
          outputFile << *ptr++ << "\n\n";
          i += 6;
        }
      }
      else
      {
        ExceptionObject e_(__FILE__,
                           __LINE__,
                           "itk::ERROR: VTKPolyDataMeshIO: Unsupported number of components in tensor.",
                           ITK_LOCATION);
        throw e_;
      }
    }
    else // not tensor
    {
      unsigned int jj;
      for (SizeValueType ii = 0; ii < this->m_NumberOfCellPixels; ++ii)
      {
        for (jj = 0; jj < this->m_NumberOfCellPixelComponents - 1; ++jj)
        {
          outputFile << buffer[ii * this->m_NumberOfCellPixelComponents + jj] << indent;
        }
        outputFile << buffer[ii * this->m_NumberOfCellPixelComponents + jj];
        outputFile << '\n';
      }
    }

    return;
  }

  template <typename T>
  void
  WriteCellDataBufferAsBINARY(std::ofstream & outputFile, T * buffer, const StringType & cellPixelComponentName)
  {
    MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
    StringType           dataName;

    outputFile << "CELL_DATA " << this->m_NumberOfCellPixels << "\n";
    switch (this->m_CellPixelType)
    {
      case IOPixelEnum::SCALAR:
      {
        outputFile << "SCALARS ";
        ExposeMetaData<StringType>(metaDic, "cellScalarDataName", dataName);
        outputFile << dataName << "  ";
        break;
      }
      case IOPixelEnum::OFFSET:
      case IOPixelEnum::POINT:
      case IOPixelEnum::COVARIANTVECTOR:
      case IOPixelEnum::VECTOR:
      {
        outputFile << "VECTORS ";
        ExposeMetaData<StringType>(metaDic, "cellVectorDataName", dataName);
        outputFile << dataName << "  ";
        break;
      }
      case IOPixelEnum::SYMMETRICSECONDRANKTENSOR:
      case IOPixelEnum::DIFFUSIONTENSOR3D:
      {
        outputFile << "TENSORS ";
        ExposeMetaData<StringType>(metaDic, "cellTensorDataName", dataName);
        outputFile << dataName << "  ";
        break;
      }
      case IOPixelEnum::ARRAY:
      case IOPixelEnum::VARIABLELENGTHVECTOR:
      {
        outputFile << "COLOR_SCALARS ";
        ExposeMetaData<StringType>(metaDic, "cellColorScalarDataName", dataName);
        outputFile << dataName << "  ";
        WriteColorScalarBufferAsBINARY(
          outputFile, buffer, this->m_NumberOfCellPixelComponents, this->m_NumberOfCellPixels);
        return;
      }
      default:
      {
        itkExceptionMacro(<< "Unknown cell pixel type");
      }
    }

    outputFile << cellPixelComponentName << "\n";
    if (this->m_CellPixelType == IOPixelEnum::SCALAR)
    {
      outputFile << "LOOKUP_TABLE default\n";
    }

    itk::ByteSwapper<T>::SwapWriteRangeFromSystemToBigEndian(
      buffer, this->m_NumberOfCells * this->m_NumberOfCellPixelComponents, &outputFile);
    outputFile << "\n";
    return;
  }

  template <typename T>
  void
  WriteColorScalarBufferAsASCII(std::ofstream & outputFile,
                                T *             buffer,
                                unsigned int    numberOfPixelComponents,
                                SizeValueType   numberOfPixels)
  {
    outputFile << numberOfPixelComponents << "\n";
    Indent indent(2);
    for (SizeValueType ii = 0; ii < numberOfPixels; ++ii)
    {
      for (unsigned int jj = 0; jj < numberOfPixelComponents; ++jj)
      {
        outputFile << ConvertNumberToString(static_cast<float>(buffer[ii * numberOfPixelComponents + jj])) << indent;
      }

      outputFile << "\n";
    }

    return;
  }

  template <typename T>
  void
  WriteColorScalarBufferAsBINARY(std::ofstream & outputFile,
                                 T *             buffer,
                                 unsigned int    numberOfPixelComponents,
                                 SizeValueType   numberOfPixels)
  {
    outputFile << numberOfPixelComponents << "\n";
    SizeValueType numberOfElements = numberOfPixelComponents * numberOfPixels;
    const auto    data = make_unique_for_overwrite<unsigned char[]>(numberOfElements);
    for (SizeValueType ii = 0; ii < numberOfElements; ++ii)
    {
      data[ii] = static_cast<unsigned char>(buffer[ii]);
    }

    outputFile.write(reinterpret_cast<char *>(data.get()), numberOfElements);
    outputFile << "\n";
    return;
  }

  /** Convert cells buffer for output cells buffer, it's user's responsibility to make sure
  the input cells don't contain any cell type that coule not be written as polygon cell */
  template <typename TInput, typename TOutput>
  void
  ReadCellsBuffer(TInput * input, TOutput * output)
  {
    SizeValueType inputIndex = 0;
    SizeValueType outputIndex = 0;

    if (input && output)
    {
      for (SizeValueType ii = 0; ii < this->m_NumberOfCells; ++ii)
      {
        ++inputIndex;
        auto nn = static_cast<unsigned int>(input[inputIndex++]);
        output[outputIndex++] = nn;
        for (unsigned int jj = 0; jj < nn; ++jj)
        {
          output[outputIndex++] = static_cast<TOutput>(input[inputIndex++]);
        }
      }
    }
  }

  /** Convenience method returns the IOComponentEnum corresponding to a string. */
  IOComponentEnum
  GetComponentTypeFromString(const std::string & pointType);

private:
  /** Reads the specified number of components from the specified input file into the specified buffer.
   * \note This member function is overloaded for `float` and `double`, in order to support reading infinity and NaN
   * values.
   */
  template <typename T>
  static void
  ReadComponentsAsASCII(std::ifstream & inputFile, T * const buffer, const SizeValueType numberOfComponents)
  {
    for (SizeValueType i = 0; i < numberOfComponents; ++i)
    {
      if (!(inputFile >> buffer[i]))
      {
        itkGenericExceptionMacro("Failed to read a component from the specified ASCII input file!");
      }
    }
  }

  static void
  ReadComponentsAsASCII(std::ifstream & inputFile, float * const buffer, const SizeValueType numberOfComponents);

  static void
  ReadComponentsAsASCII(std::ifstream & inputFile, double * const buffer, const SizeValueType numberOfComponents);
};
} // end namespace itk

#endif // itkVTKPolyDataMeshIO_h
