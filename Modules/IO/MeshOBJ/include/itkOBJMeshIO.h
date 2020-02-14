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

#ifndef itkOBJMeshIO_h
#define itkOBJMeshIO_h
#include "ITKIOMeshOBJExport.h"

#include "itkMeshIOBase.h"
#include "itkNumberToString.h"
#include <fstream>

namespace itk
{
/**
 *\class OBJMeshIO
 * \brief This class defines how to read and write Object file format.
 * \ingroup IOFilters
 * \ingroup ITKIOMeshOBJ
 */

class ITKIOMeshOBJ_EXPORT OBJMeshIO : public MeshIOBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(OBJMeshIO);

  /** Standard class type aliases. */
  using Self = OBJMeshIO;
  using Superclass = MeshIOBase;
  using ConstPointer = SmartPointer<const Self>;
  using Pointer = SmartPointer<Self>;

  using SizeValueType = Superclass::SizeValueType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(OBJMeshIO, MeshIOBase);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this MeshIO implementation.
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can read the file specified.
   */
  bool
  CanReadFile(const char * FileNameToRead) override;

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
   * \param FileNameToWrite The name of the file to test for writing.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can write the file specified.
   */
  bool
  CanWriteFile(const char * FileNameToWrite) override;

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
  /** Write points to output stream */
  template <typename T>
  void
  WritePoints(T * buffer, std::ofstream & outputFile)
  {
    NumberToString<T> convert;
    SizeValueType     index = itk::NumericTraits<SizeValueType>::ZeroValue();

    for (SizeValueType ii = 0; ii < this->m_NumberOfPoints; ii++)
    {
      outputFile << "v ";
      for (unsigned int jj = 0; jj < this->m_PointDimension; jj++)
      {
        outputFile << convert(buffer[index++]) << "  ";
      }
      outputFile << '\n';
    }
  }

  template <typename T>
  void
  WriteCells(T * buffer, std::ofstream & outputFile)
  {
    SizeValueType index = itk::NumericTraits<SizeValueType>::ZeroValue();

    for (SizeValueType ii = 0; ii < this->m_NumberOfCells; ii++)
    {
      outputFile << "f ";
      index++;
      auto numberOfCellPoints = static_cast<unsigned int>(buffer[index++]);

      for (unsigned int jj = 0; jj < numberOfCellPoints; jj++)
      {
        outputFile << buffer[index++] + 1 << "  ";
      }
      outputFile << '\n';
    }
  }

  /** Write point data to output stream */
  template <typename T>
  void
  WritePointData(T * buffer, std::ofstream & outputFile)
  {
    NumberToString<T> convert;
    SizeValueType     index = itk::NumericTraits<SizeValueType>::ZeroValue();

    for (SizeValueType ii = 0; ii < this->m_NumberOfPointPixels; ii++)
    {
      outputFile << "vn ";
      for (unsigned int jj = 0; jj < this->m_PointDimension; jj++)
      {
        outputFile << convert(buffer[index++]) << "  ";
      }

      outputFile << '\n';
    }
  }

  static bool
  SplitLine(const std::string & line, std::string & type, std::string & content);

protected:
  OBJMeshIO();
  ~OBJMeshIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  OpenFile();

  void
  CloseFile();

private:
  std::ifstream  m_InputFile;
  std::streampos m_PointsStartPosition; // file position for points rlative to
                                        // std::ios::beg
};
} // end namespace itk

#endif
