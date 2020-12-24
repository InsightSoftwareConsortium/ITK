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

#ifndef itkBYUMeshIO_h
#define itkBYUMeshIO_h
#include "ITKIOMeshBYUExport.h"

#include "itkMeshIOBase.h"
#include "itkNumberToString.h"

#include <fstream>

namespace itk
{
/**
 *\class BYUMeshIO
 * \brief This class defines how to read and write BYU Geometry File Format.
 *
 * \ingroup IOFilters
 * \ingroup ITKIOMeshBYU
 */

class ITKIOMeshBYU_EXPORT BYUMeshIO : public MeshIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BYUMeshIO);

  /** Standard class type aliases. */
  using Self = BYUMeshIO;
  using Superclass = MeshIOBase;
  using ConstPointer = SmartPointer<const Self>;
  using Pointer = SmartPointer<Self>;

  using StreamOffsetType = Superclass::StreamOffsetType;
  using SizeValueType = Superclass::SizeValueType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BYUMeshIO, MeshIOBase);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this MeshIO implementation.
   * \param FileNameToRead The name of the file to test for reading.
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
   * \param FileNameToWrite The name of the file to test for writing.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can write the file specified.
   */
  bool
  CanWriteFile(const char * fileName) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  WriteMeshInformation() override;

  /** Writes the data to disk from the memory buffer provided. */
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
    Indent            indent(1);
    SizeValueType     index = itk::NumericTraits<SizeValueType>::ZeroValue();

    for (SizeValueType ii = 0; ii < this->m_NumberOfPoints; ii++)
    {
      outputFile << indent;
      for (unsigned int jj = 0; jj < this->m_PointDimension; jj++)
      {
        outputFile << convert(buffer[index++]) << " ";
      }
      outputFile << '\n';
    }
  }

  template <typename T>
  void
  WriteCells(T * buffer, std::ofstream & outputFile)
  {
    Indent        indent(7);
    SizeValueType index = itk::NumericTraits<SizeValueType>::ZeroValue();

    for (SizeValueType ii = 0; ii < this->m_NumberOfCells; ii++)
    {
      auto numberOfCellPoints = static_cast<unsigned int>(buffer[++index]);
      index++;
      for (unsigned int jj = 0; jj < numberOfCellPoints - 1; jj++)
      {
        outputFile << indent << buffer[index++] + 1;
      }

      outputFile << indent << -static_cast<long long>(buffer[index++] + 1) << '\n';
    }
  }

protected:
  BYUMeshIO();
  ~BYUMeshIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  StreamOffsetType m_FilePosition{ 0 };
  SizeValueType    m_PartId;
  SizeValueType    m_FirstCellId;
  SizeValueType    m_LastCellId;
};
} // end namespace itk

#endif
