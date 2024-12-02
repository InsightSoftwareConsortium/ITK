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
#ifndef itkMZ3MeshIO_h
#define itkMZ3MeshIO_h
#include "IOMeshMZ3Export.h"

#include "itkMeshIOBase.h"
#include "itk_zlib.h"

#include <fstream>

namespace itk
{
class MZ3MeshIOInternals;

/** \class MZ3MeshIO
 *
 * \brief Read and write the MZ3 triangle mesh file format.
 *
 * The MZ3 file format is a binary file format for storing triangle meshes.
 *
 * The file format is as follows:
 *
 * - 2 bytes: magic number 0x4D5A
 * - 2 bytes: attributes
 * - 4 bytes: number of faces
 * - 4 bytes: number of vertices
 * - 4 bytes: number of bytes to skip
 * - 12 bytes per face: face indices
 * - 12 bytes per vertex: vertex coordinates
 * - 4 bytes per vertex: vertex colors (optional)
 * - 4 bytes per vertex: vertex scalars (optional)
 * - 8 bytes per vertex: vertex scalars (optional)
 *
 * This implementation currently only supports reading and writing from little endian systems.
 *
 * \ingroup IOFilters
 * \ingroup IOMeshMZ3
 *
 */
class IOMeshMZ3_EXPORT MZ3MeshIO : public MeshIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MZ3MeshIO);

  /** Standard class type aliases. */
  using Self = MZ3MeshIO;
  using Superclass = MeshIOBase;
  using ConstPointer = SmartPointer<const Self>;
  using Pointer = SmartPointer<Self>;

  using StreamOffsetType = Superclass::StreamOffsetType;
  using SizeValueType = Superclass::SizeValueType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(MZ3MeshIO);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this MeshIO implementation.
   * \param fileName The name of the file to test for reading.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can read the file specified.
   */
  bool
  CanReadFile(const char * fileName) override;

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
protected:
  MZ3MeshIO();
  ~MZ3MeshIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  class MZ3MeshIOInternals
  {
  public:
    gzFile             m_GzFile;
    uint16_t           m_Attributes{ 0 };
    uint32_t           m_Skip{ 0 };
    std::vector<float> m_VertexBuffer;
  };

  template <typename T>
  void
  WritePoints(T * buffer)
  {
    SizeValueType index{};
    float         component{};

    if (m_IsCompressed)
    {
      // Write vertex coordinates
      for (SizeValueType ii = 0; ii < this->m_NumberOfPoints; ++ii)
      {
        for (unsigned int jj = 0; jj < 3; ++jj)
        {
          component = static_cast<float>(buffer[index]);
          // Copy for deferred writing
          m_Internal->m_VertexBuffer[index] = component;
          ++index;
        }
      }
    }
    else
    {
      // Skip header and optional skip bytes
      m_Ofstream.seekp(16 + m_Internal->m_Skip);
      // Skip faces if present
      if (m_Internal->m_Attributes & 1)
      {
        m_Ofstream.seekp(m_NumberOfCells * 12, std::ios::cur);
      }
      // Write vertex coordinates
      for (SizeValueType ii = 0; ii < this->m_NumberOfPoints; ++ii)
      {
        for (unsigned int jj = 0; jj < 3; ++jj)
        {
          component = static_cast<float>(buffer[index++]);
          m_Ofstream.write(reinterpret_cast<char *>(&component), sizeof(float));
        }
      }
    }
  }

  template <typename T>
  void
  WriteCells(T * buffer)
  {
    SizeValueType index{};
    uint32_t      component;

    if (m_IsCompressed)
    {
      // Write face indices
      for (SizeValueType i = 0; i < m_NumberOfCells; ++i)
      {
        const auto cellType = buffer[index++];
        const auto numberOfPoints = buffer[index++];
        if (numberOfPoints == 3)
        {
          for (unsigned int jj = 0; jj < 3; ++jj)
          {
            component = static_cast<uint32_t>(buffer[index++]);
            gzwrite(m_Internal->m_GzFile, &component, sizeof(uint32_t));
          }
        }
        else
        {
          itkExceptionMacro("Only triangles are supported");
          // index += numberOfPoints;
        }
      }
      // Write vertex coordinates
      gzwrite(m_Internal->m_GzFile, m_Internal->m_VertexBuffer.data(), m_NumberOfPoints * 3 * sizeof(float));
    }
    else
    {
      // Skip header and optional skip bytes
      m_Ofstream.seekp(16 + m_Internal->m_Skip);
      // Write face indices
      for (SizeValueType i = 0; i < m_NumberOfCells; ++i)
      {
        const auto cellType = buffer[index++];
        const auto numberOfPoints = buffer[index++];
        if (numberOfPoints == 3)
        {
          for (unsigned int jj = 0; jj < 3; ++jj)
          {
            component = static_cast<uint32_t>(buffer[index++]);
            m_Ofstream.write(reinterpret_cast<char *>(&component), sizeof(uint32_t));
          }
        }
        else
        {
          itkExceptionMacro("Only triangles are supported");
          // index += numberOfPoints;
        }
      }
    }
  }

  template <typename T>
  void
  WritePointData(T * buffer)
  {
    SizeValueType index{};
    float         component{};

    if (m_IsCompressed)
    {
      for (SizeValueType ii = 0; ii < this->m_NumberOfPointPixels; ++ii)
      {
        component = static_cast<float>(buffer[index++]);
        gzwrite(m_Internal->m_GzFile, &component, sizeof(float));
      }
    }
    else
    {
      for (SizeValueType ii = 0; ii < this->m_NumberOfPointPixels; ++ii)
      {
        component = static_cast<float>(buffer[index++]);
        m_Ofstream.write(reinterpret_cast<char *>(&component), sizeof(float));
      }
    }
  }

private:
  std::ifstream m_Ifstream{};
  std::ofstream m_Ofstream{};
  bool          m_IsCompressed{};

  const std::unique_ptr<MZ3MeshIOInternals> m_Internal;
};
} // end namespace itk

#endif
