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

#ifndef itkSWCMeshIO_h
#define itkSWCMeshIO_h
#include "IOMeshSWCExport.h"

#include "itkMeshIOBase.h"
#include "itkVectorContainer.h"

#include <fstream>
#include <unordered_map>
#include <algorithm>

namespace itk
{

/** \class SWCMeshIOEnums
 *
 * \brief enums for the SWCMeshIO class.
 *
 * \ingroup IOMeshSWC
 */
class SWCMeshIOEnums
{
public:
  /** \class SWCPointData
   * \ingroup IOMeshSWC
   */
  enum class SWCPointData : uint8_t
  {
    SampleIdentifier = 0,
    TypeIdentifier,
    Radius,
    ParentIdentifier
  };
};
extern IOMeshSWC_EXPORT std::ostream &
operator<<(std::ostream & out, const SWCMeshIOEnums::SWCPointData value);

/**
 *\class SWCMeshIO
 * \brief This class defines how to read and write SWC neuron morphology files.
 *
 * \ingroup IOFilters
 * \ingroup IOMeshSWC
 */

class IOMeshSWC_EXPORT SWCMeshIO : public MeshIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SWCMeshIO);

  /** Standard class type aliases. */
  using Self = SWCMeshIO;
  using Superclass = MeshIOBase;
  using ConstPointer = SmartPointer<const Self>;
  using Pointer = SmartPointer<Self>;

  using StreamOffsetType = Superclass::StreamOffsetType;
  using SizeValueType = Superclass::SizeValueType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(SWCMeshIO);

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

  using HeaderContentType = std::vector<std::string>;
  void
  SetHeaderContent(const HeaderContentType & headerContent);
  itkGetConstReferenceMacro(HeaderContent, HeaderContentType);

  // using SampleIdentifierType = int16_t;
  // For Python wrapping
  using SampleIdentifierType = float;
  // using TypeIdentifierType = uint8_t;
  using TypeIdentifierType = float;
  using RadiusType = double;
  // using ParentIdentifierType = int16_t;
  // For Python wrapping
  using ParentIdentifierType = float;

  using SampleIdentifierContainerType = VectorContainer<IdentifierType, SampleIdentifierType>;
  using TypeIdentifierContainerType = VectorContainer<IdentifierType, TypeIdentifierType>;
  using RadiusContainerType = VectorContainer<IdentifierType, RadiusType>;
  using ParentIdentifierContainerType = VectorContainer<IdentifierType, ParentIdentifierType>;

  /** Set/Get the sample identifiers. */
  void
  SetSampleIdentifiers(const SampleIdentifierContainerType *);
  const SampleIdentifierContainerType *
  GetSampleIdentifiers() const;

  /** Set/Get the type identifiers.
   *  0 - undefined
   *  1 - soma
   *  2 - axon
   *  3 - (basal) dendrite
   *  4 - apical dendrite
   *  5 - custom
   *  6 - unspecified neurite
   *  7 - glia processes
   */
  void
  SetTypeIdentifiers(const TypeIdentifierContainerType *);
  const TypeIdentifierContainerType *
  GetTypeIdentifiers() const;


  /** Set/Get the Radius in micrometers (half the node thickness). */
  void
  SetRadii(const RadiusContainerType *);
  const RadiusContainerType *
  GetRadii() const;

  /** Set/Get the parent sample identifiers. */
  void
  SetParentIdentifiers(const ParentIdentifierContainerType *);
  const ParentIdentifierContainerType *
  GetParentIdentifiers() const;

  /** Set/Get the content of the point data on the input/output itk::Mesh. */
  itkGetConstMacro(PointDataContent, SWCMeshIOEnums::SWCPointData);
  itkSetMacro(PointDataContent, SWCMeshIOEnums::SWCPointData);

protected:
  /** Write points to output stream */
  template <typename T>
  void
  WritePoints(T * buffer)
  {
    SizeValueType index = itk::NumericTraits<SizeValueType>::ZeroValue();
    m_PointsBuffer->resize(this->GetNumberOfPoints());

    for (SizeValueType ii = 0; ii < this->m_NumberOfPoints; ++ii)
    {
      for (unsigned int jj = 0; jj < this->m_PointDimension; ++jj)
      {
        m_PointsBuffer->SetElement(index, static_cast<float>(buffer[index]));
        index++;
      }
    }
  }

  /** Write point data to output stream */
  template <typename T>
  void
  WritePointData(T * buffer)
  {
    switch (m_PointDataContent)
    {
      case SWCMeshIOEnums::SWCPointData::SampleIdentifier:
      {
        m_SampleIdentifiers->resize(this->GetNumberOfPoints());

        for (SizeValueType ii = 0; ii < this->m_NumberOfPoints; ++ii)
        {
          m_SampleIdentifiers->SetElement(ii, static_cast<SampleIdentifierType>(buffer[ii]));
        }
      }
      break;
      case SWCMeshIOEnums::SWCPointData::TypeIdentifier:
      {
        m_TypeIdentifiers->resize(this->GetNumberOfPoints());

        for (SizeValueType ii = 0; ii < this->m_NumberOfPoints; ++ii)
        {
          m_TypeIdentifiers->SetElement(ii, static_cast<TypeIdentifierType>(buffer[ii]));
        }
      }
      break;
      case SWCMeshIOEnums::SWCPointData::Radius:
      {
        m_Radii->resize(this->GetNumberOfPoints());

        for (SizeValueType ii = 0; ii < this->m_NumberOfPoints; ++ii)
        {
          m_Radii->SetElement(ii, static_cast<RadiusType>(buffer[ii]));
        }
      }
      break;
      case SWCMeshIOEnums::SWCPointData::ParentIdentifier:
      {
        m_ParentIdentifiers->resize(this->GetNumberOfPoints());

        for (SizeValueType ii = 0; ii < this->m_NumberOfPoints; ++ii)
        {
          m_ParentIdentifiers->SetElement(ii, static_cast<ParentIdentifierType>(buffer[ii]));
        }
      }
      break;
    }
  }

  template <typename T>
  void
  WriteCells(T * buffer)
  {
    m_ParentIdentifiers->resize(this->GetNumberOfPoints());
    std::fill(m_ParentIdentifiers->begin(), m_ParentIdentifiers->end(), -1);
    SizeValueType index = itk::NumericTraits<SizeValueType>::ZeroValue();

    for (SizeValueType ii = 0; ii < this->m_NumberOfCells; ++ii)
    {
      if (static_cast<uint8_t>(buffer[index]) != static_cast<uint8_t>(CommonEnums::CellGeometry::LINE_CELL))
      {
        itkExceptionMacro("Unexpected cell type -- line cell expected. Found: " << buffer[index]);
      }
      ++index;
      if (static_cast<uint8_t>(buffer[index]) != 2)
      {
        itkExceptionMacro("Unexpected number of cell points -- expected 2. Found: " << buffer[index]);
      }
      ++index;
      const auto parentPoint = static_cast<IdentifierType>(buffer[index]);
      ++index;
      const auto samplePoint = static_cast<IdentifierType>(buffer[index]);
      ++index;

      const auto parentIdentifier = m_PointIndexToSampleIdentifier[parentPoint];
      m_ParentIdentifiers->SetElement(samplePoint, parentIdentifier);
    }
  }

protected:
  SWCMeshIO();
  ~SWCMeshIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using PointsBufferContainerType = VectorContainer<IdentifierType, float>;
  using CellsBufferContainerType = VectorContainer<IdentifierType, uint32_t>;
  using SampleIdentifierToPointIndexType = std::unordered_map<SampleIdentifierType, IdentifierType>;
  using PointIndexToParentPointIndexType = std::unordered_map<IdentifierType, IdentifierType>;
  using PointIndexToSampleIdentifierType = std::unordered_map<IdentifierType, SampleIdentifierType>;

private:
  HeaderContentType                      m_HeaderContent;
  SampleIdentifierContainerType::Pointer m_SampleIdentifiers;
  TypeIdentifierContainerType::Pointer   m_TypeIdentifiers;
  RadiusContainerType::Pointer           m_Radii;
  ParentIdentifierContainerType::Pointer m_ParentIdentifiers;
  PointsBufferContainerType::Pointer     m_PointsBuffer;
  CellsBufferContainerType::Pointer      m_CellsBuffer;
  SampleIdentifierToPointIndexType       m_SampleIdentifierToPointIndex;
  PointIndexToParentPointIndexType       m_PointIndexToParentPointIndex;
  PointIndexToSampleIdentifierType       m_PointIndexToSampleIdentifier;

  SWCMeshIOEnums::SWCPointData m_PointDataContent{ SWCMeshIOEnums::SWCPointData::TypeIdentifier };
};
} // end namespace itk

#endif
