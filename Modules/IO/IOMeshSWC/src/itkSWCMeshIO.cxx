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

#include "itkSWCMeshIO.h"

#include "itksys/SystemTools.hxx"

namespace itk
{

std::ostream &
operator<<(std::ostream & out, const SWCMeshIOEnums::SWCPointData value)
{
  return out << [value] {
    switch (value)
    {
      case SWCMeshIOEnums::SWCPointData::SampleIdentifier:
        return "SWCMeshIOEnums::SWCPointData::SampleIdentifier";
      case SWCMeshIOEnums::SWCPointData::TypeIdentifier:
        return "SWCMeshIOEnums::SWCPointData::TypeIdentifier";
      case SWCMeshIOEnums::SWCPointData::Radius:
        return "SWCMeshIOEnums::SWCPointData::Radius";
      case SWCMeshIOEnums::SWCPointData::ParentIdentifier:
        return "SWCMeshIOEnums::SWCPointData::ParentIdentifier";
      default:
        return "INVALID VALUE FOR SWCMeshIOEnums";
    }
  }();
}

SWCMeshIO ::SWCMeshIO()
{
  this->AddSupportedWriteExtension(".swc");

  m_SampleIdentifiers = SampleIdentifierContainerType::New();
  m_TypeIdentifiers = TypeIdentifierContainerType::New();
  m_Radii = RadiusContainerType::New();
  m_ParentIdentifiers = ParentIdentifierContainerType::New();
  m_PointsBuffer = PointsBufferContainerType::New();
  m_CellsBuffer = CellsBufferContainerType::New();

  this->m_PointDimension = 3;
  this->m_FileType = IOFileEnum::ASCII;
}

SWCMeshIO::~SWCMeshIO() = default;

bool
SWCMeshIO ::CanReadFile(const char * fileName)
{
  if (!itksys::SystemTools::FileExists(fileName, true))
  {
    return false;
  }

  if (itksys::SystemTools::GetFilenameLastExtension(fileName) != ".swc")
  {
    return false;
  }

  return true;
}

bool
SWCMeshIO ::CanWriteFile(const char * fileName)
{
  if (itksys::SystemTools::GetFilenameLastExtension(fileName) != ".swc")
  {
    return false;
  }

  return true;
}

void
SWCMeshIO ::ReadMeshInformation()
{
  // Define input file stream and attach it to input file
  std::ifstream inputFile;
  inputFile.open(this->m_FileName.c_str(), std::ios::in);
  if (!inputFile.is_open())
  {
    itkExceptionMacro(<< "Unable to open input file " << this->m_FileName);
  }

  std::string line;

  m_HeaderContent.clear();
  bool inHeader = true;
  while (inHeader && !inputFile.eof())
  {
    std::getline(inputFile, line);

    const size_t first = line.find_first_of('#');
    if (first == std::string::npos)
    {
      inHeader = false;
    }
    else
    {
      m_HeaderContent.push_back(line.substr(first + 1));
    }
  }

  std::istringstream istrm;
  SizeValueType      numberOfPoints = 0;
  SizeValueType      numberOfCells = 0;
  m_SampleIdentifiers->clear();
  m_TypeIdentifiers->clear();
  m_Radii->clear();
  m_ParentIdentifiers->clear();
  m_PointsBuffer->clear();
  m_CellsBuffer->clear();
  this->m_CellBufferSize = 0;
  m_SampleIdentifierToPointIndex.clear();
  while (!inputFile.eof())
  {
    istrm.str(line);

    SampleIdentifierType sampleIdentifier;
    istrm >> sampleIdentifier;
    m_SampleIdentifiers->push_back(sampleIdentifier);
    m_SampleIdentifierToPointIndex[sampleIdentifier] = numberOfPoints;

    TypeIdentifierType typeIdentifier;
    istrm >> typeIdentifier;
    m_TypeIdentifiers->push_back(typeIdentifier);

    double pointComponent;
    istrm >> pointComponent;
    m_PointsBuffer->push_back(pointComponent);
    istrm >> pointComponent;
    m_PointsBuffer->push_back(pointComponent);
    istrm >> pointComponent;
    m_PointsBuffer->push_back(pointComponent);

    double radius;
    istrm >> radius;
    m_Radii->push_back(radius);

    ParentIdentifierType parentIdentifier;
    istrm >> parentIdentifier;
    m_ParentIdentifiers->push_back(parentIdentifier);
    if (parentIdentifier != -1)
    {
      ++numberOfCells;
      this->m_CellBufferSize += 4;
    }

    std::getline(inputFile, line);

    ++numberOfPoints;
  }
  this->SetNumberOfPoints(numberOfPoints);
  this->SetNumberOfCells(numberOfCells);
  this->SetNumberOfPointPixels(numberOfPoints);

  // If number of points is not equal zero, update points
  if (this->m_NumberOfPoints)
  {
    this->m_UpdatePoints = true;
    this->m_UpdatePointData = true;
  }

  if (this->m_NumberOfCells)
  {
    this->m_UpdateCells = true;
  }

  // Set default point component type
  this->m_PointComponentType = IOComponentEnum::DOUBLE;
  this->m_CellComponentType = IOComponentEnum::UINT;

  this->m_PointPixelType = IOPixelEnum::SCALAR;
  this->m_NumberOfPointPixelComponents = 1;
  switch (m_PointDataContent)
  {
    case SWCMeshIOEnums::SWCPointData::SampleIdentifier:
      this->m_PointPixelComponentType = IOComponentEnum::SHORT;
      break;
    case SWCMeshIOEnums::SWCPointData::TypeIdentifier:
      this->m_PointPixelComponentType = IOComponentEnum::UCHAR;
    case SWCMeshIOEnums::SWCPointData::Radius:
      this->m_PointPixelComponentType = IOComponentEnum::DOUBLE;
      break;
    case SWCMeshIOEnums::SWCPointData::ParentIdentifier:
      this->m_PointPixelComponentType = IOComponentEnum::SHORT;
      break;
  }
  this->m_CellPixelType = IOPixelEnum::SCALAR;
  this->m_NumberOfCellPixelComponents = 1;

  inputFile.close();
}

void
SWCMeshIO ::ReadPoints(void * buffer)
{
  auto *              data = static_cast<double *>(buffer);
  const SizeValueType numberOfValues = this->m_PointDimension * this->GetNumberOfPoints();
  for (SizeValueType ii = 0; ii < numberOfValues; ++ii)
  {
    data[ii] = this->m_PointsBuffer->GetElement(ii);
  }
}

void
SWCMeshIO ::ReadCells(void * buffer)
{
  auto * data = static_cast<unsigned int *>(buffer);

  // Skip root
  const SizeValueType numberOfPoints = this->GetNumberOfPoints();
  SizeValueType       cellBufferIndex = 0;
  for (SizeValueType pointIndex = 1; pointIndex < this->GetNumberOfPoints(); ++pointIndex)
  {
    const auto sampleIdentifier = m_SampleIdentifiers->GetElement(pointIndex);
    const auto parentIdentifier = m_ParentIdentifiers->GetElement(pointIndex);
    if (parentIdentifier != -1)
    {
      data[cellBufferIndex++] = static_cast<uint32_t>(CommonEnums::CellGeometry::LINE_CELL);
      data[cellBufferIndex++] = 2;
      data[cellBufferIndex++] = m_SampleIdentifierToPointIndex[parentIdentifier];
      data[cellBufferIndex++] = m_SampleIdentifierToPointIndex[sampleIdentifier];
    }
  }
}

void
SWCMeshIO ::ReadPointData(void * itkNotUsed(buffer))
{}

void
SWCMeshIO ::ReadCellData(void * itkNotUsed(buffer))
{}

void
SWCMeshIO ::WriteMeshInformation()
{
  // Check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str(), std::ios::out);

  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }

  // Write SWC file header
  // Indent indent(7);
  // outputFile << indent << 1;
  // outputFile << indent << this->m_NumberOfPoints;
  // outputFile << indent << this->m_NumberOfCells;
  // outputFile << indent << this->m_CellBufferSize - 2 * this->m_NumberOfCells << std::endl;
  // outputFile << indent << 1;
  // outputF ile << indent << this->m_NumberOfCells << std::endl;

  outputFile.close();
}

void
SWCMeshIO ::WritePoints(void * buffer)
{
  // check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str(), std::ios_base::app);

  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }

  // Write points
  switch (this->m_PointComponentType)
  {
    case IOComponentEnum::UCHAR:
    {
      WritePoints(static_cast<unsigned char *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::CHAR:
    {
      WritePoints(static_cast<char *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::USHORT:
    {
      WritePoints(static_cast<unsigned short *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::SHORT:
    {
      WritePoints(static_cast<short *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::UINT:
    {
      WritePoints(static_cast<unsigned int *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::INT:
    {
      WritePoints(static_cast<int *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::ULONG:
    {
      WritePoints(static_cast<unsigned long *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::LONG:
    {
      WritePoints(static_cast<long *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::ULONGLONG:
    {
      WritePoints(static_cast<unsigned long long *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::LONGLONG:
    {
      WritePoints(static_cast<long long *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::FLOAT:
    {
      WritePoints(static_cast<float *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::DOUBLE:
    {
      WritePoints(static_cast<double *>(buffer), outputFile);

      break;
    }
    case IOComponentEnum::LDOUBLE:
    {
      WritePoints(static_cast<long double *>(buffer), outputFile);

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
SWCMeshIO ::WriteCells(void * buffer)
{
  // Check file name
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("No Input FileName");
  }

  // Write to output file
  std::ofstream outputFile(this->m_FileName.c_str(), std::ios_base::app);

  if (!outputFile.is_open())
  {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= "
                      << this->m_FileName);
  }

  // Write polygons
  switch (this->m_CellComponentType)
  {
    case IOComponentEnum::UCHAR:
    {
      WriteCells(static_cast<unsigned char *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::CHAR:
    {
      WriteCells(static_cast<unsigned char *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::USHORT:
    {
      WriteCells(static_cast<unsigned short *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::SHORT:
    {
      WriteCells(static_cast<short *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::UINT:
    {
      WriteCells(static_cast<unsigned int *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::INT:
    {
      WriteCells(static_cast<int *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::ULONG:
    {
      WriteCells(static_cast<unsigned long *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::LONG:
    {
      WriteCells(static_cast<long *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::ULONGLONG:
    {
      WriteCells(static_cast<unsigned long long *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::LONGLONG:
    {
      WriteCells(static_cast<long long *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::FLOAT:
    {
      WriteCells(static_cast<float *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::DOUBLE:
    {
      WriteCells(static_cast<double *>(buffer), outputFile);
      break;
    }
    case IOComponentEnum::LDOUBLE:
    {
      WriteCells(static_cast<long double *>(buffer), outputFile);
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
SWCMeshIO ::WritePointData(void * itkNotUsed(buffer))
{}

void
SWCMeshIO ::WriteCellData(void * itkNotUsed(buffer))
{}

void
SWCMeshIO ::Write()
{}

void
SWCMeshIO ::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FilePosition: " << static_cast<typename NumericTraits<StreamOffsetType>::PrintType>(m_FilePosition)
     << std::endl;
  os << indent << "PartId: " << m_PartId << std::endl;
  os << indent << "First Cell Id: " << m_FirstCellId << std::endl;
  os << indent << "Last Cell Id: " << m_LastCellId << std::endl;
}

void
SWCMeshIO ::SetSampleIdentifiers(const SampleIdentifierContainerType * sampleIdentifiers)
{
  const SizeValueType size = sampleIdentifiers->Size();
  m_SampleIdentifiers->resize(size);
  for (SizeValueType ii = 0; ii < size; ++ii)
  {
    m_SampleIdentifiers->SetElement(ii, sampleIdentifiers->GetElement(ii));
  }
  this->Modified();
}

auto
SWCMeshIO ::GetSampleIdentifiers() const -> const SampleIdentifierContainerType *
{
  return m_SampleIdentifiers;
}

void
SWCMeshIO ::SetTypeIdentifiers(const TypeIdentifierContainerType * typeIdentifiers)
{
  const SizeValueType size = typeIdentifiers->Size();
  m_TypeIdentifiers->resize(size);
  for (SizeValueType ii = 0; ii < size; ++ii)
  {
    m_TypeIdentifiers->SetElement(ii, typeIdentifiers->GetElement(ii));
  }
  this->Modified();
}

auto
SWCMeshIO ::GetTypeIdentifiers() const -> const TypeIdentifierContainerType *
{
  return m_TypeIdentifiers;
}

void
SWCMeshIO ::SetRadii(const RadiusContainerType * radii)
{
  const SizeValueType size = radii->Size();
  m_Radii->resize(size);
  for (SizeValueType ii = 0; ii < size; ++ii)
  {
    m_Radii->SetElement(ii, radii->GetElement(ii));
  }
  this->Modified();
}

auto
SWCMeshIO ::GetRadii() const -> const RadiusContainerType *
{
  return m_Radii;
}

void
SWCMeshIO ::SetParentIdentifiers(const ParentIdentifierContainerType * parentIdentifiers)
{
  const SizeValueType size = parentIdentifiers->Size();
  m_ParentIdentifiers->resize(size);
  for (SizeValueType ii = 0; ii < size; ++ii)
  {
    m_ParentIdentifiers->SetElement(ii, parentIdentifiers->GetElement(ii));
  }
  this->Modified();
}

auto
SWCMeshIO ::GetParentIdentifiers() const -> const ParentIdentifierContainerType *
{
  return m_ParentIdentifiers;
}

void
SWCMeshIO ::SetHeaderContent(const HeaderContentType & headerContent)
{
  m_HeaderContent.resize(headerContent.size());
  for (size_t ii = 0; ii < headerContent.size(); ++ii)
  {
    m_HeaderContent[ii] = headerContent[ii];
  }
}

} // namespace itk
