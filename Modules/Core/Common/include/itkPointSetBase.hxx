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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkPointSetBase_hxx
#define itkPointSetBase_hxx

#include "itkProcessObject.h"
#include <algorithm>

namespace itk
{
template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number Of Points: " << this->GetNumberOfPoints() << std::endl;

  os << indent << "Requested Number Of Regions: " << m_RequestedNumberOfRegions << std::endl;
  os << indent << "Requested Region: " << m_RequestedRegion << std::endl;
  os << indent << "Buffered Region: " << m_BufferedRegion << std::endl;
  os << indent << "Maximum Number Of Regions: " << m_MaximumNumberOfRegions << std::endl;
}

template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::SetPoints(PointsContainer * points)
{
  itkDebugMacro("setting Points container to " << points);
  if (m_PointsContainer != points)
  {
    m_PointsContainer = points;
    this->Modified();
  }
}

template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::SetPoints(PointsVectorContainer * points)
{

  itkDebugMacro("setting Points container to " << points);
  if (points->Size() % PointDimension != 0)
  {
    itkExceptionMacro("Number of entries in given 1d array incompatible with the point dimension");
  }

  // Note: this cast is unsafe. It may lead to undefined behavior.
  auto * pointsPtr = reinterpret_cast<PointsContainer *>(points);

  m_PointsContainer = pointsPtr;
  this->Modified();
}


template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::SetPointsByCoordinates(const std::vector<CoordinateType> & coordinates)
{
  itkDebugMacro("Setting the points to the specified coordinates");

  const size_t numberOfCoordinates = coordinates.size();

  if (numberOfCoordinates % PointDimension != 0)
  {
    itkExceptionMacro("Number of specified coordinates incompatible with the point dimension");
  }

  const size_t numberOfPoints = numberOfCoordinates / PointDimension;

  if (m_PointsContainer == nullptr)
  {
    m_PointsContainer = PointsContainer::New();
  }

  using STLContainerType = typename PointsContainer::STLContainerType;

  STLContainerType & points = m_PointsContainer->CastToSTLContainer();
  points.clear();

  if constexpr (std::is_same_v<STLContainerType, std::vector<PointType>>)
  {
    // STLContainerType is either an std::vector or an std::map. Only when it is an std::vector, it should be resized.
    // std::map does not have a resize function.
    points.resize(numberOfPoints);
  }
  else
  {
    static_assert(std::is_same_v<STLContainerType, std::map<PointIdentifier, PointType>>);
  }

  auto coordinateIterator = coordinates.cbegin();

  for (PointIdentifier pointIdentifier{}; pointIdentifier < numberOfPoints; ++pointIdentifier)
  {
    PointType & point = points[pointIdentifier];
    std::copy_n(coordinateIterator, PointDimension, point.begin());
    coordinateIterator += PointDimension;
  }

  this->Modified();
}


template <typename TPointsContainer>
auto
PointSetBase<TPointsContainer>::GetPoints() -> PointsContainer *
{
  itkDebugMacro("Starting GetPoints()");
  if (!m_PointsContainer)
  {
    this->SetPoints(PointsContainer::New());
  }
  itkDebugMacro("returning Points container of " << m_PointsContainer);
  return m_PointsContainer;
}

template <typename TPointsContainer>
auto
PointSetBase<TPointsContainer>::GetPoints() const -> const PointsContainer *
{
  itkDebugMacro("returning Points container of " << m_PointsContainer);
  return m_PointsContainer.GetPointer();
}

template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::SetPoint(PointIdentifier ptId, PointType point)
{
  /**
   * Make sure a points container exists.
   */
  if (!m_PointsContainer)
  {
    this->SetPoints(PointsContainer::New());
  }

  /**
   * Insert the point into the container with the given identifier.
   */
  m_PointsContainer->InsertElement(ptId, point);
}

template <typename TPointsContainer>
bool
PointSetBase<TPointsContainer>::GetPoint(PointIdentifier ptId, PointType * point) const
{
  /**
   * If the points container doesn't exist, then the point doesn't either.
   */
  if (!m_PointsContainer)
  {
    return false;
  }

  /**
   * Ask the container if the point identifier exists.
   */
  return m_PointsContainer->GetElementIfIndexExists(ptId, point);
}

template <typename TPointsContainer>
auto
PointSetBase<TPointsContainer>::GetPoint(PointIdentifier ptId) const -> PointType
{
  /**
   * If the points container doesn't exist, then the point doesn't either.
   */
  if (!m_PointsContainer)
  {
    itkExceptionMacro("Point container doesn't exist.");
  }

  /**
   * Ask the container if the point identifier exists.
   */
  PointType  point;
  const bool exist = m_PointsContainer->GetElementIfIndexExists(ptId, &point);
  if (!exist)
  {
    itkExceptionMacro("Point id doesn't exist: " << ptId);
  }
  return point;
}


template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::PassStructure(Self *)
{
  // IMPLEMENT ME
}

template <typename TPointsContainer>
auto
PointSetBase<TPointsContainer>::GetNumberOfPoints() const -> PointIdentifier
{
  if (m_PointsContainer)
  {
    return m_PointsContainer->Size();
  }
  return 0;
}

template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::Initialize()
{
  Superclass::Initialize();

  m_PointsContainer = nullptr;
}


template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::UpdateOutputInformation()
{
  this->Superclass::UpdateOutputInformation();

  // Now we should know what our largest possible region is. If our
  // requested region was not set yet, (or has been set to something
  // invalid - with no data in it ) then set it to the largest
  // possible region.
  if (m_RequestedRegion == -1 && m_RequestedNumberOfRegions == 0)
  {
    this->SetRequestedRegionToLargestPossibleRegion();
  }
}

template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::SetRequestedRegionToLargestPossibleRegion()
{
  m_RequestedNumberOfRegions = 1;
  m_RequestedRegion = 0;
}

template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::CopyInformation(const DataObject * data)
{
  const auto * pointSet = dynamic_cast<const PointSetBase *>(data);

  if (!pointSet)
  {
    // pointer could not be cast back down
    itkExceptionMacro("itk::PointSetBase::CopyInformation() cannot cast " << typeid(data).name() << " to "
                                                                          << typeid(PointSetBase *).name());
  }

  m_MaximumNumberOfRegions = pointSet->GetMaximumNumberOfRegions();

  m_NumberOfRegions = pointSet->m_NumberOfRegions;
  m_RequestedNumberOfRegions = pointSet->m_RequestedNumberOfRegions;
  m_BufferedRegion = pointSet->m_BufferedRegion;
  m_RequestedRegion = pointSet->m_RequestedRegion;
}

template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::SetRequestedRegion(const DataObject * data)
{
  const auto * pointSet = dynamic_cast<const Self *>(data);

  if (pointSet)
  {
    // only copy the RequestedRegion if the parameter is another PointSetBase
    m_RequestedRegion = pointSet->m_RequestedRegion;
    m_RequestedNumberOfRegions = pointSet->m_RequestedNumberOfRegions;
  }
}

template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::SetRequestedRegion(const RegionType & region)
{
  if (m_RequestedRegion != region)
  {
    m_RequestedRegion = region;
  }
}

template <typename TPointsContainer>
void
PointSetBase<TPointsContainer>::SetBufferedRegion(const RegionType & region)
{
  if (m_BufferedRegion != region)
  {
    m_BufferedRegion = region;
    this->Modified();
  }
}

template <typename TPointsContainer>
bool
PointSetBase<TPointsContainer>::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  if (m_RequestedRegion != m_BufferedRegion || m_RequestedNumberOfRegions != m_NumberOfRegions)
  {
    return true;
  }

  return false;
}

template <typename TPointsContainer>
bool
PointSetBase<TPointsContainer>::VerifyRequestedRegion()
{
  const bool retval = true;

  // Are we asking for more regions than we can get?
  if (m_RequestedNumberOfRegions > m_MaximumNumberOfRegions)
  {
    itkExceptionMacro("Cannot break object into " << m_RequestedNumberOfRegions << ". The limit is "
                                                  << m_MaximumNumberOfRegions);
  }

  if (m_RequestedRegion >= m_RequestedNumberOfRegions || m_RequestedRegion < 0)
  {
    itkExceptionMacro("Invalid update region " << m_RequestedRegion << ". Must be between 0 and "
                                               << m_RequestedNumberOfRegions - 1);
  }

  return retval;
}

template <typename TPointsContainer>
LightObject::Pointer
PointSetBase<TPointsContainer>::InternalClone() const
{
  LightObject::Pointer lightObject = Superclass::InternalClone();

  if (auto * const clone = dynamic_cast<Self *>(lightObject.GetPointer()))
  {
    if (m_PointsContainer)
    {
      clone->m_PointsContainer = TPointsContainer::New();
      clone->m_PointsContainer->CastToSTLContainer() = m_PointsContainer->CastToSTLConstContainer();
    }

    clone->m_MaximumNumberOfRegions = m_MaximumNumberOfRegions;
    clone->m_NumberOfRegions = m_NumberOfRegions;
    clone->m_RequestedNumberOfRegions = m_RequestedNumberOfRegions;
    clone->m_BufferedRegion = m_BufferedRegion;
    clone->m_RequestedRegion = m_RequestedRegion;

    return lightObject;
  }
  itkExceptionMacro("downcast to type " << this->GetNameOfClass() << " failed.");
}

// Destructor. Must be defined here (rather than inside the class definition) because it is pure virtual.
template <typename TPointsContainer>
PointSetBase<TPointsContainer>::~PointSetBase() = default;

} // end namespace itk

#endif
