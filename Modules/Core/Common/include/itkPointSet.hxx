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
#ifndef itkPointSet_hxx
#define itkPointSet_hxx

#include "itkProcessObject.h"
#include <algorithm>

namespace itk
{
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number Of Points: " << this->GetNumberOfPoints() << std::endl;

  os << indent << "Requested Number Of Regions: " << m_RequestedNumberOfRegions << std::endl;
  os << indent << "Requested Region: " << m_RequestedRegion << std::endl;
  os << indent << "Buffered Region: " << m_BufferedRegion << std::endl;
  os << indent << "Maximum Number Of Regions: " << m_MaximumNumberOfRegions << std::endl;
  os << indent << "Point Data Container pointer: "
     << ((this->m_PointDataContainer) ? this->m_PointDataContainer.GetPointer() : nullptr) << std::endl;
  os << indent
     << "Size of Point Data Container: " << ((this->m_PointDataContainer) ? this->m_PointDataContainer->Size() : 0)
     << std::endl;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::SetPoints(PointsContainer * points)
{
  itkDebugMacro("setting Points container to " << points);
  if (m_PointsContainer != points)
  {
    m_PointsContainer = points;
    this->Modified();
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::SetPoints(PointsVectorContainer * points)
{

  itkDebugMacro("setting Points container to " << points);
  if (points->Size() % PointDimension != 0)
  {
    itkExceptionMacro("Number of entries in given 1d array incompatible with the point dimension");
  }
  auto * pointsPtr = reinterpret_cast<PointsContainer *>(points);

  m_PointsContainer = pointsPtr;
  this->Modified();
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
PointSet<TPixelType, VDimension, TMeshTraits>::GetPoints() -> PointsContainer *
{
  itkDebugMacro("Starting GetPoints()");
  if (!m_PointsContainer)
  {
    this->SetPoints(PointsContainer::New());
  }
  itkDebugMacro("returning Points container of " << m_PointsContainer);
  return m_PointsContainer;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
PointSet<TPixelType, VDimension, TMeshTraits>::GetPoints() const -> const PointsContainer *
{
  itkDebugMacro("returning Points container of " << m_PointsContainer);
  return m_PointsContainer.GetPointer();
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::SetPointData(PointDataContainer * pointData)
{
  itkDebugMacro("setting PointData container to " << pointData);
  if (m_PointDataContainer != pointData)
  {
    m_PointDataContainer = pointData;
    this->Modified();
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
PointSet<TPixelType, VDimension, TMeshTraits>::GetPointData() -> PointDataContainer *
{
  if (!m_PointDataContainer)
  {
    this->SetPointData(PointDataContainer::New());
  }
  itkDebugMacro("returning PointData container of " << m_PointDataContainer);
  return m_PointDataContainer;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
PointSet<TPixelType, VDimension, TMeshTraits>::GetPointData() const -> const PointDataContainer *
{
  itkDebugMacro("returning PointData container of " << m_PointDataContainer);
  return m_PointDataContainer.GetPointer();
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::SetPoint(PointIdentifier ptId, PointType point)
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

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
PointSet<TPixelType, VDimension, TMeshTraits>::GetPoint(PointIdentifier ptId, PointType * point) const
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

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
PointSet<TPixelType, VDimension, TMeshTraits>::GetPoint(PointIdentifier ptId) const -> PointType
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
  PointType point;
  bool      exist = m_PointsContainer->GetElementIfIndexExists(ptId, &point);
  if (!exist)
  {
    itkExceptionMacro("Point id doesn't exist: " << ptId);
  }
  return point;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::SetPointData(PointIdentifier ptId, PixelType data)
{
  /**
   * Make sure a point data container exists.
   */
  if (!m_PointDataContainer)
  {
    this->SetPointData(PointDataContainer::New());
  }

  /**
   * Insert the point data into the container with the given identifier.
   */
  m_PointDataContainer->InsertElement(ptId, data);
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
PointSet<TPixelType, VDimension, TMeshTraits>::GetPointData(PointIdentifier ptId, PixelType * data) const
{
  /**
   * If the point data container doesn't exist, then the point data doesn't
   * either.
   */
  if (!m_PointDataContainer)
  {
    return false;
  }

  /**
   * Ask the container if the point identifier exists.
   */
  return m_PointDataContainer->GetElementIfIndexExists(ptId, data);
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::PassStructure(Self *)
{
  // IMPLEMENT ME
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
auto
PointSet<TPixelType, VDimension, TMeshTraits>::GetNumberOfPoints() const -> PointIdentifier
{
  if (m_PointsContainer)
  {
    return m_PointsContainer->Size();
  }
  return 0;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::Initialize()
{
  Superclass::Initialize();

  m_PointsContainer = nullptr;
  m_PointDataContainer = nullptr;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
PointSet<TPixelType, VDimension, TMeshTraits>::PointSet()
  : m_PointsContainer(nullptr)
  , m_PointDataContainer(nullptr)
{

  // If we used unstructured regions instead of structured regions, then
  // assume this object was created by the user and this is region 0 of
  // 1 region.
  m_MaximumNumberOfRegions = 1;
  m_NumberOfRegions = 1;
  m_BufferedRegion = -1;
  m_RequestedNumberOfRegions = 0;
  m_RequestedRegion = -1;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::UpdateOutputInformation()
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

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::SetRequestedRegionToLargestPossibleRegion()
{
  m_RequestedNumberOfRegions = 1;
  m_RequestedRegion = 0;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::CopyInformation(const DataObject * data)
{
  const auto * pointSet = dynamic_cast<const PointSet *>(data);

  if (!pointSet)
  {
    // pointer could not be cast back down
    itkExceptionMacro(<< "itk::PointSet::CopyInformation() cannot cast " << typeid(data).name() << " to "
                      << typeid(PointSet *).name());
  }

  m_MaximumNumberOfRegions = pointSet->GetMaximumNumberOfRegions();

  m_NumberOfRegions = pointSet->m_NumberOfRegions;
  m_RequestedNumberOfRegions = pointSet->m_RequestedNumberOfRegions;
  m_BufferedRegion = pointSet->m_BufferedRegion;
  m_RequestedRegion = pointSet->m_RequestedRegion;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::Graft(const DataObject * data)
{
  // Copy Meta Data
  this->CopyInformation(data);

  const auto * pointSet = dynamic_cast<const Self *>(data);

  if (!pointSet)
  {
    // pointer could not be cast back down
    itkExceptionMacro(<< "itk::PointSet::CopyInformation() cannot cast " << typeid(data).name() << " to "
                      << typeid(Self *).name());
  }

  this->SetPoints(pointSet->m_PointsContainer);
  this->SetPointData(pointSet->m_PointDataContainer);
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::SetRequestedRegion(const DataObject * data)
{
  const auto * pointSet = dynamic_cast<const Self *>(data);

  if (pointSet)
  {
    // only copy the RequestedRegion if the parameter is another PointSet
    m_RequestedRegion = pointSet->m_RequestedRegion;
    m_RequestedNumberOfRegions = pointSet->m_RequestedNumberOfRegions;
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::SetRequestedRegion(const RegionType & region)
{
  if (m_RequestedRegion != region)
  {
    m_RequestedRegion = region;
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>::SetBufferedRegion(const RegionType & region)
{
  if (m_BufferedRegion != region)
  {
    m_BufferedRegion = region;
    this->Modified();
  }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
PointSet<TPixelType, VDimension, TMeshTraits>::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  if (m_RequestedRegion != m_BufferedRegion || m_RequestedNumberOfRegions != m_NumberOfRegions)
  {
    return true;
  }

  return false;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
PointSet<TPixelType, VDimension, TMeshTraits>::VerifyRequestedRegion()
{
  bool retval = true;

  // Are we asking for more regions than we can get?
  if (m_RequestedNumberOfRegions > m_MaximumNumberOfRegions)
  {
    itkExceptionMacro(<< "Cannot break object into " << m_RequestedNumberOfRegions << ". The limit is "
                      << m_MaximumNumberOfRegions);
  }

  if (m_RequestedRegion >= m_RequestedNumberOfRegions || m_RequestedRegion < 0)
  {
    itkExceptionMacro(<< "Invalid update region " << m_RequestedRegion << ". Must be between 0 and "
                      << m_RequestedNumberOfRegions - 1);
  }

  return retval;
}
} // end namespace itk

#endif
