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
  os << indent << "Point Data Container pointer: "
     << ((this->m_PointDataContainer) ? this->m_PointDataContainer.GetPointer() : nullptr) << std::endl;
  os << indent
     << "Size of Point Data Container: " << ((this->m_PointDataContainer) ? this->m_PointDataContainer->Size() : 0)
     << std::endl;
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
PointSet<TPixelType, VDimension, TMeshTraits>::Initialize()
{
  Superclass::Initialize();

  m_PointDataContainer = nullptr;
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
    itkExceptionMacro("itk::PointSet::Graft() cannot cast " << typeid(data).name() << " to " << typeid(Self *).name());
  }

  this->SetPoints(pointSet->Superclass::m_PointsContainer);
  this->SetPointData(pointSet->m_PointDataContainer);
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
LightObject::Pointer
PointSet<TPixelType, VDimension, TMeshTraits>::InternalClone() const
{
  LightObject::Pointer lightObject = Superclass::InternalClone();

  if (auto * const clone = dynamic_cast<Self *>(lightObject.GetPointer()))
  {
    if (m_PointDataContainer)
    {
      clone->m_PointDataContainer = PointDataContainer::New();
      clone->m_PointDataContainer->CastToSTLContainer() = m_PointDataContainer->CastToSTLConstContainer();
    }
    return lightObject;
  }
  itkExceptionMacro("downcast to type " << this->GetNameOfClass() << " failed.");
}
} // end namespace itk

#endif
